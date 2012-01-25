{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Exception handling for 'Wai' and 'Warp'.
--
--   By default 'Warp' not handles exceptions well. It just log them to 
--   console. This package - an attempt to solve the problem.
--
--   The only drawback stems from the basic advantages of Haskell - laziness. 
--   All errors within 'Wai' 'ResponseBuilder' will not be caught. Thus, the 
--   following code will not work:
-- 
-- > ... return $ responseLBS undefined ...
-- 
--   To ensure catch all errors, you need to consume all data /before/ feeding 
--   the builder. 

module Network.Wai.Middleware.Catch where

import Prelude hiding (catch)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8

import Control.Exception (Exception(..), SomeException)
import Control.Exception.Lifted (Handler(..), catches)
import Network.Wai 
import Network.HTTP.Types

-- | Handler wrapper. 
data ResponseHandler = forall e . Exception e => 
    ResponseHandler (e -> Application)

protect :: [ResponseHandler]  -- ^ Wrapped handlers. See 'mkHandler'.
    -> Middleware
protect handlers app req = 
    catches (app req) (wrapHandlers handlers)
  where
    wrapHandlers = fmap (\(ResponseHandler f) -> Handler (`f` req))

-- | Helper for make 'RequestHandler'
--
-- > protect [mkHandler myHandler] $ ...
mkHandler :: forall e . Exception e => 
       (e -> Application)   -- ^   
    -> ResponseHandler 
mkHandler = ResponseHandler

-- | Default handler. 
defHandler :: ResponseHandler    
defHandler = mkHandler (\(e::SomeException) req -> 
    return $ responseLBS status500 [] $ BL8.pack $ 
            show e ++ " : " ++ dumpRequest req)
  where
    dumpRequest req = B8.unpack $ B.concat [requestMethod req, " ", 
            rawPathInfo req, rawQueryString req]


