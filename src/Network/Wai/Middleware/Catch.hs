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

import Prelude hiding (catch, concat)

import Data.ByteString (concat)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (pack)

import Control.Exception (Exception(..), SomeException)
import Control.Exception.Lifted (Handler(..), catches)
import Network.Wai (Application, Middleware, responseLBS, Request(..))
import Network.HTTP.Types (status500)

-- | Handler wrapper. For polymorphic exceptions.
data ResponseHandler = forall e . Exception e => 
    ResponseHandler (e -> Application)

-- | Protect 'Middleware' chain from exceptions. This acts like
--   'catches', but uses own handler type for simplicity.
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
    return $ responseLBS status500 [] $ pack $ 
            show e ++ " : " ++ dumpRequest req)
  where
    dumpRequest req = unpack $ concat [requestMethod req, " ", 
            rawPathInfo req, rawQueryString req]


