{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables #-}

-- | Exception handling for 'Wai' and 'Warp'.
--
--   By default 'Warp' not handles exceptions well. It just log them to 
--   console. This package - an attempt to solve the problem.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > {-# LANGUAGE ScopedTypeVariables #-} 
-- >
-- > import Data.Typeable (Typeable)
-- > import Control.Exception (Exception, throw)
-- > import Network.HTTP.Types
-- > import Network.Wai.Handler.Warp (run)
-- > import Data.ByteString.Lazy.Char8 (pack) -- Just for an orphan instance
-- >
-- > import Network.Wai
-- > import Network.Wai.Middleware.Catch
-- >
-- > data MyException = MyException String deriving (Show, Typeable)
-- > instance Exception MyException
-- >
-- > main :: IO ()
-- > main = do
-- >     putStrLn $ "http://localhost:8888/"
-- >     run 8888 $ (protect handler) $ app
-- >     -- ... try 'protect'' and you see what /err/ request will 
-- >     -- be handled with code 500
-- >
-- > app :: Application
-- > app req = case rawPathInfo req of
-- >     "/ok" -> return $ responseLBS status200 
-- >                       [("Content-Type", "text/plain")] "OK"
-- >     "/err" -> error "Error"
-- >     "/exc" -> throw $ MyException "Raised exception"
-- >     _ -> return $ responseLBS status200 [("Content-Type", "text/plain")] 
-- >             "Try any of /ok, /exc, /err"
-- >
-- > -- Our handler
-- > handler (e :: MyException) _ = return $ 
-- >    responseLBS status200 [("Content-Type", "text/plain")] (pack $ show e)
--
--   The only drawback stems from the basic advantages of Haskell - laziness. 
--   All errors within 'Wai' 'ResponseBuilder' will not be caught. Thus, the 
--   following code will not work:
-- 
-- > ... return $ responseLBS undefined ...
-- 
--   To ensure catch all errors, you need to consume all data /before/ feeding 
--   the builder. 

module Network.Wai.Middleware.Catch (
    protect,
    protect'
) where

import Prelude hiding (catch)

-- control
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception (SomeException)
import qualified Control.Exception.Lifted as E

-- enumerator
import Data.Enumerator (Iteratee (..), Step (..))

-- bytestrings
import qualified Data.ByteString.Lazy.Char8 as BL (pack)

-- networking
import Network.HTTP.Types (status500)
import Network.Wai

-- | Handles exceptions in responses. If exception isn't handled - it will be 
--   rethrown further. To ensure handle all errors use 'protect''.
protect :: (E.Exception e) =>
       (e -> Application)   -- ^ Handler
    -> Middleware
protect handler app req = app req `catch` (`handler`req)

-- | Strict version of 'protect'. Handles all exceptions. If exception not 
--   handled, this function return /500 Internal Server Error/ with empty 
--   headers and body what contains 'show' of error.
protect' :: (E.Exception e) =>
       (e -> Application)   -- ^ Handler
    -> Middleware
protect' handler app req = 
    app req `catch` (`handler`req) `catch` (`defHandler` req)
  where
    defHandler :: SomeException -> Application
    defHandler e _ = return $ responseLBS status500 [] $ BL.pack $ show e

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

-- | Catch exceptions inside enumerator
catch :: (E.Exception e, MonadBaseControl IO m)
      => Iteratee a m b
      -> (e -> Iteratee a m b)
      -> Iteratee a m b
catch (Iteratee mstep) f = Iteratee $ do
    step <- mstep `E.catch` (runIteratee . f)
    return $ case step of
        Continue k -> Continue $ \s -> catch (k s) f
        Yield b s -> Yield b s
        Error e -> Error e

