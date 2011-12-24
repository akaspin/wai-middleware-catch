{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.Trans.Class (lift)
import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Data.ByteString.Lazy.Char8 (pack) -- Just for an orphan instance
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Network.Wai
import Network.Wai.Middleware.Catch

data MyException = MyException String deriving (Show, Typeable)
instance Exception MyException

main :: IO ()
main = do
    putStrLn $ "http://localhost:8888/"
    run 8888 $ (protect' handler) $ app
    -- ... try 'protect'' and you see what /err/ request be handled

app :: Application
app req = case rawPathInfo req of
    "/ok" -> return $ responseLBS 
        status200 [("Content-Type", "text/plain")] "OK"
    "/err" -> error "Error"
    "/errdo" -> do 
        lift $ throw $ MyException "Exception in do"
        return $ responseLBS undefined [] "Never fired"
    "/errresp" -> return $ responseLBS undefined 
        [] "Should not catched"
    "/exc" -> throw $ MyException "Raised exception"
    _ -> return $ responseLBS status200 [("Content-Type", "text/plain")] 
            "Try any of ok, exc, err"

handler (e::MyException) _ = 
    return $ responseLBS status200 [("Content-Type", "text/plain")] 
            (pack $ show e)