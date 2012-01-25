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

data MyOtherException = MyOtherException String deriving (Show, Typeable)
instance Exception MyOtherException

main :: IO ()
main = do
    putStrLn $ "http://localhost:8888/"
    run 8888 $ (protect [mkHandler handler1, defHandler]) $ app
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
    "/exc1" -> throw $ MyException "Raised exception"
    "/exc2" -> throw $ MyOtherException "Raised exception"
    _ -> return $ responseLBS status200 [("Content-Type", "text/plain")] 
            "Try any of ok, exc, err"

handler1 (MyException m) _ = 
    return $ responseLBS status200 [("Content-Type", "text/plain")] 
            (pack $ "My : " ++ m)

handler2 (MyOtherException m) _ = 
    return $ responseLBS status200 [("Content-Type", "text/plain")] 
            (pack $ "My other : " ++ m)