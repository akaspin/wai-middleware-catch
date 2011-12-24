{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Typeable (Typeable)

import Control.Exception (Exception, throw)

import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy.Char8 (pack) -- Just for an orphan instance

import Network.Wai
import Network.Wai.Middleware.Catch

data MyException = MyException String
    deriving (Show,Typeable)
instance Exception MyException

main :: IO ()
main = do
    putStrLn $ "Try http://localhost:8888/ and one of following: ok, exc, err"
    run 8888 $ (protect handler) $ app

app :: Application
app req = case rawPathInfo req of
    "/ok" -> return $ responseLBS status200 
                                  [("Content-Type", "text/plain")] "OK"
    "/err" -> error "Code 500 - handled"
    "/exc" -> throw $ MyException "Raised exception"
    _ -> return $ responseLBS status200 [("Content-Type", "text/plain")] 
            "Try any of ok, exc, err"

handler (e::MyException) _ = 
    return $ responseLBS status200 [("Content-Type", "text/plain")] 
            (pack $ show e)