{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables #-}

{- | Exception handling for 'Wai' and 'Warp'.

By default 'Warp' not handles exceptions well. It just log them to console. 
  
-}

module Network.Wai.Middleware.Catch (
    protect
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

{- | Allows to protect responses from exceptions.

     

-}
protect :: (E.Exception e) =>
       (e -> Application)   -- ^ Handler to intercept 
    -> Middleware
protect handler app req = 
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

