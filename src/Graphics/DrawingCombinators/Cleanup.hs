-- | Postponed resource cleanup triggered by GC, executed in rendering thread
module Graphics.DrawingCombinators.Cleanup
    ( CleanupAction
    , cleanQueuedGlResources
    , queueGlResourceCleanup
    ) where

import qualified Control.Exception as Exception
import           Control.Monad (join)
import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)

type CleanupAction = IO ()

{-# NOINLINE glResourceCleanupQueue #-}
glResourceCleanupQueue :: IORef CleanupAction
glResourceCleanupQueue = unsafePerformIO (newIORef (pure ()))

cleanQueuedGlResources :: IO ()
cleanQueuedGlResources =
    Exception.mask_ $ join $
    atomicModifyIORef glResourceCleanupQueue (\act -> (pure (), act))

queueGlResourceCleanup :: CleanupAction -> IO ()
queueGlResourceCleanup act =
    atomicModifyIORef glResourceCleanupQueue $
    \queue -> (act *> queue, ())
