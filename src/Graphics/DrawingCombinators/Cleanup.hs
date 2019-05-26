-- | Postponed resource cleanup triggered by GC, executed in rendering thread
module Graphics.DrawingCombinators.Cleanup
    ( CleanupAction
    , cleanQueuedGlResources
    , queueGlResourceCleanup
    ) where

import           Control.Concurrent (ThreadId, myThreadId)
import qualified Control.Exception as Exception
import           Data.IORef
import           Data.List (partition)
import           System.IO.Unsafe (unsafePerformIO)

type CleanupAction = (ThreadId, IO ())

{-# NOINLINE glResourceCleanupQueue #-}
glResourceCleanupQueue :: IORef [CleanupAction]
glResourceCleanupQueue = unsafePerformIO (newIORef [])

cleanQueuedGlResources :: IO ()
cleanQueuedGlResources =
    do
        tid <- myThreadId
        Exception.mask_ $
            atomicModifyIORef glResourceCleanupQueue (partition ((/= tid) . fst))
            >>= mapM_ snd

queueGlResourceCleanup :: ThreadId -> IO () -> IO ()
queueGlResourceCleanup tid act =
    atomicModifyIORef glResourceCleanupQueue $
    \queue -> ((tid, act) : queue, ())
