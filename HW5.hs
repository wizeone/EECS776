import Control.Monad
import System.Random
import Control.Concurrent
import Data.Char

factorial:: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

randomNum:: Int -> Int
randomNum = randomRIO(1::Int,25)

printVal:: (Int, String, Int) -> IO()
printVal (x, y, z) = do
  putStr (y ++ " -> " ++ (show x) ++ " == " ++ (show z))

beginForks mVar1 mVar2 = do
  thread1 <- forkIO (forever $ do
    val <- randomNum
    putMVar mVar1 val)

  thread2 <- forkIO (forever $ do
    val <- takeMVar mVar1
    putMVar mVar2 (val, "factorial", factorial val))

  thread3 <- forkIO (forever $ do
    val <- takeMVar mVar2
    threadDelay (1000000)
    printVal val)

main::IO()
main = do
  mVar1 <- newEmptyMVar
  mVar2 <- newEmptyMVar
  beginForks mVar1 mVar2
