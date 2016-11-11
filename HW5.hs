import Control.Monad
import System.Random
import Control.Concurrent
import Data.Char

factorial:: Integer -> Integer
factorial 1 = 1
factorial n = n * (factorial (n-1))

squared:: Integer -> Integer
squared n = n * n

cubed:: Integer -> Integer
cubed n = n * n * n

randomNum:: IO Integer
randomNum = randomRIO(1::Integer,25)

printVal:: (Integer, String, Integer) -> IO()
printVal (x, y, z) = do
  putStrLn (y ++ " -> " ++ (show x) ++ " == " ++ (show z))

beginForks mVar1 mVar2 = do
  thread1 <- forkIO (forever $ do
    val <- randomNum
    putMVar mVar1 val)

  thread2 <- forkIO (forever $ do
    val <- takeMVar mVar1
    putMVar mVar2 (val, "factorial", factorial val))

  thread3 <- forkIO (forever $ do
    val <- takeMVar mVar1
    putMVar mVar2 (val, "squared", squared val))

  thread4 <- forkIO (forever $ do
    val <- takeMVar mVar1
    putMVar mVar2 (val, "cubed", cubed val))

  thread5 <- forkIO (forever $ do
    val <- takeMVar mVar2
    threadDelay (1000000)
    printVal val)
  putStrLn "Threads created"

main::IO()
main = do
  mVar1 <- newEmptyMVar
  mVar2 <- newEmptyMVar
  beginForks mVar1 mVar2
