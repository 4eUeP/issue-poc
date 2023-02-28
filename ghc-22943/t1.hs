module Main (main) where

import           Control.Concurrent
import qualified Control.Concurrent.Async as Async
import           Control.Monad
import qualified Data.ByteString          as BS

main :: IO ()
main =
  -- Async.withAsync f1 $ \a -> Async.wait a
  -- f2
  -- f4
  f3

f1 :: IO ()
f1 = forever $ do
  putStrLn "************************"
  threadDelay 1000000

f2 :: IO ()
f2 = forever $ do
  print $ BS.replicate 102400 65

f3 :: IO ()
f3 =
  Async.withAsync f1 $ \a -> do
    a1 <- Async.async f1
    Async.link2Only (const True) a a1
    Async.wait a

f4 :: IO ()
f4 = forever $ do
  t1 <- forkIO f1
  throwTo t1 (userError "aa")
