module Main (main) where

import           Control.Concurrent
import qualified Control.Concurrent.Async as Async
import           Control.Monad
import qualified Data.ByteString          as BS

main :: IO ()
main =
  Async.withAsync f1 $ \a -> Async.wait a
  --f2

f1 :: IO ()
f1 = forever $ do
  putStrLn "************************"
  threadDelay 1000000

f2 :: IO ()
f2 = forever $ do
  print $ BS.replicate 102400 65
