{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Foreign
import GHC.HeapView
import Foreign.C.Types
import qualified Data.ByteString as BS


main :: IO ()
main = do
  p <- new_memory
  x <- newForeignPtr delete_memory p

  --tree <- buildHeapTree 100 (asBox ("xxx" :: BS.ByteString))
  --putStrLn $ ppHeapTree tree

  forM_ [1..100] $ \i -> do
    print $ "-------------- " <> show i
    tree <- buildHeapTree i (asBox x)
    putStrLn $ ppHeapTree tree

foreign import ccall unsafe "new_memory"
  new_memory :: IO (Ptr CInt)

foreign import ccall unsafe "&delete_memory"
  delete_memory :: FunPtr (Ptr CInt -> IO ())
