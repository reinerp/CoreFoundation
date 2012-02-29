{-# LANGUAGE MagicHash, UnboxedTuples #-}
module CoreFoundation.Touch where

import GHC.Exts(touch#)
import GHC.IO(IO(..))

touch :: a -> IO ()
touch a = IO (\s -> (# touch# a s, () #))
