{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, EmptyDataDecls, ViewPatterns #-}
module CoreFoundation.Dictionary(
  CFDictionary,
  fromVectors,
  toVectors,
  ) where

#include "CoreFoundation/CFDictionary.h"
#include "cbits.h"

import Control.Applicative

import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign hiding(unsafeForeignPtrToPtr)
import Foreign.C.Types

{#import CoreFoundation.Base#}
import CoreFoundation.Array

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM

{- |
Arrays of 'CFType' objects.
-}
data CFDictionary k v
--{#pointer CFDictionaryRef -> CFDictionary #}

fromVectors :: (IsCFType k, IsCFType v) => 
   V.Vector (Ref k) -> V.Vector (Ref v) -> IO (Ref (CFDictionary k v))
fromVectors keys vals 
  | V.length keys /= V.length vals = error "CoreFoundation.Dictionary.fromVectors: Vectors must have equal length"
  | otherwise =
  withVector_ keys $ \pk len ->
  withVector_ vals $ \pv _ ->
    create $ castPtr <$>
      {#call unsafe hsCFDictionaryCreate as ^ #} (castPtr pk) (castPtr pv) (fromIntegral len)

toVectors :: (IsCFType k, IsCFType v) => Ref (CFDictionary k v) -> IO (V.Vector (Ref k), V.Vector (Ref v))
toVectors r = withRef r $ \(castPtr -> p) -> do
  len <- {#call unsafe CFDictionaryGetCount as ^ #} p
  buildVector_ (fromIntegral len) $ \kp ->
    fst <$> (buildVector_ (fromIntegral len) $ \vp ->
      {#call unsafe CFDictionaryGetKeysAndValues as ^ #} p (castPtr kp) (castPtr vp))

instance IsCFType (CFDictionary k v) where 
  staticType _ = CFTypeID {#call pure unsafe CFDictionaryGetTypeID as ^ #}
