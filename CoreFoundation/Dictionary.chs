{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, EmptyDataDecls, ViewPatterns #-}
module CoreFoundation.Dictionary(
  Dictionary,
  CFDictionary,
  fromVectors,
  toVectors,
  ) where

#include "CoreFoundation/CFDictionary.h"
#include "cbits.h"

import Control.Applicative

import qualified System.IO.Unsafe as U
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign hiding(unsafeForeignPtrToPtr)
import Foreign.C.Types

{#import CoreFoundation.Base#}
import CoreFoundation.Array.Internal

import qualified Data.Vector as V

{- |
Arrays of 'CFType' objects.
-}
data CFDictionary
newtype Dictionary k v = Dictionary { unDictionary :: Ref CFDictionary }
{#pointer CFDictionaryRef -> CFDictionary #}
instance (CF k, CF v) => CF (Dictionary k v) where
  type Repr (Dictionary k v) = CFDictionary
  wrap = Dictionary
  unwrap = unDictionary
instance (CF k, CF v) => CFConcrete (Dictionary k v) where
  type Hs (Dictionary k v) = (V.Vector k, V.Vector v)
  fromHs (keys, vals)
    | V.length keys /= V.length vals = error "CoreFoundation.Dictionary.fromHs: Vectors must have equal length"
    | otherwise =
        U.unsafePerformIO $
        withVector keys $ \pk len ->
        withVector vals $ \pv _ ->
        create $ 
        castPtr <$> 
        {#call unsafe hsCFDictionaryCreate as ^ #} 
          (castPtr pk)
          (castPtr pv)
          (fromIntegral len)
  toHs o =
    U.unsafePerformIO $
    withObject o $ \p -> do
      len <- {#call unsafe CFDictionaryGetCount as ^ #} p
      buildVector (fromIntegral len) $ \kp ->
        fst <$> (buildVector (fromIntegral len) $ \vp ->
          {#call unsafe CFDictionaryGetKeysAndValues as ^ #} 
             p 
             (castPtr kp) 
             (castPtr vp)
         )
  staticType _ = TypeID {#call pure unsafe CFDictionaryGetTypeID as ^ #}

-- | Synonym for 'fromHs'
fromVectors :: (CF k, CF v) => (V.Vector k, V.Vector v) -> Dictionary k v
fromVectors = fromHs

toVectors :: (CF k, CF v) => Dictionary k v -> (V.Vector k, V.Vector v)
toVectors = toHs
