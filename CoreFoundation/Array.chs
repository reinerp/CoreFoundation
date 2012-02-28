{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, EmptyDataDecls #-}
module CoreFoundation.Array(
  CFArray,
  fromVector,
  toVector,
  ) where

#include "CoreFoundation/CFData.h"
#include "cbits.h"

import Control.Applicative

import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign hiding(unsafeForeignPtrToPtr)
import Foreign.C.Types

{#import CoreFoundation.Base#}

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM

{- |
Arrays of 'CFType' objects.
-}
data CFArray
{#pointer CFArrayRef -> CFArray#}

extractPtr :: Ref CFType -> Ptr CFType
extractPtr (Ref r) = unsafeForeignPtrToPtr r

fromVector :: V.Vector (Ref CFType) -> IO (Ref CFArray)
fromVector v = 
  create $
    S.unsafeWith (V.convert (V.map extractPtr v)) $ \buf ->
      {#call unsafe hsCFArrayCreate as ^ #} buf (fromIntegral $ V.length v)

toVector :: Ref CFArray -> IO (V.Vector (Ref CFType))
toVector r = withRef r $ \p -> do
  len <- {#call unsafe CFArrayGetCount as ^ #} p
  mvec <- SM.new (fromIntegral len)
  SM.unsafeWith mvec $ \ptr -> {#call unsafe hsCFArrayGetValues#} p len ptr
  vec <- S.unsafeFreeze mvec
  V.mapM (create . return) $ S.convert vec

instance IsCFType CFArray where 
  staticType _ = CFTypeID {#call pure unsafe CFArrayGetTypeID as ^ #}
