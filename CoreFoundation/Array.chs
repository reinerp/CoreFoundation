{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, EmptyDataDecls #-}
module CoreFoundation.Array(
  CFArray,
  fromVector,
  toVector,
  -- * internal
  withVector_,
  buildVector_,
  
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
data CFArray a
--{#pointer CFArrayRef -> CFArray#}

extractPtr :: Ref a -> Ptr a
extractPtr (Ref r) = unsafeForeignPtrToPtr r

fromVector :: IsCFType a => V.Vector (Ref a) -> IO (Ref (CFArray a))
fromVector v = 
    withVector_ v $ \buf len ->  
      create $ 
        castPtr <$> 
          {#call unsafe hsCFArrayCreate as ^ #} (castPtr buf) (fromIntegral len)

toVector :: IsCFType a => Ref (CFArray a) -> IO (V.Vector (Ref a))
toVector r = withRef r $ \p -> do
  len <- {#call unsafe CFArrayGetCount as ^ #} (castPtr p)
  fst <$> buildVector_ (fromIntegral len) 
    (\buf -> {#call unsafe hsCFArrayGetValues#} (castPtr p) len (castPtr buf))

withVector_ :: V.Vector (Ref a) -> (Ptr (Ptr a) -> Int -> IO b) -> IO b
withVector_ v f = 
  S.unsafeWith (V.convert (V.map extractPtr v)) $ \buf -> f buf (V.length v)

buildVector_ :: IsCFType a => Int -> (Ptr (Ptr a) -> IO b) -> IO (V.Vector (Ref a), b)
buildVector_ len f = do
  mvec <- SM.new (fromIntegral len)
  res <- SM.unsafeWith mvec $ \ptr -> f ptr
  vec <- S.unsafeFreeze mvec
  vec' <- V.mapM (get . return) $ S.convert vec
  return (vec', res)

instance IsCFType (CFArray a) where 
  staticType _ = CFTypeID {#call pure unsafe CFArrayGetTypeID as ^ #}
