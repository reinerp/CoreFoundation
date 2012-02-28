{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, EmptyDataDecls #-}
module CoreFoundation.Array(
  CFArray,
  fromVector,
  toVector,
  ) where

#include "CoreFoundation/CFData.h"
#include "cbits.h"

import Control.Applicative

import Foreign
import Foreign.C.Types

import CoreFoundation.Base

import qualified Data.Vector as V

{- |
Arrays of 'CFType' objects.
-}
data CFArray
{#pointer CFArrayRef -> CFArray#}

fromVector :: V.Vector (Ref CFData) -> IO (Ref CFArray)
fromVector = undefined -- we can do this one

fromVectorManaged :: V.Vector (ForeignPtr CFData) -> Create CFArray

fromByteString :: B.ByteString -> Create CFData
fromByteString bs = Create $ B.unsafeUseAsCStringLen bs $ \(buf, len) ->
  {#call unsafe CFDataCreate as ^ #} nullPtr (castPtr buf) (fromIntegral len)

toByteString :: Ptr CFData -> IO B.ByteString
toByteString p = do
  buf <- {#call unsafe CFDataGetBytePtr as ^ #} p
  len <- {#call unsafe CFDataGetLength as ^ #} p
  B.packCStringLen (castPtr buf, fromIntegral len)

instance IsCFType CFData where 
  staticType _ = CFTypeID {#call pure unsafe CFDataGetTypeID as ^ #}
