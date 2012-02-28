{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, EmptyDataDecls #-}
module CoreFoundation.Data(
  CFData,
  fromByteString,
  toByteString,
  ) where

#include "CoreFoundation/CFData.h"
#include "cbits.h"

import Control.Applicative

import Foreign
import Foreign.C.Types

import CoreFoundation.Base

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

{- |
CFData and its derived mutable type, CFMutableData (not implemented in Haskell), provide support for data objects, object-oriented wrappers for byte buffers. Data objects let simple allocated buffers (that is, data with no embedded pointers) take on the behavior of Core Foundation objects. CFData creates static data objects, and CFMutableData creates dynamic data objects. Data objects are typically used for raw data storage.
-}
data CFData
{#pointer CFDataRef -> CFData#}

fromByteString :: B.ByteString -> IO (Ref CFData)
fromByteString bs = create $ B.unsafeUseAsCStringLen bs $ \(buf, len) ->
  {#call unsafe CFDataCreate as ^ #} nullPtr (castPtr buf) (fromIntegral len)

toByteString :: Ref CFData -> IO B.ByteString
toByteString rp = withRef rp $ \p -> do
  buf <- {#call unsafe CFDataGetBytePtr as ^ #} p
  len <- {#call unsafe CFDataGetLength as ^ #} p
  B.packCStringLen (castPtr buf, fromIntegral len)

instance IsCFType CFData where 
  staticType _ = CFTypeID {#call pure unsafe CFDataGetTypeID as ^ #}
