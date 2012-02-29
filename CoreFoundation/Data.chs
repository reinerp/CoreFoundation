module CoreFoundation.Data(
  Data,
  CFData,
  fromByteString,
  toByteString,
  ) where

#include "CoreFoundation/CFData.h"
#include "cbits.h"

import Control.Applicative

import qualified System.IO.Unsafe as U
import Foreign
import Foreign.C.Types

import CoreFoundation.Base

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

{- |
CFData and its derived mutable type, CFMutableData (not implemented in Haskell), provide support for data objects, object-oriented wrappers for byte buffers. Data objects let simple allocated buffers (that is, data with no embedded pointers) take on the behavior of Core Foundation objects. CFData creates static data objects, and CFMutableData creates dynamic data objects. Data objects are typically used for raw data storage.
-}
data CFData
newtype Data = Data { unData :: Ref CFData }
instance CF Data where
  type Repr Data = CFData
  wrap = Data
  unwrap = unData
{#pointer CFDataRef -> CFData#}

instance CFConcrete Data where
  type Hs Data = B.ByteString
  fromHs bs = 
    U.unsafePerformIO $
    B.unsafeUseAsCStringLen bs $ \(buf, len) ->
    create $
    {#call unsafe CFDataCreate as ^ #} nullPtr (castPtr buf) (fromIntegral len)
  toHs o = 
    U.unsafePerformIO $
    withObject o $ \p -> do
      buf <- {#call unsafe CFDataGetBytePtr as ^ #} p
      len <- {#call unsafe CFDataGetLength as ^ #} p
      B.packCStringLen (castPtr buf, fromIntegral len)
  staticType _ = TypeID {#call pure unsafe CFDataGetTypeID as ^ #}

-- | Synonym for 'fromHs'
fromByteString :: B.ByteString -> Data
fromByteString = fromHs

-- | Synonym for 'toHs'
toByteString :: Data -> B.ByteString
toByteString = toHs
