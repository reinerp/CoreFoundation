module CoreFoundation.Types.Data(
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

import CoreFoundation.Types.Base

import Control.DeepSeq
import Data.String
import Data.Typeable

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString.Char8

{- | Opaque type representing the CoreFoundation @CFData object@.
-}
data CFData
{- | Type wrapping @CFDataRef@. A 'ByteString' at heart. -}
newtype Data = Data { unData :: Ref CFData }
  deriving(Typeable)
instance CF Data where
  type Repr Data = CFData
  wrap = Data
  unwrap = unData
{#pointer CFDataRef -> CFData#}

type instance UnHs B.ByteString = Data
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

instance Show Data where
  show = show . toHs
instance Eq Data where
  a == b = toHs a == toHs b
instance Ord Data where
  compare a b = compare (toHs a) (toHs b)
instance IsString Data where
  fromString = fromHs . fromString
instance NFData Data
