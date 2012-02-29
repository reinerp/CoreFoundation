module CoreFoundation.Array(
  CFArray,
  fromVector,
  toVector,
  ) where

#include "CoreFoundation/CFData.h"
#include "cbits.h"

import Control.Applicative
import Control.Exception

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
data CFArray
newtype Array a = Array { unArray :: Ref CFArray }
{#pointer CFArrayRef -> CFArray#}

instance CF a => CF (Array a) where
  type Repr (Array a) = CFArray
  wrap = Array
  unwrap = unArray
instance CF a => CFConcrete (Array a) where
  type Hs (Array a) = V.Vector a
  fromHs v =
    U.unsafePerformIO $
    withVector v $ \buf len ->
    create $
    castPtr <$>
    {#call unsafe hsCFArrayCreate as ^ #} (castPtr buf) (fromIntegral len)
  toHs o =
    U.unsafePerformIO $
    withObject o $ \p -> do
      len <- {#call unsafe CFArrayGetCount as ^ #} (castPtr p)
      (res, _) <- buildVector (fromIntegral len) $ \buf ->
        {#call unsafe hsCFArrayGetValues#} (castPtr p) len (castPtr buf)
      return res
  staticType _ = TypeID {#call pure unsafe CFArrayGetTypeID as ^ #}

fromVector :: CF a => V.Vector a -> Array a
fromVector = fromHs

toVector :: CF a => Array a -> V.Vector a
toVector = toHs
