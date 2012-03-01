-- | See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFArrayRef/Reference/reference.html>
module CoreFoundation.Types.Array(
  Array,
  CFArray,
  fromVector,
  toVector,
  fromList,
  toList,
  createArray,
  ) where

#include "CoreFoundation/CFData.h"
#include "cbits.h"

import Control.Applicative
import Control.Exception
import Data.Maybe(fromMaybe)
import Data.Typeable
import Control.DeepSeq

import qualified System.IO.Unsafe as U
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign hiding(unsafeForeignPtrToPtr)
import Foreign.C.Types

{#import CoreFoundation.Types.Base#}
import CoreFoundation.Types.Array.Internal
import CoreFoundation.Marshal

import qualified Data.Vector as V

-- | The opaque CoreFoundation @CFArray@ type.
data CFArray
{- | 
Arrays of pointers. Wraps the @CFArrayRef@ type.
-}
newtype Array a = Array { unArray :: Ref CFArray }
  deriving(Typeable)
{#pointer CFArrayRef -> CFArray#}

instance CF a => CF (Array a) where
  type Repr (Array a) = CFArray
  wrap = Array
  unwrap = unArray

type instance UnHs (V.Vector a) = Array a
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

-- | Synonym for 'fromHs'
fromVector :: CF a => V.Vector a -> Array a
fromVector = fromHs

-- | Synonym for 'toHs'
toVector :: CF a => Array a -> V.Vector a
toVector = toHs

-- | Convert from a list
fromList :: CF a => [a] -> Array a
fromList = fromVector . V.fromList

-- | Convert to a list
toList :: CF a => Array a -> [a]
toList = V.toList . toVector

{- |
CoreFoundation represents empty arrays by null pointers, which
may not be passed to 'create'. Instead, use this scheme for wrapping
arrays.
-}
createArray :: CF a => Scheme (Ptr CFArray) (Array a)
createArray = fmap (fromMaybe (fromHs V.empty)) . maybeCreate

instance (CF a, Show a) => Show (Array a) where
  show = show . toHs
instance (CF a, Eq a) => Eq (Array a) where
  a == b = toHs a == toHs b
instance (CF a, Ord a) => Ord (Array a) where
  compare a b = compare (toHs a) (toHs b)
-- | For CoreFoundation 'Array's, 'seq' and 'deepSeq' are the same
instance (CF a, NFData a) => NFData (Array a)
