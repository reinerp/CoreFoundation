-- | See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFDictionaryRef/Reference/reference.html>
module CoreFoundation.Types.Dictionary(
  Dictionary,
  CFDictionary,
  fromVectors,
  toVectors,
  fromMap,
  toMap,
  ) where

#include "CoreFoundation/CFDictionary.h"
#include "cbits.h"

import Control.Applicative

import qualified System.IO.Unsafe as U
import Foreign
import Foreign.C.Types

import Data.Typeable
import Control.DeepSeq
import qualified Data.Text.Lazy as Text


{#import CoreFoundation.Types.Base#}
import CoreFoundation.Types.Array.Internal

import qualified Data.Map as M
import qualified Data.Vector as V

{- |
The CoreFoundation @CFDictionary@ type.
-}
data CFDictionary
{- |
A dictionary with keys of type @k@ and values of type @v@. Wraps
@CFDictionaryRef@.
-}
newtype Dictionary k v = Dictionary { unDictionary :: Ref CFDictionary }
  deriving(Typeable)
{#pointer CFDictionaryRef -> CFDictionary #}
instance (CF k, CF v) => CF (Dictionary k v) where
  type Repr (Dictionary k v) = CFDictionary
  wrap = Dictionary
  unwrap = unDictionary

type instance UnHs (V.Vector k, V.Vector v) = Dictionary k v
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

-- | Synonym for 'toHs'
toVectors :: (CF k, CF v) => Dictionary k v -> (V.Vector k, V.Vector v)
toVectors = toHs

-- | Convert from a 'Map'
fromMap :: (CF k, CF v) => M.Map k v -> Dictionary k v
fromMap = fromVectors . V.unzip . V.fromList . M.toList

-- | Convert to a 'Map'
toMap :: (Ord k, CF k, CF v) => Dictionary k v -> M.Map k v
toMap = M.fromList . V.toList . uncurry V.zip . toVectors

instance (CF k, CF v, Show k, Show v) => Show (Dictionary k v) where
  show = interCommas . V.map showPair . uncurry V.zip . toHs
    where
      showPair (k, v) = show k ++ ":" ++ show v
      interCommas = Text.unpack . Text.intercalate ", " . V.toList . V.map Text.pack
instance (CF k, CF v, Eq k, Eq v) => Eq (Dictionary k v) where
  a == b = toHs a == toHs b
-- | Equality by converting to a 'Map'
instance (CF k, CF v, Ord k, Ord v) => Ord (Dictionary k v) where
  compare a b = compare (toMap a) (toMap b)
instance NFData (Dictionary k v)
