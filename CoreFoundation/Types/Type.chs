-- | See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFTypeRef/Reference/reference.html>
module CoreFoundation.Types.Type(
  -- * Object-oriented hierarchy
  CF(..),
  CFConcrete(..),
  dynamicCast,
  -- ** The 'Object' type
  Object,
  withObject,
  toObject,
  -- * Determining equality
  equal,
  -- * Hashing
  hash,
  -- * Description
  description,
  ) where

#include "CoreFoundation/CFBase.h"

import qualified System.IO.Unsafe as U
import C2HS

import Control.Applicative
import Control.Monad

import Data.Text(Text, unpack)
{#import CoreFoundation.Types.Base#}
import CoreFoundation.Types.String

{#pointer CFStringRef -> CFString#}

-------------------- Determining equality
{- |
Determines whether two Core Foundation objects are considered equal.

 [Discussion] Equality is something specific to each Core Foundation opaque type. For example, two CFNumber objects are equal if the numeric values they represent are equal. Two CFString objects are equal if they 
represent identical sequences of characters, regardless of encoding.
-}
equal :: (CF a, CF b) => a -> b -> Bool
equal o1 o2 =
  U.unsafePerformIO $
  withObject (toObject o1) $ \p1 ->
  withObject (toObject o2) $ \p2 ->
  (/=0) <$> {#call unsafe CFEqual as ^ #} p1 p2

--------- Hashing
-- | type used for CoreFoundation hashes. Use 'fromIntegral' for conversions as necessary.
newtype HashCode = HashCode {#type CFHashCode#}
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

{- |
Returns a code that can be used to identify an object in a hashing structure.

 [Discussion] Two objects that are equal (as determined by the 'equal' function) have the same hashing value. However, the converse is not true: two objects with the same hashing value might not be equal. That is, hashing values are not necessarily unique. The hashing value for an object might change from release to release or from platform to platform.
-}
hash :: CF a => a -> HashCode
hash o =
  HashCode $
  U.unsafePerformIO $
  withObject (toObject o) $
  {#call unsafe CFHash as ^ #}

--------- Description
{- |
Returns a textual description of a Core Foundation object.

[Discussion] The nature of the description differs by object. For example, a description of a CFArray object would include descriptions of each of the elements in the collection. You can use this function for debugging Core Foundation objects in your code. Note, however, that the description for a given object may be different in different releases of the operating system. Do not create dependencies in your code on the content or format of
the information returned by this function.

This function string is \"approximately\" referentially transparent: for equal objects @a@ and @b@,
one \"morally\" has @description a == description b@, except that descriptions contain pointer locations.
Thus this function lives in 'IO'.
-}
description :: CF a => a -> IO Text
description o =
  withObject (toObject o) $ \p ->
    toText <$> create ({#call unsafe CFCopyDescription as ^ #} p)

---------- Type ID

{- |
Returns a textual description of a Core Foundation type, as identified by its type ID, which can be used when debugging.

[Discussion] You can use this function for debugging Core Foundation objects in your code. Note, however, that the description for a given object may be different in different releases of the operating system. Do not create dependencies in your code on the content or format of the information returned by this function.

[Availability] Available in Mac OS X v10.0 and later.
-}
typeIDDescription :: TypeID -> Text
typeIDDescription (TypeID tid) =
  U.unsafePerformIO $
    toText <$> create ({#call unsafe CFCopyTypeIDDescription as ^ #} tid)

instance Show TypeID where show = unpack . typeIDDescription
