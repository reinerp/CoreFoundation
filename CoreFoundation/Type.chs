{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
module CoreFoundation.Type(
  -- * Object-oriented hierarchy
  Proxy,
  CFType,
  IsCFType(..),
  -- ** Coercing to @CFType@
  toCFType,
  -- ** Coercing between Core Foundation types
  CFTypeID(..),
  dynamicType,
  dynamicCast,
  typeIDDescription,
  -- * Memory management
  Ref,
  -- * Determining equality
  equal,
  -- * Hashing
  hash,
  -- * Description
  description,
  ) where

#include "CoreFoundation/CFBase.h"

import System.IO.Unsafe(unsafePerformIO)
import C2HS hiding(unsafePerformIO)

import Control.Applicative
import Control.Monad

import Data.Text(Text, unpack)
{#import CoreFoundation.Base#}
import CoreFoundation.String

-- because the newtype is defined in Base
{#pointer CFStringRef -> CFString#}

-- helper
withCFType :: IsCFType a => Ref a -> (Ptr CFType -> IO b) -> IO b
withCFType ref = withRef (toCFType ref)

-------------------- Determining equality
{- |
Determines whether two Core Foundation objects are considered equal.

 [Discussion] Equality is something specific to each Core Foundation opaque type. For example, two CFNumber objects are equal if the numeric values they represent are equal. Two CFString objects are equal if they 
represent identical sequences of characters, regardless of encoding.
-}
{#fun unsafe CFEqual as equal
 `(IsCFType a, IsCFType b)' => {withCFType* `Ref a', withCFType* `Ref b'} -> `Bool'#}

{-
equal :: (IsCFType a, IsCFType b) => Ptr a -> Ptr b -> IO Bool
equal fp1 fp2 = 
  withCFTypeRef (toCFTypeRef fp1) $ \p1 ->
  withCFTypeRef (toCFTypeRef fp2) $ \p2 ->
    cvtBool <$> {#call unsafe CFEqual as ^ #} p1 p2
-}
--------- Hashing
-- | type used for CoreFoundation hashes. Use 'fromIntegral' for conversions as necessary.
newtype CFHashCode = CFHashCode {#type CFHashCode#}
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

{- |
Returns a code that can be used to identify an object in a hashing structure.

 [Discussion] Two objects that are equal (as determined by the 'equal' function) have the same hashing value. However, the converse is not true: two objects with the same hashing value might not be equal. That is, hashing values are not necessarily unique. The hashing value for an object might change from release to release or from platform to platform.
-}
{#fun unsafe CFHash as hash
  `IsCFType a' => {withCFType* `Ref a'} -> `CFHashCode' CFHashCode #}
{-
hash :: IsCFType a => a -> IO CFHashCode
hash fp = CFHashCode <$> withCFTypeRef (toCFTypeRef fp) {#call unsafe CFHash as ^ #}
-}

--------- Description
{- |
Returns a textual description of a Core Foundation object.

*Discussion*
The nature of the description differs by object. For example, a description of a CFArray object would include descriptions of each of the elements in the collection.

You can use this function for debugging Core Foundation objects in
your code. Note, however, that the description for a given object
may be different in different releases of the operating system. Do
not create dependencies in your code on the content or format of
the information returned by this function.
-}
description :: IsCFType a => Ref a -> IO Text
description p = toText =<< create (withRef (toCFType p) {#call unsafe CFCopyDescription as ^ #})

---------- Type ID

{- |
Returns a textual description of a Core Foundation type, as identified by its type ID, which can be used when debugging.

*Discussion*
You can use this function for debugging Core Foundation objects in your code. Note, however, that the description for a given object may be different in different releases of the operating system. Do not create dependencies in your code on the content or format of the information returned by this function.

*Availability*
Available in Mac OS X v10.0 and later.
-}
typeIDDescription :: CFTypeID -> Text
typeIDDescription (CFTypeID tid) =
  unsafePerformIO $
    toText =<< create ({#call unsafe CFCopyTypeIDDescription as ^ #} tid)

instance Show CFTypeID where show = unpack . typeIDDescription
