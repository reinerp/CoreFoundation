module CoreFoundation.Base(
  -- * Object-oriented hierarchy
  CF(..),
  CFConcrete(..),
  UnHs,
  dynamicCast,
  -- ** The 'Object' type
  Object,
  withObject,
  toObject,
  -- * Internals
  CFType,
  CFTypeRef,
  -- ** Type IDs and coercions
  TypeID(..),
  dynamicType,
  -- ** Memory management
  Ref(..),
  unsafeCastCF,
  extractPtr,
  create,
  get,
  constant,
  ) where

#include "CoreFoundation/CFBase.h"
#include "CoreFoundation/CFString.h"

import System.IO.Unsafe as U
import Foreign
import Foreign.ForeignPtr.Unsafe as U
import Foreign.C.Types
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Proxy

-------------------- Object-oriented hierarchy

{- |
The class of CoreFoundation objects. For communicating with CoreFoundation methods, the underlying CoreFoundation object can be accessed using 'withObject'. All objects can be converted to 'Object' using 'toObject'.
-}
class CF ty where
  type Repr ty
  wrap :: Ref (Repr ty) -> ty
  unwrap :: ty -> Ref (Repr ty)

{- |
\"Concrete\" CoreFoundation objects. These are immutable, and can be marshalled in pure code
using 'toHs' and 'fromHs'. Checked coercions (i.e. ones which may fail at runtime) are provided by 'dynamicCast'.
-}
class (ty ~ UnHs (Hs ty), CF ty) => CFConcrete ty where
  -- converting to hs value
  type Hs ty
  toHs :: ty -> Hs ty
  fromHs :: Hs ty -> ty
  -- misc
  staticType :: Proxy ty -> TypeID

-- | Inverse of 'Hs'. Used as a superclass of 'CFConcrete'.
type family UnHs ty :: *

{- |
Dynamic cast between CoreFoundation types. The argument's type is compared at runtime to the
desired 'staticType'. On a match, 'Just' is returned; otherwise 'Nothing'.
-}
dynamicCast :: forall a b. (CF a, CFConcrete b) => a -> Maybe b
dynamicCast a = 
  if dynamicType a == staticType (Proxy :: Proxy b)
     then Just (unsafeCastCF a)
     else Nothing

--- The 'Object' type
{#pointer CFTypeRef -> CFType #}

data CFType
newtype Object = Object { unObject :: Ref CFType }
instance CF Object where
  type Repr Object = CFType
  wrap = Object
  unwrap = unObject

-- | Unsafe: don't modify the object (although most CoreFoundation functions don't allow you to)
withObject :: CF a => a -> (Ptr (Repr a) -> IO b) -> IO b
withObject = withForeignPtr . unRef . unwrap

toObject :: CF a => a -> Object
toObject = unsafeCastCF

----------------------- Type IDs
{- |
A type for unique, constant integer values that identify particular Core Foundation opaque types.

@typedef unsigned long CFTypeID;@

 [Discussion] Defines a type identifier in Core Foundation. A type ID is an integer that identifies the opaque type to which a Core Foundation object “belongs.” You use type IDs in various contexts, such as when you are operating on heterogeneous collections. Core Foundation provides programmatic interfaces for obtaining and evaluating type IDs. Because the value for a type ID can change from release to release, your code should not rely on stored or hard-coded type IDs nor should it hard-code any observed properties of a type ID (such as, for example, it being a small integer).
-}
newtype TypeID = TypeID {#type CFTypeID#}
  deriving(Eq, Ord)

{- |
Returns the unique identifier of an opaque type to which a Core Foundation object belongs. Underlying function: @CFGetTypeID@.

 [Discussion]  This function returns a value that uniquely identifies the opaque type of any Core Foundation object. You can compare this value with the 'CFTypeID' identifier from 'staticType'. These values might
change from release to release or platform to platform.

 [Availability] Available in Mac OS X v10.0 and later.
-}
dynamicType :: CF a => a -> TypeID
dynamicType o = 
  TypeID $
  U.unsafePerformIO $
  withObject (toObject o) {#call unsafe CFGetTypeID as ^ #}

------------------- managing memory

-- | A managed reference to the object a. These are approximately pointers to a,
-- but when garbage-collected, they release their underlying objects as appropriate.
newtype Ref a = Ref { unRef :: ForeignPtr a }

unsafeCastCF :: (CF a, CF b) => a -> b
unsafeCastCF = wrap . Ref . castForeignPtr . unRef . unwrap

-- | Put the newly-created object under Haskell's memory management. This 
create :: CF a => IO (Ptr (Repr a)) -> IO a
create gen =
  bracketOnError
    gen
    unref
    (\ptr -> do
        when (ptr == nullPtr) $ fail "CoreFoundation.manageObj: object is NULL"
        (wrap . Ref) <$> newForeignPtr unrefPtr ptr)

-- | Extract the underlying pointer. Make sure to touch the 'ForeignPtr' after using the 'Ptr',
-- to make sure that the object isn't accidentally finalised.
--
-- To avoid this concern, prefer to use 'withObject' when possible
extractPtr :: CF a => a -> Ptr (Repr a)
extractPtr = U.unsafeForeignPtrToPtr . unRef . unwrap

-- | Own (i.e. retain) the object, and put it under Haskell's memory management.
get :: CF a => IO (Ptr (Repr a)) -> IO a
get gen = create $ do
  ptr <- gen
  ref ptr
  return ptr

-- | Wrap a constant object: the object does not need to be retained or released.
constant :: CF a => Ptr (Repr a) -> a
constant = wrap . Ref . U.unsafePerformIO . newForeignPtr_

-- implementation functions
foreign import ccall unsafe "&CFRelease"
  unrefPtr :: FinalizerPtr a

-- | Manual memory management
unref :: Ptr a -> IO ()
unref ptr
  | ptr == nullPtr = return ()
  | otherwise = {#call unsafe CFRelease as ^ #} (castPtr ptr)

ref :: Ptr a -> IO ()
ref ptr
  | ptr == nullPtr = return ()
  | otherwise = Foreign.void $ {#call unsafe CFRetain as ^ #} (castPtr ptr)
