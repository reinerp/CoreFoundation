{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving, EmptyDataDecls, ScopedTypeVariables #-}
module CoreFoundation.Base(
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
  -- * Memory management
  Ref,
  withRef,
  create,
  unsafeCastRef,
  ) where

#include "CoreFoundation/CFBase.h"
#include "CoreFoundation/CFString.h"

import Foreign
import Foreign.C.Types
import Control.Applicative
import Control.Monad
import Control.Exception

{#pointer CFTypeRef -> CFType #}
-------------------- Object-oriented hierarchy
----------------------- Type IDs
{- |
A type for unique, constant integer values that identify particular Core Foundation opaque types.

@typedef unsigned long CFTypeID;@

 [Discussion] Defines a type identifier in Core Foundation. A type ID is an integer that identifies the opaque type to which a Core Foundation object “belongs.” You use type IDs in various contexts, such as when you are operating on heterogeneous collections. Core Foundation provides programmatic interfaces for obtaining and evaluating type IDs. Because the value for a type ID can change from release to release, your code should not rely on stored or hard-coded type IDs nor should it hard-code any observed properties of a type ID (such as, for example, it being a small integer).
-}
newtype CFTypeID = CFTypeID {#type CFTypeID#}
  deriving(Eq, Ord)

{- |
Returns the unique identifier of an opaque type to which a Core Foundation object belongs. Underlying function: @CFGetTypeID@.

 [Discussion]  This function returns a value that uniquely identifies the opaque type of any Core Foundation object. You can compare this value with the 'CFTypeID' identifier from 'staticType'. These values might
change from release to release or platform to platform.

 [Availability] Available in Mac OS X v10.0 and later.
-}
dynamicType :: IsCFType a => Ref a -> IO CFTypeID
dynamicType p = CFTypeID <$> withRef (toCFType p) {#call unsafe CFGetTypeID as ^ #}

-- | Uninhabited type; see 'staticType'.
data Proxy a

{- |
All other Core Foundation opaque types derive from CFType. The functions, callbacks, data types, and constants defined for CFType can be used by any derived opaque type. Hence, CFType functions are referred to as “polymorphic functions.” You use CFType functions to retain and release objects, to compare and inspect objects, get descriptions of objects and opaque types, and to get object allocators.

See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFTypeRef/Reference/reference.html>.
-}
data CFType

-- | The class of all CoreFoundation objects.
class IsCFType o where
  -- | Static type ID. The first argument is not inspected, so this function may be called for example
  -- as follows
  --
  -- > staticType (undefined :: Proxy CFString)
  --
  staticType :: Proxy o -> CFTypeID
instance IsCFType CFType where
  staticType _ = error "CFType has no static type"

{- |
Safe conversion to 'CFType'.
-}
toCFType :: IsCFType a => Ref a -> Ref CFType
toCFType = unsafeCastRef

{- |
Dynamic cast between CoreFoundation types. The argument's type is compared at runtime to the
desired 'staticType'. On a match, 'Just' is returned; otherwise 'Nothing'.
-}
dynamicCast :: forall a b. (IsCFType a, IsCFType b) => Ref a -> IO (Maybe (Ref b))
dynamicCast ref = do
  ty <- dynamicType ref
  if ty == staticType (undefined :: Proxy b)
    then return (Just (unsafeCastRef ref))
    else return Nothing

------------------- managing memory

-- | A managed reference to the object a. These are approximately pointers to a,
-- but when garbage-collected, they release their underlying objects as appropriate.
newtype Ref a = Ref (ForeignPtr a)

unsafeCastRef :: Ref a -> Ref b
unsafeCastRef (Ref a) = Ref (castForeignPtr a)

-- | Use the managed object.
withRef :: Ref a -> (Ptr a -> IO b) -> IO b
withRef (Ref fp) = withForeignPtr fp

-- | Put the newly-created object under Haskell's memory management.
create :: IsCFType a => IO (Ptr a) -> IO (Ref a)
create gen =
  bracketOnError
    gen
    unref
    (\ptr -> do
        when (ptr == nullPtr) $ fail "CoreFoundation.manageObj: object is NULL"
        Ref <$> newForeignPtr unrefPtr ptr)

-- implementation functions
foreign import ccall unsafe "&CFRelease"
  unrefPtr :: FinalizerPtr a

-- | Manual memory management
unref :: IsCFType a => Ptr a -> IO ()
unref ptr
  | ptr == nullPtr = return ()
  | otherwise = {#call unsafe CFRelease as ^ #} (castPtr ptr)
