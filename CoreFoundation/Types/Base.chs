{- | 
Internal module. Prefer to use "CoreFoundation.Types" and
"CoreFoundation.Marshal"
-}
module CoreFoundation.Types.Base(
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
  Ref,
  unsafeCastCF,
  extractPtr,
  -- *** Schemes
  Scheme,
  checkError,
  passThrough,
  create,
  maybeCreate,
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
import Data.Typeable
import Control.DeepSeq
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

-- | Opaque @CFType@ object.
data CFType
{- | 
Wrapper for @CFTypeRef@ object. This is the base type of the CoreFoundation type hierarchy.
-}
newtype Object = Object { unObject :: Ref CFType }
  deriving(Typeable)
instance CF Object where
  type Repr Object = CFType
  wrap = Object
  unwrap = unObject
instance NFData Object
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

-- | Extract the underlying pointer. Make sure to touch the 'ForeignPtr' after using the 'Ptr',
-- to make sure that the object isn't accidentally finalised.
--
-- To avoid this concern, prefer to use 'withObject' when possible
extractPtr :: CF a => a -> Ptr (Repr a)
extractPtr = U.unsafeForeignPtrToPtr . unRef . unwrap

unsafeCastCF :: (CF a, CF b) => a -> b
unsafeCastCF = wrap . Ref . castForeignPtr . unRef . unwrap

---------------- Schemes
{- |
Schemes describe how to put CoreFoundation pointers under Haskell's memory management. 

 * The Ownership Policy document (see <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Conceptual/CFMemoryMgmt/Concepts/Ownership.html>)
defines the \"Create Rule\" and the \"Get Rule\" for memory management. In the context of Haskell,
these rules can be adhered to by using the 'create' (or 'maybeCreate' or 'createArray') schemes
for any CoreFoundation function with @Copy@ or @Create@ in its name; and by using 'get' for
any other functions returning CoreFoundation objects.

 * The 'constant' scheme can be used for literal constant objects, or any other objects guaranteed never to be deallocated for the life of the program.

 * The 'checkError' scheme ensures that the return code is nonzero

 * The 'passThrough' scheme passes the result through unmodified
-}
type Scheme a b = IO a -> IO b

{- | 
The 'checkError' scheme ensures that the return code for errors.
If the return code is zero, the exception is thrown; otherwise,
@()@ is returned.
-}
checkError :: (Num a, Eq a, Exception e) => e -> Scheme a ()
checkError exception gen = do
  res <- gen
  when (res == 0) $ throw exception

{- |
The 'passThrough' scheme does no processing.

> passThrough = id
-}
passThrough :: Scheme a a
passThrough = id

{- | 
Put the newly-created object under Haskell's memory management, throwing an exception if the
object is null. When the Haskell object is garbage collected, the
CoreFoundation object has its reference count decremented.
-}
create :: CF a => Scheme (Ptr (Repr a)) a
create gen = do
  res <- maybeCreate gen
  case res of
    Nothing -> fail "CoreFoundation.create: null object"
    Just p -> return p

{- | 
Put the newly-created object under Haskell's memory management. When
the Haskell object is garbage collected, the CoreFoundation object 
has its reference count decremented.
-}
maybeCreate :: CF a => IO (Ptr (Repr a)) -> IO (Maybe a)
maybeCreate gen =
  bracketOnError
    gen
    unref
    (\ptr ->
        if ptr == nullPtr
          then return Nothing
          else (Just . wrap . Ref) <$> newForeignPtr unrefPtr ptr)

{- |
Own (i.e. retain) the object, and put it under Haskell's memory management.
The CoreFoundation object has its reference count incremented upon creation
of the Haskell object, and then decremented upon garbage collection of the
Haskell object.
-}
get :: CF a => IO (Ptr (Repr a)) -> IO a
get gen = create $ do
  ptr <- gen
  ref ptr
  return ptr

{- | 
Wrap a constant object: the underlying object's reference count is neither
incremented nor decremented.
-}
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
