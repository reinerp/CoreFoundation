{- |
Marshalling assistance for foreign calls using CoreFoundation types.
-}
module CoreFoundation.Marshal(
  -- * Accessing the underlying pointers
  withObject,
  withMaybe,
  -- * Managing memory
  -- $memory
  Ref,
  -- ** Schemes
  -- $scheme
  Scheme,
  create,
  maybeCreate,
  get,
  constant,
  passThrough,
  checkError,
  -- ** Helper functions
  -- $helper
  call1,
  call2,
  call3,
  call4,
  call5,
  call6,
  ) where

import Foreign
import CoreFoundation.Types.Base

-- | Get the pointer for the object, or a null pointer otherwise
withMaybe :: CF a => Maybe a -> (Ptr (Repr a) -> IO b) -> IO b
withMaybe Nothing f = f nullPtr
withMaybe (Just o) f = withObject o f


{- $helper
These functions are all defined in roughly the same way. For example:

> call2 scheme f = \arg1 arg2 ->
>   withObject arg1 $ \parg1 ->
>   withObject arg2 $ \parg2 ->
>   scheme $
>   f parg1 parg2

That is, they all unpack their parameters into pointers, call the
given function, and then use the appropriate scheme to put the
resulting value under memory management. The Scheme section has some
common schemes to use. Also see the @createArray@ scheme in "CoreFoundation.Types.Array".

See "CoreFoundation.Preferences" for example uses of these helpers. For example:

> getValue :: Key -> AppID -> UserID -> HostID -> IO (Maybe Plist)
> getValue = call4 maybeCreate {#call unsafe CFPreferencesCopyValue as ^ #}

-}

call1 scheme f = \arg1 ->
  withObject arg1 $ \parg1 ->
  scheme $
  f parg1

call2 scheme f = \arg1 arg2 ->
  withObject arg1 $ \parg1 ->
  withObject arg2 $ \parg2 ->
  scheme $
  f parg1 parg2

call3 scheme f = \arg1 arg2 arg3 ->
  withObject arg1 $ \parg1 ->
  withObject arg2 $ \parg2 ->
  withObject arg3 $ \parg3 ->
  scheme $
  f parg1 parg2 parg3

call4 scheme f = \arg1 arg2 arg3 arg4 ->
  withObject arg1 $ \parg1 ->
  withObject arg2 $ \parg2 ->
  withObject arg3 $ \parg3 ->
  withObject arg4 $ \parg4 ->
  scheme $
  f parg1 parg2 parg3 parg4

call5 scheme f = \arg1 arg2 arg3 arg4 arg5 ->
  withObject arg1 $ \parg1 ->
  withObject arg2 $ \parg2 ->
  withObject arg3 $ \parg3 ->
  withObject arg4 $ \parg4 ->
  withObject arg5 $ \parg5 ->
  scheme $
  f parg1 parg2 parg3 parg4 parg5

call6 scheme f = \arg1 arg2 arg3 arg4 arg5 arg6 ->
  withObject arg1 $ \parg1 ->
  withObject arg2 $ \parg2 ->
  withObject arg3 $ \parg3 ->
  withObject arg4 $ \parg4 ->
  withObject arg5 $ \parg5 ->
  withObject arg6 $ \parg6 ->
  scheme $
  f parg1 parg2 parg3 parg4 parg5 parg6

