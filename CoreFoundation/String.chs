{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, EmptyDataDecls #-}
module CoreFoundation.String(
  CFString,
  fromText,
  toText,
  ) where

#include "CoreFoundation/CFString.h"
#include "cbits.h"

import Control.Applicative

import Foreign
import Foreign.C.Types

import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text

{#import CoreFoundation.Base#}

data CFString
{#pointer CFStringRef -> CFString#}

fromText :: Text.Text -> IO (Ref CFString)
fromText t = create $ Text.useAsPtr t $ \buf len ->
  {#call unsafe CFStringCreateWithCharacters as ^ #} nullPtr (safeCastPtr buf) (fromIntegral len)

toText :: Ref CFString -> IO Text.Text
toText ref = withRef ref $ \str_p -> do
  len <- {#call unsafe CFStringGetLength as ^ #} str_p
  ptr <- {#call unsafe CFStringGetCharactersPtr as ^ #} str_p
  if ptr /= nullPtr
    then Text.fromPtr (safeCastPtr ptr) (fromIntegral len)
    else allocaArray (fromIntegral len) $ \out_ptr -> do
      {#call unsafe hsCFStringGetCharacters as ^ #} str_p len out_ptr
      Text.fromPtr (safeCastPtr out_ptr) (fromIntegral len)

instance IsCFType CFString where 
  staticType _ = CFTypeID {#call pure unsafe CFStringGetTypeID as ^ #}

safeCastPtr :: forall a b. (Storable a, Storable b) => Ptr a -> Ptr b
safeCastPtr =
  if sizeOf (undefined :: a) == sizeOf (undefined :: b)
  then castPtr
  else error "CoreFoundation.String: Unexpected mismatch between CFString's UniChar and Data.Text's Word16"

