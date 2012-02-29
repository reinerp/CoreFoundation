module CoreFoundation.String(
  String,
  CFString,
  fromText,
  toText,
  ) where

#include "CoreFoundation/CFString.h"
#include "cbits.h"

import Control.Applicative

import Prelude hiding(String)
import qualified System.IO.Unsafe as U
import Foreign
import Foreign.C.Types

import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text

{#import CoreFoundation.Base#}

data CFString
newtype String = String { unString :: Ref CFString }
instance CF String where
  type Repr String = CFString
  wrap = String
  unwrap = unString
{#pointer CFStringRef -> CFString#}

instance CFConcrete String where
  type Hs String = Text.Text
  fromHs t = 
    U.unsafePerformIO $
    Text.useAsPtr t $ \buf len ->
    create $
    {#call unsafe CFStringCreateWithCharacters as ^ #} nullPtr (castPtr buf) (fromIntegral len)
  toHs str =
    U.unsafePerformIO $
    withObject str $ \str_p -> do
      len <- {#call unsafe CFStringGetLength as ^ #} str_p
      ptr <- {#call unsafe CFStringGetCharactersPtr as ^ #} str_p
      if ptr /= nullPtr
        then Text.fromPtr (castPtr ptr) (fromIntegral len)
        else allocaArray (fromIntegral len) $ \out_ptr -> do
          {#call unsafe hsCFStringGetCharacters as ^ #} str_p len out_ptr
          Text.fromPtr (castPtr out_ptr) (fromIntegral len)
  staticType _ = TypeID {#call pure unsafe CFStringGetTypeID as ^ #}


-- | Synonym for 'fromHs'
fromText :: Text.Text -> String
fromText = fromHs

-- | Synonym for 'toHs'
toText :: String -> Text.Text
toText = toHs
