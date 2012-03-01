module CoreFoundation.Types.String(
  String,
  CFString,
  fromText,
  toText,
  fromString,
  toString,
  ) where

#include "CoreFoundation/CFString.h"
#include "cbits.h"

import qualified Data.String as S
import Prelude hiding(String)
import qualified Prelude
import qualified System.IO.Unsafe as U
import Foreign
import Foreign.C.Types

import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import Data.Typeable
import Control.DeepSeq

{#import CoreFoundation.Types.Base#}

-- | The opaque CoreFoundation @CFString@ type.
data CFString
-- | 
-- Wraps the CoreFoundation @CFStringRef@ type. Literals of this type
-- may be constructed with the @OverloadedStrings@ language extension:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > my_str = "example" :: CoreFoundation.Types.String
--
newtype String = String { unString :: Ref CFString }
  deriving(Typeable)
instance CF String where
  type Repr String = CFString
  wrap = String
  unwrap = unString
{#pointer CFStringRef -> CFString#}

type instance UnHs Text.Text = String
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

-- | Convert from a Prelude 'Prelude.String'
fromString :: Prelude.String -> String
fromString = fromText . Text.pack

-- | Convert to a Prelude 'Prelude.String'
toString :: String -> Prelude.String
toString = Text.unpack . toText

instance S.IsString String where
  fromString = fromText . Text.pack
instance Show String where
  show = show . toHs
instance Eq String where
  a == b = toHs a == toHs b
instance Ord String where
  compare a b = compare (toHs a) (toHs b)
instance NFData String
