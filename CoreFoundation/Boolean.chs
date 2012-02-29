module CoreFoundation.Boolean(
  Boolean,
  CFBoolean,
  toBool,
  fromBool,
  ) where

#include <CoreFoundation/CFDate.h>
#include "cbits.h"

import CoreFoundation.Base
import System.IO.Unsafe as U

import Foreign hiding (toBool, fromBool)
import Foreign.C.Types

data CFBoolean
newtype Boolean = Boolean { unBoolean :: Ref CFBoolean }
{#pointer CFBooleanRef -> CFBoolean#}
instance CF Boolean where
  type Repr Boolean = CFBoolean
  wrap = Boolean
  unwrap = unBoolean
instance CFConcrete Boolean where
  type Hs Boolean = Bool
  toHs o =
    0 /= (U.unsafePerformIO $ withObject o {#call unsafe CFBooleanGetValue as ^#})

  fromHs True = kTrue
  fromHs False = kFalse

  staticType _ = TypeID {#call pure unsafe CFBooleanGetTypeID as ^ #}

toBool :: Boolean -> Bool
toBool = toHs

fromBool :: Bool -> Boolean
fromBool = fromHs

kTrue :: Boolean
kTrue = constant {#call pure unsafe hsTrue#}

kFalse :: Boolean
kFalse = constant {#call pure unsafe hsFalse#}
