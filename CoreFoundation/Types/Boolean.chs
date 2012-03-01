-- | See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFBooleanRef/Reference/reference.html#>
module CoreFoundation.Types.Boolean(
  Boolean,
  CFBoolean,
  toBool,
  fromBool,
  ) where

#include <CoreFoundation/CFDate.h>
#include "cbits.h"

import CoreFoundation.Types.Base
import System.IO.Unsafe as U

import Foreign hiding (toBool, fromBool)
import Foreign.C.Types

import Data.Typeable
import Control.DeepSeq

-- | Opaque type @CFBoolean@
data CFBoolean
{- |
Wrapper for @CFBooleanRef@
-}
newtype Boolean = Boolean { unBoolean :: Ref CFBoolean }
  deriving(Typeable)
{#pointer CFBooleanRef -> CFBoolean#}
instance CF Boolean where
  type Repr Boolean = CFBoolean
  wrap = Boolean
  unwrap = unBoolean

type instance UnHs Bool = Boolean
instance CFConcrete Boolean where
  type Hs Boolean = Bool
  toHs o =
    0 /= (U.unsafePerformIO $ withObject o {#call unsafe CFBooleanGetValue as ^#})

  fromHs True = kTrue
  fromHs False = kFalse

  staticType _ = TypeID {#call pure unsafe CFBooleanGetTypeID as ^ #}

-- | Synonym for 'toHs'
toBool :: Boolean -> Bool
toBool = toHs

-- | Synonym for 'fromHs'
fromBool :: Bool -> Boolean
fromBool = fromHs

kTrue :: Boolean
kTrue = constant {#call pure unsafe hsTrue#}

kFalse :: Boolean
kFalse = constant {#call pure unsafe hsFalse#}

instance Show Boolean where
  show = show . toHs
instance Eq Boolean where
  a == b = toHs a == toHs b
instance Ord Boolean where
  compare a b = compare (toHs a) (toHs b)
instance NFData Boolean
