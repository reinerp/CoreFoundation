-- | See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFDateRef/Reference/reference.html>
module CoreFoundation.Types.Date(
  Date,
  CFDate,
  toUTCTime,
  fromUTCTime,
  ) where

#include <CoreFoundation/CFDate.h>

import qualified System.IO.Unsafe as U
import Data.Time
import Foreign
import Foreign.C.Types
import Data.Typeable
import Control.DeepSeq
import CoreFoundation.Types.Base

-- | CoreFoundation @CFDate@ type.
data CFDate

{- |
Wraps the @CFDateRef@ type.
-}
newtype Date = Date { unDate :: Ref CFDate }
 deriving(Typeable)
{#pointer CFDateRef -> CFDate#}
instance CF Date where
  type Repr Date = CFDate
  wrap = Date
  unwrap = unDate

type instance UnHs UTCTime = Date
instance CFConcrete Date where
  type Hs Date = UTCTime
  toHs o =
    realToFrac 
      (U.unsafePerformIO $ withObject o {#call unsafe CFDateGetAbsoluteTime as ^ #})
    `addUTCTime` appleEpoch
  fromHs t =
    U.unsafePerformIO $
    create $
    {#call unsafe CFDateCreate as ^#}
      nullPtr 
      (realToFrac (t `diffUTCTime` appleEpoch))
  staticType _ = TypeID {#call pure unsafe CFDateGetTypeID as ^ #}

-- | Synonym for 'toHs'
toUTCTime :: Date -> UTCTime
toUTCTime = toHs

-- | Synonym for 'fromHs'
fromUTCTime :: UTCTime -> Date
fromUTCTime = fromHs

appleEpoch :: UTCTime
appleEpoch = 
  UTCTime{
    utctDay = fromGregorian 2001 1 1,
    utctDayTime = 0
    }

instance Show Date where
  show = show . toHs
instance Eq Date where
  a == b = toHs a == toHs b
instance Ord Date where
  compare a b = compare (toHs a) (toHs b)
instance NFData Date