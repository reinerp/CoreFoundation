{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module CoreFoundation.Date(
  Date,
  CFDate,
  toUTCTime,
  fromUTCTime,
  ) where

#include <CoreFoundation/CFDate.h>

import Control.Applicative

import qualified System.IO.Unsafe as U
import Data.Time
import Foreign
import Foreign.C.Types

import CoreFoundation.Base

data CFDate
newtype Date = Date { unDate :: Ref CFDate }
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

toUTCTime :: Date -> UTCTime
toUTCTime = toHs

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
