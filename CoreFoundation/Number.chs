module CoreFoundation.Number(
  HsNumber,
  Number,
  CFNumber,
  toNumber,
  fromNumber,
  ) where

#include <CoreFoundation/CFNumber.h>
#include "cbits.h"
import CoreFoundation.Base
import System.IO.Unsafe as U
import Control.Monad

import Data.Int
import Foreign hiding (toBool, fromBool)
import Foreign.C.Types

data HsNumber
 = I !Int64
 | D !Double

data CFNumber
newtype Number = Number { unNumber :: Ref CFNumber }
{#pointer CFNumberRef -> CFNumber#}

type NumberType = {#type CFNumberType#}

float64Type, int64Type :: NumberType
float64Type = {#call pure unsafe hsFloat64Type#}
int64Type = {#call pure unsafe hsInt64Type#}

instance CF Number where
  type Repr Number = CFNumber
  wrap = Number
  unwrap = unNumber
instance CFConcrete Number where
  type Hs Number = HsNumber
  toHs o =
    U.unsafePerformIO $
    withObject o $ \p -> do
      isFloat <- {#call unsafe CFNumberIsFloatType as ^ #} p
      case isFloat /= 0 of
        True -> alloca $ \pres -> do
          success <- {#call unsafe CFNumberGetValue as ^ #} p float64Type (castPtr pres)
          when (success == 0) $ error "CoreFoundation.Number.toHs: conversion unexpectedly resulted in loss of precision"
          val <- peek pres
          return (D val)
        False -> alloca $ \pres -> do
          success <- {#call unsafe CFNumberGetValue as ^ #} p int64Type (castPtr pres)
          when (success == 0) $ error "CoreFoundation.Number.toHs: conversion unexpectedly resulted in loss of precision"
          val <- peek pres
          return (I val)

  fromHs (D d) = createWith d float64Type
  fromHs (I i) = createWith i int64Type

  staticType _ = TypeID {#call pure unsafe CFNumberGetTypeID as ^ #}



createWith n nty =
    U.unsafePerformIO $
    with n $ \np ->
    create $
    {#call unsafe CFNumberCreate as ^ #}
      nullPtr
      nty
      (castPtr np)

toNumber :: Number -> HsNumber
toNumber = toHs

fromNumber :: HsNumber -> Number
fromNumber = fromHs