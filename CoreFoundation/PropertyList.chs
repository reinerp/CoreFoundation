-- See http://www.declaresub.com/ideclare/CoreFoundation/12.html

module CoreFoundation.PropertyList(
  -- * Basic interface
  Plist,
  CFPropertyList,
  PlistClass,
  toPlist,
  fromPlist,
  viewPlist,
  -- * 'Data.PropertyList' compat
  toPropertyList,
  fromPropertyList,
  ) where

-- get header parse errors otherwise
#define __BLOCKS__ 0
#include <CoreFoundation/CFPropertyList.h>

import Prelude hiding(String)
import qualified Prelude

import CoreFoundation.Base
import CoreFoundation.String
import CoreFoundation.Number
import CoreFoundation.Boolean
import CoreFoundation.Date
import CoreFoundation.Data
import CoreFoundation.Array
import CoreFoundation.Dictionary

import Control.Arrow((***))
import Control.Applicative
import qualified Data.Vector as V
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.Text as T

import qualified Data.PropertyList as PL
import           Data.PropertyList.Algebra hiding(toPlist, fromPlist)
import qualified Data.PropertyList.Algebra as PL

import Foreign hiding(fromBool)

data CFPropertyList
newtype Plist = Plist { unPlist :: Ref CFPropertyList }
{#pointer CFPropertyListRef -> CFPropertyList#}
instance CF Plist where
  type Repr Plist = CFPropertyList
  wrap = Plist
  unwrap = unPlist

class CFConcrete a => PlistClass a
instance PlistClass String
instance PlistClass Number
instance PlistClass Boolean
instance PlistClass Date
instance PlistClass Data
instance PlistClass (Array Plist)
instance PlistClass (Dictionary String Plist)

toPlist :: PlistClass a => a -> Plist
toPlist = unsafeCastCF

fromPlist :: PlistClass a => Plist -> Maybe a
fromPlist = dynamicCast . toObject

viewPlist :: Plist -> PlistView
viewPlist (fromPlist -> Just v) = String v
viewPlist (fromPlist -> Just v) = Number v
viewPlist (fromPlist -> Just v) = Boolean v
viewPlist (fromPlist -> Just v) = Date v
viewPlist (fromPlist -> Just v) = Data v
viewPlist (fromPlist -> Just v) = Array v
viewPlist (fromPlist -> Just v) = Dictionary v
viewPlist _ = error "CoreFoundation.PropertyList.viewPlist: Unexpected type in Plist"

data PlistView
 = String !String
 | Number !Number
 | Boolean !Boolean
 | Date !Date
 | Data !Data
 | Array !(Array Plist)
 | Dictionary !(Dictionary String Plist)

------------ support for Data.PropertyList
instance PListAlgebra Identity Plist where
  plistAlgebra (Identity v) = case v of
    PLArray w -> mk $ V.fromList w
    PLData w -> mk w
    PLDate w -> mk w
    PLDict w -> mk $ cvtMap w
    PLReal w -> mk $ D w
    PLInt w -> mk $ I $ fromIntegral w
    PLString w -> mk $ T.pack w
    PLBool w -> mk w
   where
     mk :: PlistClass a => Hs a -> Plist
     mk = toPlist . fromHs

cvtMap :: CF a => M.Map Prelude.String a -> (V.Vector String, V.Vector a)
cvtMap = (V.map fromString *** id) . V.unzip . V.fromList . M.toList

uncvtMap :: CF a => (V.Vector String, V.Vector a) -> M.Map Prelude.String a
uncvtMap = M.fromList . V.toList . uncurry V.zip . (V.map toString *** id)

plNumber (D d) = PLReal d
plNumber (I i) = PLInt (fromIntegral i)

instance Applicative f => PListCoalgebra f Plist where
  {-# SPECIALISE instance PListCoalgebra Identity Plist #-}
  plistCoalgebra v = case v of
    (fromPlist -> Just v) -> mk (PLArray . V.toList) v
    (fromPlist -> Just v) -> mk PLData v
    (fromPlist -> Just v) -> mk PLDate v
    (fromPlist -> Just v) -> mk (PLDict . uncvtMap) v
    (fromPlist -> Just v) -> mk plNumber v
    (fromPlist -> Just v) -> mk (PLString . T.unpack) v
    (fromPlist -> Just v) -> mk PLBool v
   where
    mk :: forall b a. PlistClass a => (Hs a -> PropertyListS Plist) -> a -> f (PropertyListS Plist)
    mk ctor v = pure . ctor . toHs $ v

toPropertyList :: Plist -> PL.PropertyList
toPropertyList = PL.toPlist

fromPropertyList :: PL.PropertyList -> Plist
fromPropertyList = PL.toPlistWith idId
  where
    idId :: Identity a -> Identity a
    idId = id

