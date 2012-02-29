module CoreFoundation.Array.Internal where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM

import CoreFoundation.Base
import CoreFoundation.Touch
import Foreign

import Control.Exception

withVector :: CF a => V.Vector a -> (Ptr (Ptr (Repr a)) -> Int -> IO b) -> IO b
withVector v f =
  (S.unsafeWith (V.convert (V.map extractPtr v)) $ \buf -> f buf (V.length v))
  `finally` touch v

buildVector :: CF a => Int -> (Ptr (Ptr (Repr a)) -> IO b) -> IO (V.Vector a, b)
buildVector len f = do
  mvec <- SM.new (fromIntegral len)
  res <- SM.unsafeWith mvec $ \ptr -> f ptr
  vec <- S.unsafeFreeze mvec
  vec' <- V.mapM (get . return) $ S.convert vec
  return (vec', res)
