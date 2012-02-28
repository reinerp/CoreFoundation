{-# LANGUAGE OverloadedStrings #-}
import CoreFoundation.Type
import CoreFoundation.String
import qualified Data.Text.IO as Text

main = do
  s <- fromText "Grüße von Haskell"
  descr <- description s
  Text.putStrLn descr
