{-# LANGUAGE OverloadedStrings #-}
import CoreFoundation.String
import qualified Data.Text.IO as Text

main = do
  s1 <- fromText "Grüße von Haskell"
  s2 <- toText s1
  Text.putStrLn s2
