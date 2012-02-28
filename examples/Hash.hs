{-# LANGUAGE OverloadedStrings #-}
import CoreFoundation.Type
import CoreFoundation.String
import qualified Data.Text.IO as Text

main = do
  s <- fromText "Grüße von Haskell"
  code <- hash s
  print code
