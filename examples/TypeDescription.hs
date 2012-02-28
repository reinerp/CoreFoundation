import qualified Data.Text.IO as Text

import CoreFoundation.Type
import CoreFoundation.String

main = Text.putStrLn (typeIDDescription stringTypeID)