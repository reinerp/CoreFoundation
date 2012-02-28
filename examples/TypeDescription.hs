import qualified Data.Text.IO as Text

import CoreFoundation.Type
import CoreFoundation.String

main = print $ staticType (undefined :: Proxy CFString)