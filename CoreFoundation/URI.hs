{- |
CoreFoundation tends to store filepaths as URIs rather than in POSIX
format. The functions 'uriToFilepath' and 'filepathToUri' provide the
appropriate conversions.
-}
module CoreFoundation.URI( 
  Uri,
  uriToFilepath,
  filepathToUri,
 ) where

import qualified Prelude
import           Prelude hiding(String)

import CoreFoundation.Types.String

import Control.Monad
import Network.URI
import System.FilePath

-- | CoreFoundation strings which are formatted as uris
type Uri = String

fileScheme = "file:"
fileAuth = Just $ URIAuth "" "localhost" ""
fileQuery = ""
fileFragment = ""

{- |

>>> uriToFilepath "file://localhost/path/to/foo%20bar" 
Just "/path/to/foo bar"

>>> uriToFilePath "malformed..."
Nothing

-}
uriToFilepath :: Uri -> Maybe FilePath
uriToFilepath str = do
  uri <- parseURI $ toString str
  guard $ uriScheme uri == fileScheme
  guard $ uriAuthority uri == fileAuth
  guard $ uriQuery uri == fileQuery
  guard $ uriFragment uri == fileFragment
  return $ unEscapeString $ uriPath uri

{- |

>>> filepathToUri "/path/to/foo bar"
"file://localhost/path/to/foo%20bar"

>>> filepathToUri "path/to/foo"
error: input path must be absolute

-}
filepathToUri :: FilePath -> Uri
filepathToUri fp 
  | not (isAbsolute fp) = error "CoreFoundation.Utils.filepathToUri: input path must be absolute"
  | otherwise = fromString $ uriToString id uri ""
  where                    
    uri = 
      URI {
        uriScheme = fileScheme,
        uriAuthority = fileAuth,
        uriPath = escapeURIString isUnescapedInURI fp,
        uriQuery = fileQuery,
        uriFragment = fileFragment
        }
