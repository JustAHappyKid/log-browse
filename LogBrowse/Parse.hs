module LogBrowse.Parse (lineToParts) where

import Data.String.Utils (startswith, split)
import Safe (tailSafe)

lineToParts :: String -> [String]
lineToParts ln = map (\p -> if startswith "[" p then tailSafe p else p) $ split "] " ln
