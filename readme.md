Provides a lens interface to the bibles.org API.

Examples presume an active ghci session.

### Versions

Fetch available Bible versions.

```haskell
> :set -XOverloadedStrings
> :m Bible.Versions

> import Control.Lens
> import Data.Maybe

> versionResult <- listVersions "YOUR-API-KEY"

> let versions = fromJust versionResult -- don't use fromJust outside of testing...

> let kjv = head $ filter (\x -> x ^. name == "King James Version") versions

> kjv ^. versionId
"eng-KJV"

> kjv ^. lang
"eng-GB"

> kjv ^. langCode
"ISO 639-3"

> kjv ^. contactUrl 
"http://www.biblesociety.org.uk/"

> kjv ^. name
"King James Version"
```
