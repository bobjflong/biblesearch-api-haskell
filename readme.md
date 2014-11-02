### Introduction

*WIP - adding resources as I have time*

Provides a lens interface to the bibles.org API.

Examples presume an active ghci session. _In examples I may use unsafe functions such as `head` and `fromJust`. You should avoid their use in production code_.

### Versions

Fetch available Bible versions.

```haskell
> :set -XOverloadedStrings
> :m Bible.Versions

> import Control.Lens
> import Data.Maybe

> versionResult <- listVersions "YOUR-API-KEY"

> let versions = fromJust versionResult

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

### Books

Fetch books is a Bible version

```haskell
> :set -XOverloadedStrings
> :m Bible.Versions

> import Control.Lens
> import Data.Maybe

> bookResult <- listBooks "eng-KJV" "YOUR-API-KEY"

> let genesis = (head.fromJust) bookResult

> genesis ^. name
"Genesis"

> genesis ^. bookId
"eng-KJV:Gen"

> genesis ^. osisEnd
"eng-KJV:Gen.50.26"

> genesis ^. testament
OldTestament
```
