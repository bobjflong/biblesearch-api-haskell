### Introduction

Currently supported resources: `Versions`, `Books`, `Chapters`, `Versions`

Provides a lens interface to the bibles.org API.

Examples presume an active ghci session. **In examples I may use unsafe functions such as `head` and `fromJust`. You should avoid their use in production code**.

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

Fetch Books in a Bible version

```haskell
> :set -XOverloadedStrings
> :m Bible.Books

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

### Chapters

Fetch Chapters in a Book

```haskell
> :set -XOverloadedStrings
> :m Bible.Chapters

> import Control.Lens
> import Data.Maybe

> chapterResult <- listChapters "eng-GNTD:2Tim" "YOUR-API-KEY"

> let chapterOne = (head.fromJust) chapterResult

> chapterOne ^. chapter
ChapterNumber 1

> chapterOne ^. chapterId
"eng-GNTD:2Tim.1"

> chapterOne ^. osisEnd
"eng-GNTD:2Tim.1.18"
```

### Verses

Fetch Verses in a Chapter

```haskell
> :set -XOverloadedStrings
> :m Bible.Verses

> import Control.Lens
> import Data.Maybe

> verses <- listVerses "eng-KJV:Matt.10" "YOUR-API-KEY"

> let verse = head $ filter (\v -> v ^. reference == "Matthew 10:16") (fromJust verses)

> verse ^. reference
"Matthew 10:16"

> verse ^. text
"<p class=\"p\"><sup id=\"Matt.10.16\" class=\"v\">16</sup>
  Behold, I send you forth as sheep in the midst of wolves: be ye therefore wise as serpents, and harmless as doves.
</p>"

> verse ^. verseId
"eng-KJV:Matt.10.16"

> verse ^. osisEnd
"eng-KJV:Matt.10.16"
```
