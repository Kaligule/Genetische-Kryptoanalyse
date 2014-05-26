module WordListModule (wordlist) where

import NormalizeLanguageModule (normalizeLanguage)
import Data.List (nub)

--TODO: Find good Sources
wordlist :: [String]
wordlist = (nub . map normalizeLanguage)
			[ "Die"
			, "Sandra"
			, "ist"
			, "die"
			, "Sandra"
			, "ist"
			, "ne"
			, "Maus"
			]

