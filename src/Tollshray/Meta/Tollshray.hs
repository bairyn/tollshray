{-# LANGUAGE Haskell2010 #-}

-- https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-# LANGUAGE GADTs #-}

module Tollshray.Meta.Tollshray
	(
		version
	) where

import Prelude

import qualified Data.Version V

-- Note: be sure the version is consistent in the 3 places it appears:
-- 	- `tollshray.cabal`
-- 	- `CHANGELOG.md`
-- 	- `src/Tollshray/Meta/Tollshray.hs`
-- Try to let only commit have each version.

-- | 0.1.1.0-dev
version :: V.Version
version = V.Version versionBranch' versionTags
	where
		versionBranch' :: [Integer]
		versionBranch' = map fromIntegral versionBranch
		versionBranch :: [Int]
		versionBranch = [0, 1, 1, 0]
		-- On a particular release commit, make this ‘[]’ rather than
		-- ‘["dev"]’.
		versionTags :: [String]
		versionTags = ["dev"]
