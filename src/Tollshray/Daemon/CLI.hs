{-# LANGUAGE Haskell2010 #-}

-- https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Tollshray.Daemon.CLI
	(
		main
	) where

import qualified Data.ByteString as B

-- | Placeholder.
main :: IO ()
main = do
	const (return ()) $ B.empty
	putStr "Placeholder 2.\n"
