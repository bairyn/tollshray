{-# LANGUAGE Haskell2010 #-}

-- https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-
module BenchTollshray
	(
		main
	) where
-}
module Main where

import Tollshray.Bench.All as B

main :: IO ()
main = B.main
