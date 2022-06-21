{-# LANGUAGE Haskell2010 #-}

-- https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-
module Tollshray
	(
		main
	) where
-}
module Main where

import Tollshray.Tests.All as T

main :: IO ()
main = T.main
