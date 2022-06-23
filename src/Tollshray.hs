{-# LANGUAGE Haskell2010 #-}

-- https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-# LANGUAGE GADTs #-}

{-
module Tollshray
	(
		main
	) where
-}
module Main where

import Tollshray.Daemon.CLI as T

main :: IO ()
main = T.main
