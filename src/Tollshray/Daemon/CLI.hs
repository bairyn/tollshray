{-# LANGUAGE Haskell2010 #-}

-- https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Tollshray.Daemon.CLI
	(
		main
	) where

-- bytestring
import qualified Data.ByteString as B

-- template-haskell
import qualified Language.Haskell.TH as TH

-- language-rust
--import qualified Language.Rust.Syntax as R

-- primes
--import qualified Data.Numbers.Primes as R

-- language-lua
import qualified Language.Lua.Syntax as L

-- | Placeholder.
main :: IO ()
main = do
	const (return ()) $ B.empty
	const (return ()) $ TH.location
	--const (return ()) $ (take 10 R.primes  :: [Integer])
	const (return ()) $ (L.Unop L.Neg)
	putStr "Placeholder 3.\n"
