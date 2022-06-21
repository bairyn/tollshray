{-# LANGUAGE Haskell2010 #-}

-- https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Tollshray.Control.Evaluator
	(
		--version
	) where

import Prelude

-- | FRP: A time-varying function modeled as a self-modifying value.
-- TODO

-- | Composable APIs: messages that time-varying behaviours understand and can
-- respond to.  This is like a composable collection of method declarations in
-- a C++ header file.  Inheritance may be analogized as just another GADT
-- nested struct, but with implicit namespacing to avoid e.g. an extra
-- ‘.my_impl’ layer.
--
-- See also that blog post that showed how type classes like the Monad can be
-- represented as a regular GADT.
--
-- What's cool is this sort of establishes a bridge between FP and OOP: in FRP,
-- you can model time-varying functions as self-modifying functions.  This
-- permits hidden local state, for one.  FRP behaviors can through a composable
-- message API-lke interface, and more.
--
-- Constructors simply instantiate new behaviors, which can implement APIs,
-- like implementing (providing definitions for) C++ header class declarations.
-- Constructors create API implementations and provide the initial behaviour.
-- A constructor ultimately provides the API implementation (like definitions)
-- by producing a behaviour from an API input (symbol and arguments) to an
-- output with a type specified by the API.
--
-- Then you can even use other behaviours (actually, constructors) as blueprint
-- templates that you ca nuse for default implementations with your own manual
-- overrides, although you will likely need knowledge of the internal
-- implementation.
-- TODO

-- | Linear type model: mutate, dup, drop.
-- TODO
