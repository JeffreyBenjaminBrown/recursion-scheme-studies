module FastPretty where

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as Pretty

import Expr
import Lib


fastPretty :: RAlgebra' Expr Doc

-- All our cases, aside from the `Call` nodes in which
-- we are interested, are the same as in the pretty-printing
-- catamorphism in the previous installment. We just ignore
-- the first `Term` argument because it doesn't have anything we need
-- to look at.
fastPretty _ (Literal i) = Pretty.int i
fastPretty _ (Ident s)   = Pretty.text s
-- uninteresting cases omitted, blah blah blah

-- Here's where it gets interesting. We're going to look
-- at the first argument to determine  whether this is a
-- `Call` node with the function name (an `Ident`) named `id`.
-- If so, we'll just return the only argument provided.
fastPretty (In (Call { func = "id" }))
           (Call {args = [theArg]}) = theArg

-- Otherwise, we won't look at the first `Term` argument,
-- and just glom the name and the parenthesized and
-- comma-separated arguments together.
fastPretty _ (Call f args) = f <> Pretty.parens (mconcat ("," `Pretty.punctuate` args))

-- Straightforward ALGOL-style syntax for the remaining cases
fastPretty _ (Index it idx)  = it <> Pretty.brackets idx
fastPretty _ (Unary op it)   = Pretty.text op <> it
fastPretty _ (Binary l op r) = l <> Pretty.text op <> r
fastPretty _ (Paren ex)      = Pretty.parens ex
