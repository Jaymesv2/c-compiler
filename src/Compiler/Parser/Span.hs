module Compiler.Parser.Span (Span (..), Spanned (..)) where

import qualified Data.Text as T


-- Spans contains the absolute position of the start and end of an element
-- If something does not originate from the source file then it will be an `EmptySpan`
data Span
    = Span
        { filename :: !T.Text
        , start :: !(Int, Int, Int) -- abs pos, line, col
        , end :: !(Int, Int, Int)
        , inner :: [Span]
        }
    | EmptySpan
    deriving stock (Eq, Show)



mergeSpans :: Span -> Span -> Span
mergeSpans (Span fname aStart aEnd aInner) (Span fname2 bStart bEnd bInner) = Span fname aStart bEnd (aInner ++ bInner)
mergeSpans s EmptySpan = s
mergeSpans EmptySpan s = s

instance Semigroup Span where
    (<>) = mergeSpans

instance Monoid Span where
    mempty = EmptySpan

data Spanned a = Spanned Span a deriving stock (Eq, Show, Functor)

instance Applicative Spanned where
    liftA2 f (Spanned aSpan a) (Spanned bSpan b) = Spanned (aSpan <> bSpan) (f a b) 
    pure = Spanned EmptySpan
