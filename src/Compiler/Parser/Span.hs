module Compiler.Parser.Span (Span, Spanned) where

-- Spans contains the absolute position of the start and end of an element
-- If something does not originate from the source file then it will be an `EmptySpan`
data Span
    = Span
        { start :: Int
        , end :: Int
        , inner :: [Span]
        }
    | EmptySpan
    deriving (Eq, Show)

mergeSpans :: Span -> Span -> Span
mergeSpans (Span aStart aEnd aInner) (Span bStart bEnd bInner) = Span aStart bEnd [aInner bInner]
mergeSpans s EmptySpan = s
mergeSpans EmptySpan s = s
mergeSpans EmptySpan EmptySpan = EmptySpan

instance Semigroup Span where
    (<>) = mergeSpans

instance Monoid Span where
    mempty = EmptySpan

data Spanned a = Spanned a Span deriving (Eq, Show, Functor)

instance Applicative Spanned a where
    liftA2 f (Spanned a aSpan) (Spanned b bSpan) = Spanned (f a b) (aSpan <> bSpan)
    pure a = Spanned a EmptySpan
