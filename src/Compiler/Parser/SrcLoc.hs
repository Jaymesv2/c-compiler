module Compiler.Parser.SrcLoc (SrcLoc (..), UnhelpfulSpanReason (..), SrcSpan (..), GenLocated (..), Located) where

import qualified Data.Text as T


-- Spans contains the absolute position of the start and end of an element
-- If something does not originate from the source file then it will be an `EmptySpan`


-- human usable source locations, references 
data SrcLoc = 
    SrcLoc {
        srcLocFile :: {-# UNPACK #-} !String,
        srcLocLine :: {-# UNPACK #-} !Int,
        srcLocCol :: {-# UNPACK #-} !Int
    }
    deriving stock (Eq,Show)

-- kinda copied this design from GHC
data SrcSpan = 
    SrcSpan {
        srcLocFile :: !String,
        srcSpanSLine :: {-# UNPACK #-} !Int,
        srcSpanSCol :: {-# UNPACK #-} !Int,
        srcSpanELine :: {-# UNPACK #-} !Int,
        srcSpanECol :: {-# UNPACK #-} !Int
    }
    | UnhelpfulSpan UnhelpfulSpanReason
    deriving stock (Eq, Show)

data BufPos = BufPos {
    
    }


data UnhelpfulSpanReason 
    = UnhelpfulGenerated
    | UnhelpfulNoLocation
    | UnhelpfulOther !String
    deriving stock (Eq,Show)


mergeSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
mergeSrcSpans (SrcSpan fname sLine1 sCol1 eLine1 eCol1) (SrcSpan _ sLine2 sCol2 eLine2 eCol2) = SrcSpan fname sLine1 sCol1 eLine2 eCol2
mergeSrcSpans s (UnhelpfulSpan _) = s
mergeSrcSpans (UnhelpfulSpan _) s = s

instance Semigroup SrcSpan where
    (<>) = mergeSrcSpans

instance Monoid SrcSpan where
    mempty = UnhelpfulSpan UnhelpfulGenerated



-- I just took this from GHC because it seems like a better way to do it
data GenLocated l e = L l e
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type Located = GenLocated SrcSpan
--type RealLocated = GenLocated RealSrcSpan



--data Located  a = L Span a deriving stock (Eq, Show, Functor)

instance Applicative Located where
    liftA2 f (L aSpan a) (L bSpan b) = L (aSpan <> bSpan) (f a b)
    pure = L (UnhelpfulSpan UnhelpfulNoLocation)
