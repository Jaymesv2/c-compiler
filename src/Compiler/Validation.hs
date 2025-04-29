
module Validation where 

-- acts like either and collects non fatal warnings
data Validation w e a
    = Ok 
        { val :: a
        , warnings :: w }
    | Fail 
        { err :: e
        , warnings :: w }

instance Functor (Validation w e) where
    fmap f r@Ok{..} = r{val=f val}
    fmap _ s@Fail{err,warnings} = Fail{err,warnings}

instance (Semigroup w, Semigroup e, Semigroup a) => Semigroup (Validation w e a) where
    Ok{val=val1,warnings=w1} <> Ok{val=val2,warnings=w2} = Ok{val=val1<>val2,warnings=w1<>w2}
    Ok{warnings=w1} <> Fail{err,warnings=w2} = Fail{err,warnings=w1<>w2}
    Fail{err,warnings=w1}<>Ok{warnings=w2} = Fail{err,warnings=w1<>w2}
    Fail{err=e1,warnings=w1} <> Fail{err=e2,warnings=w2} = Fail{err=e1<>e2, warnings=w1<>w2}

instance (Monoid w, Monoid e, Semigroup a) => Monoid (Validation w e a) where
    mempty = Fail{err=mempty,warnings=mempty}


instance (Monoid w, Semigroup e) => Applicative (Validation w e) where
    liftA2 f Ok{val=val1,warnings=w1} Ok{val=val2,warnings=w2} = Ok{val=f val1 val2,warnings=w1<>w2}
    liftA2 _ Ok{warnings=w1} Fail{err,warnings=w2} = Fail{err,warnings=w1<>w2}
    liftA2 _ Fail{err,warnings=w1} Ok{warnings=w2} = Fail{err,warnings=w1<>w2}
    liftA2 _ Fail{err=e1,warnings=w1} Fail{err=e2,warnings=w2} = Fail{err=e1<>e2, warnings=w1<>w2}
    pure val = Ok{val,warnings=mempty} 

    
{- instance (Monoid w, Monoid e) => Alternative (Validation w e) where
    empty = Fail{err=mempty,warnings=mempty}

    Ok{val,warnings=w1} <|> Ok{warnings=w2} = Ok{val,warnings=w1<>w2}
    Ok{warnings=w1} <|> Fail{err,warnings=w2} = Ok{val,}
    Fail{err,warnings=w1}<|>Ok{warnings=w2} = 
    Fail{err=e1,warnings=w1} <|> Fail{err=e2,warnings=w2} = Fail{err=e1<>e2, warnings=w1<>w2} -}


data ValidationReader r w e a
    = OkR 
        { val :: a
        , warnings :: w
        , env :: r }
    | FailR
        { err :: e
        , warnings :: w }

