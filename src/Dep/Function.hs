{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fcontext-stack=1024 #-}
--{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, OverlappingInstances, UndecidableInstances, FlexibleContexts #-}

module Dep.Function
    ( mnToHead
    , mnFromHead
    , mnSwap
    , mnApplyListSeries
    , mnZipWith
    ) where
    {-
    ( NToHead(..)
    , NFromHead(..)
    , NSwap(..)
    -- , zipWithn
    ) -]where-}

import Control.Applicative
import Dep.Data.Nat

class (MNat n) => NToHead n f f' | n f -> f' where
    nToHead :: n -> f -> f'

instance NToHead MZero (a -> b) (a -> b) where
    nToHead MZero f = f

instance (MNat n, NToHead n f (b -> c)) => NToHead (MSucc n) (a -> f) (b -> a -> c) where
    nToHead (MSucc n) f = \b a -> nToHead n (f a) b


class (MNat n) => NFromHead n f f' | n f -> f' where
    nFromHead :: n -> f -> f'

instance NFromHead MZero (a -> b) (a -> b) where
    nFromHead MZero f = f

instance (MNat n, NFromHead n (a -> f) c) => NFromHead (MSucc n) (a -> b -> f) (b -> c) where
    nFromHead (MSucc n) f = \b -> nFromHead n (\a -> f a b)


class (MNat n) => NSwap n f f' | n f -> f' where
    nSwap :: n -> f -> f'

instance NSwap MZero (a -> b) (a -> b) where
    nSwap MZero f = f

instance (MNat n, NFromHead (MSucc n) f f', NToHead n f' f'') => NSwap (MSucc n) f f'' where
    nSwap sn@(MSucc n) = nToHead n . nFromHead sn


class (MNat n) => NApplyListSeries n s t | n s -> t where
    nApplyListSeries :: n -> s -> t

instance NApplyListSeries MZero [a] [a] where
    nApplyListSeries MZero = id

instance (MNat n, NApplyListSeries n [b] [b]) => NApplyListSeries (MSucc n) [(a -> b)] ([a] -> [b]) where
    nApplyListSeries (MSucc n) (f:fs) (a:as) = f a : nApplyListSeries n (getZipList $ ZipList fs <*> ZipList as)
    nApplyListSeries _         _      _      = []


class (MNat n) => NZipWith n f f' | n f -> f' where
    nZipWith :: n -> f -> f'

instance (MNat n, NApplyListSeries n [f] f') => NZipWith n f f' where
    nZipWith n f = nApplyListSeries n (repeat f)

--- Closed declarations ---

mnToHead :: (NToHead n f f') => n -> f -> f'
mnToHead = nToHead

mnFromHead :: (NFromHead n f f') => n -> f -> f'
mnFromHead = nFromHead

mnSwap :: (NSwap n f f') => n -> f -> f'
mnSwap = nSwap

mnApplyListSeries :: (NApplyListSeries n s t) => n -> s -> t
mnApplyListSeries = nApplyListSeries

mnZipWith :: (NZipWith n f f') => n -> f -> f'
mnZipWith = nZipWith

{-
-- | TODO
--
-- 'Zero' corresponds with  'map', 1 corresponds with 'zipWith', and 2 corresponds with 'zipWith3'
class (Nat n) => NZipWith n f f' | n f -> f' where
    nZipWith :: n -> f -> f'

instance NZipWith Zero (a -> b) ([a] -> [b]) where
{-
    nZipWith Zero _ []     = []
    nZipWith Zero f (x:xs) = (f x) : nZipWith Zero f xs
-}
    nZipWith Zero = fmap

{-
instance NZipWith (Succ Zero) (a -> b -> c) ([a] -> [b] -> [c]) where
    nZipWith (Succ Zero) f (a:as) (b:bs) = (f a b) : nZipWith (Succ Zero) as bs
-}

--instance NZipWith (Succ Zero) (a -> (b -> c)) ([a] -> ([b] -> [c]) where
instance (Nat n, NZipWith n f f') => NZipWith (Succ n) (a -> f) ([a] -> f') where
    nZipWith (Succ n) f []     = undefined
    nZipWith (Succ n) f (a:as) = undefined
    nZipWith (Succ n) f as = f <$> ZipWith as <*> 
    -}
