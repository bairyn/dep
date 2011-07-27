{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fcontext-stack=1024 #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Dep.Data.Nat
    ( MNat
    , MZero(..)
    , MSucc(..)
    , mnToInteger
    , mnPlus
    , mnTimes

    , M0, m0
    , M1, m1
    , M2, m2
    , M3, m3
    , M4, m4
    , M5, m5
    ) where
    {-
    ( MNat
    , MZero(..)
    , MSucc(..)
    , NToInteger(..)
    -- , NFromInteger(..)
    , NPlus(..)
    , NTimes(..)
    ) where
    -}

import Text.Printf

class Nat n
instance Nat MZero
instance Nat (MSucc n)

data MZero = MZero
data (Nat n) => MSucc n = MSucc n

instance Show MZero where
    show = printf "%d" . nToInteger
instance (Nat n, NToInteger n) => Show (MSucc n) where
    show = printf "%d" . nToInteger

class (Nat n) => NToInteger n where
    nToInteger :: n -> Integer

instance NToInteger MZero where
    nToInteger MZero = 0

instance (Nat n, NToInteger n) => NToInteger (MSucc n) where
    nToInteger (MSucc n) = succ (nToInteger n)

class (Nat a, Nat b, Nat c) => NPlus a b c | a b -> c where
    nPlus :: a -> b -> c

instance (Nat b) => NPlus MZero b b where
    nPlus MZero b = b

instance (Nat a', Nat b, NPlus a' b c) => NPlus (MSucc a') b (MSucc c) where
    nPlus (MSucc a') b = MSucc (a' `nPlus` b)

class (Nat a, Nat b, Nat c) => NTimes a b c | a b -> c where
    nTimes :: a -> b -> c

instance (Nat b) => NTimes MZero b MZero where
    nTimes MZero _ = MZero

instance (Nat a', Nat b, NTimes a' b c, NPlus b c d) => NTimes (MSucc a') b d where
    nTimes (MSucc a') b = nPlus b (nTimes a' b)

--- Closed declarations ---

class (Nat n) => MNat n
instance (Nat n) => MNat n

mnToInteger :: (NToInteger n) => n -> Integer
mnToInteger = nToInteger

mnPlus :: (NPlus a b c) => a -> b -> c
mnPlus = nPlus

mnTimes :: (NTimes a b c) => a -> b -> c
mnTimes = nTimes


type M0 = MZero
m0 :: M0
m0 = MZero

type M1 = MSucc M0
m1 :: M1
m1 = MSucc m0

type M2 = MSucc M1
m2 :: M2
m2 = MSucc m1

type M3 = MSucc M2
m3 :: M3
m3 = MSucc m2

type M4 = MSucc M3
m4 :: M4
m4 = MSucc m3

type M5 = MSucc M4
m5 :: M5
m5 = MSucc m4
