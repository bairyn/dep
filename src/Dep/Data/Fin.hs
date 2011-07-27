{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fcontext-stack=1024 #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Dep.Data.Fin
{-
    ( MFin
    , MFZero(..)
    , FSucc(..)
    , mfToNat
    ) where
-} where

import Dep.Data.Nat

class (MNat n) => Fin n fn
instance (MNat n) => Fin (MSucc n) MFZero
--instance (MNat n, Fin n fn) => Fin (MSucc n) (MFSucc (MSucc n) fn)
instance (MNat n, Fin n fn) => Fin (MSucc n) (MFSucc n fn)

data MFZero = MFZero
data (MNat n, Fin n fn) => MFSucc n fn = MFSucc fn

{-
class (Fin n fn, MNat n2) => FToNat fn n2 | fn -> n2 where
    fToNat :: fn -> n2

instance (MNat n) => FToNat n MFZero MZero where
    fToNat MFZero = MZero

--instance (MNat n, Fin n fn, Fin (MSucc n) (MFSucc n fn), FToNat n fn n2) => FToNat (MSucc n) (MFSucc n fn) (MSucc n2) where
    --fToNat (MFSucc n fn) = MSucc (fToNat n fn)
--instance (MNat n, Fin n fn, Fin (MSucc n) (MFSucc n fn), FToNat n fn n2) => FToNat (MSucc n) (MFSucc (MSucc n) fn) (MSucc n2) where
    --fToNat (MFSucc (MSucc n) fn) = MSucc (fToNat n fn)
-}


{-
--class (Nat n, Fin n fn, MNat n2) => FToNat fn n2 | fn -> n2 where
--class (MNat n, Fin n fn, MNat n2) => FToNat fn n2 | fn -> n2 where
class (MNat n, Fin n fn, MNat n2) => FToNat n fn n2 | n fn -> n2 where
    fToNat :: fn -> n2

instance (MNat n) => FToNat n MFZero MZero where
    fToNat MFZero = MZero

--instance (MNat n, Fin n fn, Fin (MSucc n) (MFSucc n fn), FToNat n fn n2) => FToNat (MSucc n) (MFSucc n fn) (MSucc n2) where
    --fToNat (MFSucc n fn) = MSucc (fToNat n fn)
--instance (MNat n, Fin n fn, Fin (MSucc n) (MFSucc n fn), FToNat n fn n2) => FToNat (MSucc n) (MFSucc (MSucc n) fn) (MSucc n2) where
    --fToNat (MFSucc (MSucc n) fn) = MSucc (fToNat n fn)

--- Closed declarations ---

-}

class (Fin n fn) => MFin n fn
instance (Fin n fn) => MFin n fn

{-
mfToNat :: (FToNat n fn n2) => n -> fn -> n2
mfToNat _ = fToNat
-}
