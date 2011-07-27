{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fcontext-stack=1024 #-}
--{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
--{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TypeFamilies, ExplicitForAll, ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

module Dep.Data.Vec
    ( MVec
    , MVEmpty(..)
    , MVCons(..)
    , mvIndex
    , mvLength
    , mvReplicate
    --, mvStringRepresentation
    ) where

import Data.Proxy
import Data.Tagged
import Dep.Data.Fin
import Dep.Data.Nat

class (MNat n) => Vec a n v
instance Vec a MZero MVEmpty
instance (Vec a n v) => Vec a (MSucc n) (MVCons a n v)

data MVEmpty = MVEmpty
data (MNat n, Vec a n v) => MVCons a n v = MVCons a v  -- n is length of list to which the label is prepended; (MSucc n) is the length of the resultant list

instance (MNat n, Vec a n v, VStringRepresentation a n v) => Show v where
    show v = "[" ++ vStringRepresentation (Proxy :: Proxy a) (Proxy :: Proxy n) v ++ "]"

class (Vec a l v, MFin l fn) => VIndex fn l v a | fn l v -> a where
    vIndex :: fn -> l -> v -> a

instance (Vec a n v) => VIndex MFZero (MSucc n) (MVCons a n v) a where
    vIndex MFZero      _         (MVCons a _) = a

instance (Vec a n v, Fin n fn, VIndex fn n v a) => VIndex (MFSucc n fn) (MSucc n) (MVCons a n v) a where
    vIndex (MFSucc fn) (MSucc n) (MVCons _ v) = vIndex fn n v

class (Vec a n v) => VLength a v n | a v -> n where
    vLength :: Tagged a v -> n

instance (Vec a MZero MVEmpty) => VLength a MVEmpty MZero where
    vLength (Tagged MVEmpty)      = MZero

instance (MNat n, Vec a n v, VLength a v n) => VLength a (MVCons a n v) (MSucc n) where
    vLength (Tagged (MVCons _ v)) = MSucc (vLength (Tagged v  :: Tagged a v))

class (MNat n, Vec a n v) => VReplicate n a v | n a -> v where
    vReplicate :: n -> a -> v

instance VReplicate MZero a MVEmpty where
    vReplicate MZero _ = MVEmpty

instance (MNat n, Vec a n v, VReplicate n a v) => VReplicate (MSucc n) a (MVCons a n v) where
    vReplicate (MSucc n) a = MVCons a (vReplicate n a)

class (Vec a n v) => VStringRepresentation a n v where
    vStringRepresentation :: Proxy a -> Proxy n -> v -> String

instance VStringRepresentation a MZero MVEmpty where
    vStringRepresentation Proxy Proxy MVEmpty = ""

instance (Vec a MZero v, Show a, VStringRepresentation a MZero v) => VStringRepresentation a (MSucc MZero) (MVCons a MZero v) where
    vStringRepresentation p0@Proxy Proxy (MVCons a v) = show a ++ vStringRepresentation p0 (Proxy :: Proxy MZero) v

instance (MNat n, Vec a (MSucc n) v, Show a, VStringRepresentation a n v) => VStringRepresentation a (MSucc (MSucc n)) (MVCons a (MSucc n) v) where
    vStringRepresentation p0@Proxy Proxy (MVCons a v) = show a ++ "," ++ vStringRepresentation p0 (Proxy :: Proxy n) v

--- Closed declarations ---

class (Vec a n v) => MVec a n v
instance (Vec a n v) => MVec a n v

mvIndex :: (VIndex fn l v a) => fn -> l -> v -> a
mvIndex = vIndex

mvLength :: (VLength a v n) => Tagged a v -> n
mvLength = vLength

mvReplicate :: (VReplicate n a v) => n -> a -> v
mvReplicate = vReplicate

{-
mvStringRepresentation :: (VStringRepresentation a n v) => a -> n -> v
mvStringRepresentation = vStringRepresentation
-}
