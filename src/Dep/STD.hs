{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fcontext-stack=1024 #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Dep.STD
    ( module Dep.Data.Fin
    , module Dep.Data.Nat
    , module Dep.Data.Vec
    , module Dep.Function
    ) where

import Dep.Data.Fin
import Dep.Data.Nat
import Dep.Data.Vec
import Dep.Function
