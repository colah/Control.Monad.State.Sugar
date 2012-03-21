{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables  #-}



module Control.Monad.State.Sugar where

import Control.Monad.State

-- We want to be able to store an arbitrarily large
-- number of monads in our friend state.
-- We'll store them like so (0,(1,(2,(3,...))))

-- We need to encode these positions into the 
-- type system.
--
-- Using TypeNats would be cool, but that isn't
-- wide spread.

data BasePos = BasePos
data NextPos a = NextPos a

-- Some synomyms for convenience...

type P0 = BasePos
type P1 = NextPos P0
type P2 = NextPos P1
type P3 = NextPos P2
type P4 = NextPos P3
type P5 = NextPos P4
type P6 = NextPos P5
type P7 = NextPos P6
type P8 = NextPos P7
type P9 = NextPos P8

-- We need to be able to use these to get stuff.

class Accessor position nest result | position nest -> result where
	accessNest :: position -> nest -> result
	modifyNest :: position -> (result -> result) -> nest -> nest

instance Accessor BasePos (pres,children) pres where
	accessNest _ (a,b) = a
	modifyNest _ mod (a,b) = (mod a, b)

instance 
	forall relposition pres children result.
	(Accessor relposition children result) => 
	Accessor (NextPos relposition) (pres,children) result 
	where

	accessNest _ (a,b) = accessNest (undefined :: relposition) b
	modifyNest _ mod (a,b) = (a, b') where b' = modifyNest (undefined :: relposition) mod b

-- OK. Now a variable is going position and real type

data Var position actualtype = Var
--                             | VarExpr (State a actualtype)

getv :: (Accessor position nest result) => State nest (Var position result)
getv = return Var


class Assignee assignee assigned state | assignee -> assigned  where 
	assigner :: assignee -> assigned -> State state ()

instance 
	forall position state result. 
	Accessor position state result => 
	Assignee (Var position result) result state 
	where

	assigner _ val = modify (modifyNest (undefined :: position) (\_ -> val))

instance 
	forall pos1 res1 pos2 res2 state. 
	(
		Accessor pos1 state res1,
		Accessor pos2 state res2
	) =>
	Assignee (Var pos1 res1, Var pos2 res2) (res1, res2) state 
	where

	assigner _ (val1, val2) = modify $ (modifyNest (undefined :: pos1) (\_ -> val1)) . (modifyNest (undefined :: pos2) (\_ -> val2))


(|=) :: 
	forall position nest result. 
	(Accessor position nest result) => 
		Var position result 
		-> result 
		-> State nest ()
var |= val = modify (modifyNest (undefined :: position) (\_ -> val)) 

a :: State (Int,(Int, children)) ()
a = do
	n :: Var P0 Int <- getv
	m :: Var P1 Int <- getv
	n |= 2

