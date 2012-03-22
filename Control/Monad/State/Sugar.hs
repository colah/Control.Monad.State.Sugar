{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, GADTs   #-}


module Control.Monad.State.Sugar where

import Control.Monad.State

-- We want to be able to store an arbitrarily large
-- number of monads in our friend state.
-- We'll store them like so (0,(1,(2,(3,...))))

-- This may seem like it has wasteful boxing.
-- I take the position: Box them all and let
-- the compiler sort them out.

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

-- Also, sometimes we have values derived from expressions. 
-- Let's have a type for that "position"

-- It is still based on a variable, so we'll place it's position 
-- as a type inside it.

data ExprPos a = ExprPos a

-- We need to be able to use these to get stuff.

class Accessor position nest result | position nest -> result where
	accessNest :: position -> nest -> result
	modifyNest :: position -> (result -> result) -> nest -> nest

-- If BasePos, we access a from (a,b)

instance Accessor BasePos (pres,children) pres where
	accessNest _ (a,b) = a
	modifyNest _ mod (a,b) = (mod a, b)

-- If NextPos foo we access whatever is in position foo of b from (a,b)

instance 
	forall relposition pres children result.
	(Accessor relposition children result) => 
	Accessor (NextPos relposition) (pres,children) result 
	where

	accessNest _ (a,b) = accessNest (undefined :: relposition) b
	modifyNest _ mod (a,b) = (a, b') where b' = modifyNest (undefined :: relposition) mod b



-- OK. Now a variable is going position and real type

data Var position actualtype where
	Var :: Var position actualtype
	VarExpr :: 
		(Var position actualtype) 
		-> (actualtype -> actualtype2) 
		-> (Var (ExprPos position) actualtype2)

-- For creating variables. The important part is the type :)

getv :: (Accessor position nest result) => State nest (Var position result)
getv = return Var

-- We need to be able to evaluate variables/variable expressions. 
-- For convenience, also non-variables.

class Evaluable start state res where
	eval :: start -> State state res

instance Evaluable a state a where
	eval a = return a

-- {-
instance forall pos res state. (Accessor pos state res) => Evaluable (Var pos res) state res where
	eval Var = do
		nest <- get
		return $ accessNest (undefined :: pos) nest
-- -}

-- We want to be able assign things other that simple variables.
-- Basically, pattern matching.
-- There's a class for that!

class Assignee assignee assigned state | assignee -> assigned where 
	(=!!) :: assignee -> assigned -> State state ()

-- The basic case of assigning 

instance 
	forall position state result. 
	Accessor position state result => 
	Assignee (Var position result) result state 
	where

	_ =!! val = modify (modifyNest (undefined :: position) (\_ -> val))

-- Assigning two tuples

instance 
	forall pos1 res1 pos2 res2 state. 
	(
		Accessor pos1 state res1,
		Accessor pos2 state res2
	) =>
	Assignee (Var pos1 res1, Var pos2 res2) (res1, res2) state 
	where

	(var1, var2) =!! (val1, val2) = do
		var1 =!! val1
		var2 =!! val2

-- assigning three tuples

instance 
	forall pos1 res1 pos2 res2 pos3 res3 state. 
	(
		Accessor pos1 state res1,
		Accessor pos2 state res2,
		Accessor pos3 state res3
	) =>
	Assignee (Var pos1 res1, Var pos2 res2, Var pos3 res3) (res1, res2, res3) state 
	where

	(var1, var2, var3) =!! (val1, val2, val3) = do
		var1 =!! val1
		var2 =!! val2
		var3 =!! val3


var =! preval = do
	val <- eval preval
	var =!! val

-- test

a :: State (Int,(Int, children)) ()
a = do
	n :: Var P0 Int <- getv
	m :: Var P1 Int <- getv
	(n,m) =!! (2,3)
	m =! n

