Control.Monad.State.Sugar
=========================

State.Sugar provides some niceties for working with the State monad. It provides:

* Reference to state as variables
* Multiple variables in one state

An example:

```haskell
test :: State (Int, (String, (Int, ()))) ()
test = do 
	-- n is an Int state-variable in position 0
	n :: Var P0 Int <- getv
	-- m is an String state-variable in position 1
	m :: Var P1 String <- getv
	-- set n = 2, m = "abc"
	n =!! 2
	m =!! "abc"
	-- Then set n = 5, m = "foo" using a nicer notation...
	(n,m) =!! (5,"foo")
	-- At any point, you can extract the value of a variable into a normal haskell object
	nval <- eval n
	-- Which can then be used to do something to change the value of m
	m =!! (show nval)
	-- Let's use another variable now...
	k :: Var P2 Int <- getv
	-- We can use (=!) to assign variables to variables
	k =! n
	-- This works because (=!) evaluates if needed and then assigns.
	-- It can work in other circumstances, expect the value can't be polymoriphic...
	-- For example:
	n =! (1 :: Int) 
	-- because the typechecker can't infer type type of 1 with (=!)
	--
	-- BELOW THIS POINT IS NOT YET IMPLEMENTED!!!!!
	-- We also provide a while loop!
	(n,m) = (0,10)
	while (n < m) $ do
		n +=!! 1
		m -=!! 1
```


