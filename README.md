Control.Monad.State.Sugar
=========================

State.Sugar provides some niceties for working with the State monad. It provides:

* Reference to state as variables
* Multiple variables in one state

An example:

```haskell
do 
	-- n is an Int state-variable in position 0
	n :: Var P0 Int <- getv
	-- m is an String state-variable in position 1
	m :: Var P1 String <- getv
	-- set n = 2, m = "abc"
	n |= 2
	m |= "abc"
	-- Then set n = 5, m = "foo" using a nicer notation...
	(n,m) |= (5,"foo")
	-- At any point, you can extract the value of a variable into a normal haskell object
	nval <- val n
```


