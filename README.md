# ppl

Get the vertices of a polyhedron defined by linear constraints.

## Example

Consider the system of linear constraints:

```
x + y + z =  0
        z <= 10
x + y     <= 5/2
x - y     <= 0
x - y     >= -1        
```

First define the variables:

```haskell
x = newVar 1 -- that means x is the first variable
y = newVar 2 -- y is the second variable
z = newVar 3 -- z is the third variable
```

Define the set of linear constraints:

```haskell
import Data.Ratio
vars = [x,y,z]
constraints =
  [(linearCombination vars [(1,x), (1,y), (1,z)]) .=. constant 0
  ,(linearCombination vars [(1,z)])               .<. constant 10
  ,(linearCombination vars [(1,x), (1,y)])        .<. constant (5%2)
  ,(linearCombination vars [(1,x), (-1,y)])       .<. constant 0
  ,(linearCombination vars [(2,x), (-1,y)])       .>. constant (-1)]
```

Get the vertices:

```haskell
> vertices <- getVertices constraints
> vertices
[[5 % 4,5 % 4,(-5) % 2],[(-1) % 1,(-1) % 1,2 % 1],[1 % 2,2 % 1,(-5) % 2]]
```

If you find this output unreadable, use `showVertices`:

```haskell
> showVertices vertices
(5/4, 5/4, -5/2)
(-1, -1, 2)
(1/2, 2, -5/2)
```

The type of the two members of a linear constraint is `LinearCombination`,
and this type has the `VectorSpace` instance. Thus the above set of constraints
can be equivalently defined like this:

```haskell
import Data.VectorSpace
x = newVar 1
y = newVar 2
z = newVar 3
vars = [x,y,z]
x' = asLinearCombination vars x
y' = asLinearCombination vars y
z' = asLinearCombination vars z
one = constant 1
constraints =
  [ (x' ^+^ y' ^+^ z') .=. zeroV
  , z'                 .<. (10*^one)
  , (x' ^+^ y')        .<. (5*^one ^/ 2)
  , (x' ^-^ y')        .<. zeroV
  , (2*^x' ^-^ y')     .>. ((-1)*^one)]
```
