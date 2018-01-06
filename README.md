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

## Application: integration on a polytope

Consider this integral:

![equation](http://latex.codecogs.com/gif.latex?%5Cint_%7B-5%7D%5E4%5Cint_%7B-5%7D%5E%7B3-x%7D%5Cint_%7B-10%7D%5E%7B6-x-y%7D%5Csin%28x+y+z%29%5C,%5Cmathrm%7Bd%7Dz%5C,%5Cmathrm%7Bd%7Dy%5C,%5Cmathrm%7Bd%7Dx.)


Get the vertices of the domain of integration:

```haskell
constraints =
  [ x' .>. ((-5)*^one)
  , x' .<. (4*^one)
  , y' .>. ((-5)*^one)
  , y' .<. (3*^one ^-^ x')
  , z' .>. ((-10)*^one)
  , z' .<. (6*^one ^-^ x' ^-^ y')]
  where
    x = newVar 1
    y = newVar 2
    z = newVar 3
    vars = [x,y,z]
    x' = asLinearCombination vars x
    y' = asLinearCombination vars y
    z' = asLinearCombination vars z
    one = constant 1
vertices <- getVertices constraints
```

Now use the [delaunayn](https://github.com/stla/delaunayn) library to split the
polyhedron into simplices:

```haskell
import Delaunay

vertices' = map (map realToFrac) vertices

(indices, _, _) <- delaunay vertices'

simplices = map (\x -> [vertices'!!i | i <- x]) indices
```

Then use the [simplicialcubature](https://github.com/stla/simplicialcubature)
library to integrate on the simplices:

```haskell
import Cubature
import Data.Vector.Unboxed as V

f :: Vector Double -> Double
f v = sin (V.sum v)

integral <- integrateOnSimplex' f simplices 100000 0 1e-10 3
```

```haskell
> integral
Result {value = -79.76973647867133
      , errorEstimate = 7.937713035188602e-2
      , evaluations = 63030
      , success = True}
```
