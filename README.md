# ppl

Get the vertices of a polyhedron defined by linear constraints.

## Example

Define the variables:

```haskell
x = newVar 1
y = newVar 2
z = newVar 3
vars = [x,y,z]
```

Define a set of linear constraints:

```haskell
import Data.Ratio
constraints =
  [(linearCombination vars [(1,x), (1,y), (1,z)]) .=. constant 0
   (linearCombination vars [(1,z)])               .<. constant 10
   (linearCombination vars [(1,x), (-1,y)])       .>. constant (-10)
   (linearCombination vars [(1,x), (1,y)])        .<. constant (5%2)
   (linearCombination vars [(1,x), (-1,y)])       .<. constant 0
   (linearCombination vars [(1,x), (-1,y)])       .>. constant (-1)]
```

Get the vertices:

```haskell
> getVertices constraintsExample
[[(-5) % 1,(-5) % 1,10 % 1],[5 % 4,5 % 4,(-5) % 2],[3 % 4,7 % 4,(-5) % 2],[(-11) % 2,(-9) % 2,10 % 1]]
```
