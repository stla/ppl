{-# LANGUAGE ForeignFunctionInterface #-}
module Vertices
  (newVar, linearCombination, constant, (.>.), (.<.), (.=.), getVertices)
  where
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import           Data.List          (intercalate)
import           Data.List.Unique   (allUnique)
import           Data.Ratio
import           Data.Tuple         (swap)
import           Foreign.C.String
import           System.IO          (writeFile)
import           TemporaryFile
import           Text.Printf        (printf)

foreign import ccall unsafe "getVertices" c_getVertices
  :: CString -> IO CString

type Var = Int

data LinearCombination = LinearCombination (IntMap Rational)
     deriving Eq

instance Show LinearCombination where
  show (LinearCombination x) =
    intercalate " + " $
     (map (\(i,r) -> if i==0 then showR r else (showR r) ++ " x" ++ (show i))
          (M.toAscList x))

showR :: Rational -> String
showR r = if q==1 then show p else (show p) ++ "/" ++ (show q)
          where
            p = numerator r
            q = denominator r

data Sense = Gt | Lt | Eq
     deriving Eq

instance Show Sense where
  show Gt = ">="
  show Lt = "<="
  show Eq = "="

data Constraint = Constraint (LinearCombination, Sense, Rational)
     deriving Eq

instance Show Constraint where
  show (Constraint (a,b,c)) = (show a) ++ " " ++ (show b) ++ " " ++ (showR c)

newVar :: Int -> Var
newVar i = case i >= 0 of
  True  -> i
  False -> error "negative index"

simplifyTerms :: [(Rational,Var)] -> IntMap Rational
simplifyTerms terms = M.fromListWith (+) (map swap terms)

linearCombination :: [Var] -> [(Rational,Var)] -> LinearCombination
linearCombination vars terms =
  case allUnique vars of
    False -> error "duplicated variable"
    True -> case all (flip elem vars) presentVariables of
      False -> error "unknown variable in the terms"
      True -> LinearCombination
               (M.union simplifiedTerms
                        (M.fromList [(i,0) | i <- [0..length vars]]))
  where
    simplifiedTerms = simplifyTerms terms
    presentVariables = M.keys simplifiedTerms

constant :: Rational -> LinearCombination
constant x = LinearCombination (M.singleton 0 x)

inequality :: Sense -> LinearCombination -> LinearCombination -> Constraint
inequality sense (LinearCombination lhs) (LinearCombination rhs) =
  Constraint (LinearCombination newlhs, sense, newrhs)
  where
    newrhs = (M.findWithDefault 0 0 rhs) - (M.findWithDefault 0 0 lhs)
    newlhs = M.delete 0 (M.mergeWithKey (\i x y -> Just (x-y)) id id lhs rhs)

constraintVariables :: Constraint -> [Var]
constraintVariables (Constraint (LinearCombination x, _, _)) = M.keys x

(.>.) :: LinearCombination -> LinearCombination -> Constraint
(.>.) = inequality Gt

(.<.) :: LinearCombination -> LinearCombination -> Constraint
(.<.) = inequality Lt

(.=.) :: LinearCombination -> LinearCombination -> Constraint
(.=.) = inequality Eq

normalizeConstraint :: Constraint -> String
normalizeConstraint (Constraint (LinearCombination lhs, sense, rhs)) =
  printf "size %d " (length lhs + 1) ++ string ++ " (C)"
  where
    string = case sense of
        Gt -> intercalate " " (map show newcoefs) ++ " >="
        Lt -> intercalate " " (map (show.negate) newcoefs) ++ " >="
        Eq -> intercalate " " (map show newcoefs) ++ " ="
    coefs = (-rhs) : (map snd (M.toAscList lhs))
    denominators = map denominator coefs
    ppcm = (foldr lcm 1 denominators) % 1
    newcoefs = map (numerator.(*ppcm)) coefs

writeConstraints :: [Constraint] -> FilePath -> IO ()
writeConstraints constraints filename = do
  writeFile filename (intercalate "\n" lines)
  where
    lines = header ++ (map normalizeConstraint constraints)
    d = dimension (head constraints)
    l = length constraints
    header = ["topology NECESSARILY_CLOSED"] ++
             [printf "%d x %d SPARSE (not_sorted)" l d] ++
             [printf "index_first_pending %d" l]
    dimension :: Constraint -> Int
    dimension (Constraint (LinearCombination lhs, _, _)) = length lhs

getVerticesFromFile :: FilePath -> IO String
getVerticesFromFile filename = do
  filename <- newCString filename
  (>>=) (c_getVertices filename) peekCString

getVerticesRaw :: [Constraint] -> IO String
getVerticesRaw constraints = do
  tmpFile <- getTemporaryFile "tmp.txt"
--  let tmpFile = "tmp.txt"
  writeConstraints constraints tmpFile
  getVerticesFromFile tmpFile

rawResultsToVertices :: String -> [[Rational]]
rawResultsToVertices rawResults =
  zipWith (\nums -> \den -> map (flip (%) den) nums) numerators denominators
  where
    stringLines = lines rawResults
    n = read [head (stringLines!!1)] :: Int
    rawPoints = map words (drop 3 stringLines)
    coefs = map ((drop 2).(init.init)) rawPoints
    denominators = map (read.head) coefs :: [Integer]
    numerators = (map ((map read).tail) coefs) :: [[Integer]]

getVertices :: [Constraint] -> IO [[Rational]]
getVertices constraints = do
  case all (== head variablesSets) (tail variablesSets) of
    False -> error "constraints with different variables"
    True -> do
      rawResults <- getVerticesRaw constraints
      case rawResults == "unbounded" of
        True  -> error "unbounded polyhedron"
        False -> return $ rawResultsToVertices rawResults
  where variablesSets = map constraintVariables constraints

constraintExample :: Constraint
constraintExample = l1 .>. l2
  where
    x = newVar 1
    y = newVar 2
    l1 = linearCombination [x,y] [(2%3,x), (1%4,y)]
    l2 = linearCombination [x,y] [(1,x), (2,y)]

constraintsExample :: [Constraint]
constraintsExample = [c1,c2,c3,c4,c5,c6]
  where
    x = newVar 1
    y = newVar 2
    z = newVar 3
    vars = [x,y,z]
    c1 = linearCombination vars [(1,x), (1,y), (1,z)] .=. constant 0
    c2 = linearCombination vars [(1,z)] .<. constant (10)
    c3 = linearCombination vars [(1,x), (-1,y)] .>. constant (-10)
    c4 = linearCombination vars [(1,x), (1,y)] .<. constant (5%2)
    c5 = linearCombination vars [(1,x), (-1,y)] .<. constant 0
    c6 = linearCombination vars [(1,x), (-1,y)] .>. constant (-1)