module Rcube
(
   Color (..),
   switchColor,

   Cell (..),
   
   Position (..),
   getX,
   getY,
   getZ,
   getCoords,
   isCorner,
   isEdge,
   isCenter,
   isInternal,

   Notation(..),

   Plane (..),

   Algorithm (..),
   strToAlg,
   
   Cube (..),
   getStartPos,
   getEndPos,
   cbSize,
   isSolved,
   createSolvedCubeCell,
   createSolvedCube,
   rotateList,
   rotateListA,
   rotateListN,
   rotateListNA,
   rotateCubePlane,
   rotateCubePlaneA,
   uMove,
   uPrMove,
   dMove,
   dPrMove,
   lMove,
   lPrMove,
   rMove,
   rPrMove,
   fMove,
   fPrMove,
   bMove,
   bPrMove,
   mMove,
   mPrMove,
   eMove,
   ePrMove,
   sMove,
   sPrMove,
   singleMove,
   listMove
   
) where

import qualified Data.Map as Map
import Data.Array
import Data.List
import Data.String
import Data.Char

-- *************************
-- * Color
-- *
-- *  Signify all possible colors of the cube sides.
-- *  Assuming that the blue side is in front of us and the white side on the top side,
-- *  then following an anti-clockwise order we have:
-- *  Blue, Orange, Green, Red, White, Yellow 
-- *
-- *************************
data Color = Blue | Orange | Green | Red | White | Yellow  --Yellow | Red | White | Orange | Blue | Green 
             deriving  (Eq,  Ord,  Show,  Read,  Bounded,  Enum)

-- *************************
-- * Cell
-- *
-- *  Signify the colors of a cube cell.
-- *  A cube cell can have:
-- *    3 colors (corner cell)
-- *    2 colors (edge cell)
-- *    1 color  (center cell)
-- *    0 colors (internal cell)
-- *
-- *   We use the "Nothing" value of the "Maybe Color" data type to signify the no-color case
-- *
-- *   The colors of a cell are ordered according to this rule:
-- *   Cell(Maybe Color1, Maybe Color2, Maybe Color3)
-- *     Maybe Color1 : is the color of the front/back side
-- *     Maybe Color2 : is the color of the left/right side
-- *     Maybe Color3 : is the color of the top/bottom side
-- *     The internal cells have color: Cell(Nothing, Nothing, Nothing) 
-- *
-- *************************
data Cell = Cell (Maybe Color, Maybe Color, Maybe Color)
            deriving  (Eq,  Ord,  Show,  Read)

------------
-- switchColor
--    switches the colors of a Cell when a single move on the cube is performed.
--    The colors that will be switched depends on the Plane that the rotation takes place
--
------------
switchColor
  :: Plane  -- input plane (that the rotation takes plane)
  -> Cell   -- input Cell
  -> Cell   -- output Cell with switched colors
switchColor X (Cell(c1 , c2, c3))  = Cell(c3, c2, c1)
switchColor Y (Cell(c1 , c2, c3))  = Cell(c2, c1, c3)
switchColor Z (Cell (c1 , c2, c3))  = Cell(c1, c3, c2)
            

-- *************************
-- * Position
-- *
-- *   Signifies a position of coordinates (x,y,z) in the n x n x n "cube space".
-- *   Where x,y,z take values from the domain [1, n]
-- *   
-- *************************
data Position = Position (Int, Int, Int)
                deriving  (Eq,  Ord,  Show,  Read, Ix)
--type Position = Ix Int            

------------
-- getX
------------
getX :: Position -> Int
getX (Position(x,_,_)) = x

------------
-- getY
------------
getY :: Position -> Int
getY (Position(_,y,_)) = y

------------
-- getZ
------------
getZ :: Position -> Int
getZ (Position(_,_,z)) = z

------------
-- getCoords
------------
getCoords :: Position -> (Int, Int, Int)
getCoords (Position(x,y,z)) = (x, y, z)


-----------------
--  NOTES (some conventions):
-- Assume a cube c nxnxn
-- For each association of the form: (Position(x,y,z), Cell(a,b,c))
-- the following constraints hold:
-- If ALL out of x,y,z IN [1,n] then c = "corner cell" i.e., it has 3 colors
--  if ONLY ONE out of x,y,z NOT IN [1,n] then c = "edge cell" i.e., it has only 2 colors
--  if ONLY ONE out of x,y,z IN [1,n] then c = "center cell", i.e., it has only 1 color
--  If ALL out of x,y,z NOT IN [1,n] then c = "internal cell" i.e., it has no colors
--
--  Normal poistion of the cube:
--   we assume that the blue side is in front of us and the white side on the top side.
--
-----------------

------------
-- isCorner 
--    Returns True only if a Position corresponds to a corner cell in the Cube
------------
isCorner
   :: Int -- size of the cube
   -> Position  -- position in question
   -> Bool
isCorner n pos 
-- = if pos `elem` allCombinationsStartEnd (Position(1,1,1) Position(n,n,n)) then True else False
   | pos `elem` [Position(x,y,z) | x <- [1,n], y <- [1,n], z <- [1,n]] = True
   | otherwise = False


------------
-- isEdge 
--    Returns True only if a Position corresponds to an edge cell in the Cube
------------
isEdge 
   :: Int -- size of the cube
   -> Position  -- position in question
   -> Bool
isEdge n pos 
   | pos `elem` [Position(x,y,z) | x <- [1,n], y <- [2..n-1], z <- [1,n]] = True
   | pos `elem` [Position(x,y,z) | x <- [1,n], y <- [1,n], z <- [2..n-1]] = True
   | pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [1,n], z <- [1,n]] = True
   | otherwise = False

------------
-- isCenter 
--    Returns True only if a Position corresponds to a center cell in the Cube
------------
isCenter 
   :: Int -- size of the cube
   -> Position  -- position in question
   -> Bool
isCenter n pos 
   | pos `elem` [Position(x,y,z) | x <- [1,n], y <- [2..n-1], z <- [2..n-1]] = True
   | pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [1,n], z <- [2..n-1]] = True
   | pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [2..n-1], z <- [1,n]] = True
   | otherwise = False

--
-- isInternal 
-- Returns True if a Position corresponds to an internal position in the Cube
--
isInternal 
   :: Int -- size of the cube
   -> Position  -- position in question
   -> Bool
isInternal n pos 
   | pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [2..n-1], z <- [2..n-1]] = True
   | otherwise = False

------------
-- allCombinationsStartEnd
--
--    Get a start and an end Position and returns a list of Positions corresponding to all posible combinations of the coordinates
--    of the input Positions.
--    Example: 
--       allCombinationsStartEnd Position(1,1,1) Position(3,3,3) = [Position(0,0,0),Position(0,0,3),Position(0,3,0),Position(0,3,3),Position(3,0,0),Position(3,0,3),Position(3,3,0),Position(3,3,3)]
--    Note: this is different from the "range" function of Type Class  Ix which returns ALL positions between the two end points.
------------
allCombinationsStartEnd :: Position -> Position -> [Position]
allCombinationsStartEnd startPos endPos = map (Position) $ zip3 (take 4 [startX,startX..] ++ take 4 [endX,endX..]) (take 8 $ cycle [startY,startY,endY,endY]) (take 8 $ cycle [startY,endY])
   where startX = getX startPos
         startY = getY startPos
         startZ = getZ startPos
         endX = getX endPos
         endY = getY endPos
         endZ = getZ endPos

-- *************************
-- * Notation
--
--    Rubiks Cube Notation
--      http://ruwix.com/the-rubiks-cube/notation/
-- *************************

data Notation = U | D | L | R | F | B | M | E | S | U' | D' | L' | R' | F' | B' | M' | E' | S' | Ui Int | Di Int | Li Int | Ri Int | Fi Int | Bi Int | U'i Int | D'i Int | L'i Int | R'i Int | F'i Int | B'i Int
             deriving  (Eq,  Ord,  Show,  Read)


-----------------------------------
-- Ui 1  -- for 4x4x4 and 5x5x5
-- Ui 2  -- for 6x6x6 and 7x7x7
-- Ui 3  -- for 8x8x8 and 9x9x9
--
-- 1 means the closest to U, 2 means the 2nd closest to U and so on before the E plane
-----------------------------------

-- *************************
-- * Algorithm
--
-- *************************             

type Algorithm = [Notation]

--instance IsString Notation where
--  fromString :: String -> Algorithm
--  fromString [] = []
--  fromString (x:('\'':(y:xs)))
--        |  toUpper(x) == 'U' && not (isDigit(y))   = U':fromString(xs)
--        |  toUpper(x) == 'D' && not (isDigit(y))   = D':fromString(xs)        
--        |  toUpper(x) == 'F' && not (isDigit(y))   = F':fromString(xs)        
--        |  toUpper(x) == 'B' && not (isDigit(y))   = B':fromString(xs)        
--        |  toUpper(x) == 'L' && not (isDigit(y))   = L':fromString(xs)        
--        |  toUpper(x) == 'R' && not (isDigit(y))   = R':fromString(xs)        
--        |  toUpper(x) == 'M' && not (isDigit(y))   = M':fromString(xs)        
--        |  toUpper(x) == 'E' && not (isDigit(y))   = E':fromString(xs)        
--        |  toUpper(x) == 'S' && not (isDigit(y))   = S':fromString(xs)        
--        |  toUpper(x) == 'U' && isDigit(y)       = (U'i digitToInt(y)):fromString(xs)        
--        |  toUpper(x) == 'D' && isDigit(y)       = (D'i digitToInt(y)):fromString(xs)        
--        |  toUpper(x) == 'L' && isDigit(y)       = (L'i digitToInt(y)):fromString(xs)
--        |  toUpper(x) == 'R' && isDigit(y)       = (R'i digitToInt(y)):fromString(xs)
--        |  toUpper(x) == 'F' && isDigit(y)       = (F'i digitToInt(y)):fromString(xs)
--        |  toUpper(x) == 'B' && isDigit(y)       = (B'i digitToInt(y)):fromString(xs)
--  fromString (x:(y:xs))  
--        |  toUpper(x) == 'U'  && not (isDigit(y)) && y /= '\'' = U:fromString(y:xs)
--        |  toUpper(x) == 'D'  && not (isDigit(y)) && y /= '\'' = D:fromString(y:xs)
--        |  toUpper(x) == 'F'  && not (isDigit(y)) && y /= '\'' = F:fromString(y:xs)
--        |  toUpper(x) == 'B'  && not (isDigit(y)) && y /= '\'' = B:fromString(y:xs)
--        |  toUpper(x) == 'L'  && not (isDigit(y)) && y /= '\'' = L:fromString(y:xs)
--        |  toUpper(x) == 'R'  && not (isDigit(y)) && y /= '\'' = R:fromString(y:xs)
--        |  toUpper(x) == 'M'  && not (isDigit(y)) && y /= '\'' = M:fromString(y:xs)
--        |  toUpper(x) == 'E'  && not (isDigit(y)) && y /= '\'' = E:fromString(y:xs)
--        |  toUpper(x) == 'S'  && not (isDigit(y)) && y /= '\'' = S:fromString(y:xs)
--        |  toUpper(x) == 'U'  && isDigit(y) = (Ui digitToInt(y)):fromString(xs)
--        |  toUpper(x) == 'D'  && isDigit(y) = (Di digitToInt(y)):fromString(xs)
--        |  toUpper(x) == 'L'  && isDigit(y) = (Li digitToInt(y)):fromString(xs)
--        |  toUpper(x) == 'R'  && isDigit(y) = (Ri digitToInt(y)):fromString(xs)
--        |  toUpper(x) == 'F'  && isDigit(y) = (Fi digitToInt(y)):fromString(xs)
--        |  toUpper(x) == 'B'  && isDigit(y) = (Bi digitToInt(y)):fromString(xs)
--        |  otherwise = []    

------------
-- strToAlg
--    Convert a string to an Algorithm
--
------------
strToAlg
  :: String     -- input string
  -> Algorithm  -- output algorithm
strToAlg [] = []
        -- U'i, D'i, ...
strToAlg (x:('\'':(y:xs)))
      |  toUpper(x) == 'U' && isDigit(y)         = (U'i $ digitToInt(y)):strToAlg(xs)        
      |  toUpper(x) == 'D' && isDigit(y)         = (D'i $ digitToInt(y)):strToAlg(xs)        
      |  toUpper(x) == 'L' && isDigit(y)         = (L'i $ digitToInt(y)):strToAlg(xs)
      |  toUpper(x) == 'R' && isDigit(y)         = (R'i $ digitToInt(y)):strToAlg(xs)
      |  toUpper(x) == 'F' && isDigit(y)         = (F'i $ digitToInt(y)):strToAlg(xs)
      |  toUpper(x) == 'B' && isDigit(y)         = (B'i $ digitToInt(y)):strToAlg(xs)
        -- Ui, Di, ...
strToAlg (x:(y:xs))  
      |  toUpper(x) == 'U'  && isDigit(y)        = (Ui $ digitToInt(y)):strToAlg(xs)
      |  toUpper(x) == 'D'  && isDigit(y)        = (Di $ digitToInt(y)):strToAlg(xs)
      |  toUpper(x) == 'L'  && isDigit(y)        = (Li $ digitToInt(y)):strToAlg(xs)
      |  toUpper(x) == 'R'  && isDigit(y)        = (Ri $ digitToInt(y)):strToAlg(xs)
      |  toUpper(x) == 'F'  && isDigit(y)        = (Fi $ digitToInt(y)):strToAlg(xs)
      |  toUpper(x) == 'B'  && isDigit(y)        = (Bi $ digitToInt(y)):strToAlg(xs)
        -- U', D', ...
strToAlg (x:('\'':xs))
      |  toUpper(x) == 'U' = U':strToAlg(xs)
      |  toUpper(x) == 'D' = D':strToAlg(xs)        
      |  toUpper(x) == 'F' = F':strToAlg(xs)        
      |  toUpper(x) == 'B' = B':strToAlg(xs)        
      |  toUpper(x) == 'L' = L':strToAlg(xs)        
      |  toUpper(x) == 'R' = R':strToAlg(xs)        
      |  toUpper(x) == 'M' = M':strToAlg(xs)        
      |  toUpper(x) == 'E' = E':strToAlg(xs)        
      |  toUpper(x) == 'S' = S':strToAlg(xs)        
        -- U,D, ...
strToAlg (x:xs)
      |  toUpper(x) == 'U'  = U:strToAlg(xs)
      |  toUpper(x) == 'D'  = D:strToAlg(xs)
      |  toUpper(x) == 'F'  = F:strToAlg(xs)
      |  toUpper(x) == 'B'  = B:strToAlg(xs)
      |  toUpper(x) == 'L'  = L:strToAlg(xs)
      |  toUpper(x) == 'R'  = R:strToAlg(xs)
      |  toUpper(x) == 'M'  = M:strToAlg(xs)
      |  toUpper(x) == 'E'  = E:strToAlg(xs)
      |  toUpper(x) == 'S'  = S:strToAlg(xs)      
      |  otherwise = []    



-- *************************
-- * Plane
--
-- *************************             

data Plane = X | Y | Z
            deriving (Eq, Ord, Show, Read, Bounded, Enum)       
-- *************************
-- * Cube
-- *************************

--data Cube = Cube Int Array Position Cell
--            deriving  (Eq,  Ord,  Show,  Read, Array)

type Cube = Array Position Cell -- Array Position (Position, Cell)

-- alternatively we could define a Cube as a map
-- (we need to find a data structure that is most efficient for the typical "cube operations")
-- type Cube = Map.Map Position Cell

--------
-- xor
--   a simple XOR function
--------
xor :: Bool -> Bool -> Bool
xor x y = (x || y) && (not (x && y))

------------
-- getStartPos
--    Returns the starting position of a Cube
------------
getStartPos :: Cube -> Position
getStartPos c = fst $ bounds c

------------
-- getEndPos
--    Returns the ending position of a Cube
------------
getEndPos :: Cube -> Position
getEndPos c = snd $ bounds c


-------------
-- cbSize
--  Returns the size of a cube (e.g. for a 3x3x3 the size is 3)
--
-------------
cbSize
  :: Cube
  -> Int
cbSize cb = getX (getEndPos cb) - getX (getStartPos cb)  + 1

-------------
-- isSolved
--  Returns True if the input cube is a solved cube
--
-------------
isSolved
  :: Cube
  -> Bool
isSolved c = c == createSolvedCube (cbSize c)  


------------
--  createSolvedCubeCell:
--    Description: Receive as input a specific Position and the size of a Cube and return 
--      the appopriate Cell that corresponds to a (Position, Cell) association of a solved cube.
--
--  NOTES:
-- Assume a cube c nxnxn
-- Assume starting position is Position(1,1,1) and ending position is Position(n,n,n)
-- For each association of the form: (Position(x,y,z), Cell(a,b,c))
-- the following constraints hold:
-- If ALL out of x,y,z IN [1,n] then c = "corner cell" i.e., it has 3 colors
--  if ONLY ONE out of x,y,z NOT IN [1,n] then c = "edge cell" i.e., it has only 2 colors
--  if ONLY ONE out of x,y,z IN [1,n] then c = "center cell", i.e., it has only 1 color
--  If ALL out of x,y,z NOT IN [1,n] then c = "internal cell" i.e., it has no colors
--
------------
createSolvedCubeCell 
   :: Int -- Size of the cube (e.g., for a 3x3x3 this should be 3)   
   -> Position  -- position in question
   -> Cell -- Cell for the input position that corresponds to a solved cube
createSolvedCubeCell n pos
  -- corner cells
  | pos == Position (1,1,1) = Cell (Just Blue, Just Red, Just Yellow) 
  | pos == Position (1,1,n) = Cell (Just Green, Just Red, Just Yellow) 
  | pos == Position (1,n,1) = Cell (Just Blue, Just Red, Just White)   
  | pos == Position (1,n,n) = Cell (Just Green, Just Red, Just White)           
  | pos == Position (n,1,1) = Cell (Just Blue, Just Orange, Just Yellow)                    
  | pos == Position (n,1,n) = Cell (Just Green, Just Orange, Just Yellow)                            
  | pos == Position (n,n,1) = Cell (Just Blue, Just Orange, Just White)                                       
  | pos == Position (n,n,n) = Cell (Just Green, Just Orange, Just White)   
  -- edge cells
  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [1], y <- [2..n-1], z <- [1]] = Cell (Just Blue, Just Red, Nothing)
  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [n], y <- [2..n-1], z <- [1]] = Cell (Just Blue, Just Orange, Nothing)
  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [1], z <- [1]] = Cell (Just Blue, Nothing, Just Yellow)
  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [n], z <- [1]] = Cell (Just Blue, Nothing, Just White)

  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [1], y <- [2..n-1], z <- [n]] = Cell (Just Green, Just Red, Nothing)
  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [n], y <- [2..n-1], z <- [n]] = Cell (Just Green, Just Orange, Nothing)
  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [1], z <- [n]] = Cell (Just Green, Nothing, Just Yellow)
  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [n], z <- [n]] = Cell (Just Green, Nothing, Just White)

  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [1], y <- [1], z <- [2..n-1]] = Cell (Nothing, Just Red, Just Yellow)
  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [1], y <- [n], z <- [2..n-1]] = Cell (Nothing, Just Red, Just White)
  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [n], y <- [1], z <- [2..n-1]] = Cell (Nothing, Just Orange, Just Yellow)
  | isEdge n pos &&  pos `elem` [Position(x,y,z) | x <- [n], y <- [n], z <- [2..n-1]] = Cell (Nothing, Just Orange, Just White)
  -- center cells
  | isCenter n pos &&  pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [2..n-1], z <- [1]] = Cell (Just Blue, Nothing, Nothing)
  | isCenter n pos &&  pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [2..n-1], z <- [n]] = Cell (Just Green, Nothing, Nothing)

  | isCenter n pos &&  pos `elem` [Position(x,y,z) | x <- [1], y <- [2..n-1], z <- [2..n-1]] = Cell (Nothing, Just Red, Nothing)
  | isCenter n pos &&  pos `elem` [Position(x,y,z) | x <- [n], y <- [2..n-1], z <- [2..n-1]] = Cell (Nothing, Just Orange, Nothing)

  | isCenter n pos &&  pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [1], z <- [2..n-1]] = Cell (Nothing, Nothing, Just Yellow)
  | isCenter n pos &&  pos `elem` [Position(x,y,z) | x <- [2..n-1], y <- [n], z <- [2..n-1]] = Cell (Nothing, Nothing, Just White)
  -- internal cells
  | otherwise = Cell (Nothing, Nothing, Nothing) 
          

------------
-- createSolvedCube
--
--    Creates a cube of size n, which is solved, i.e., its (Position, Cell) associations
--    correspond to a solved cube.
--
------------
createSolvedCube 
   :: Int -- size of the cube (e.g. 3 for a 3x3x3)
   -> Cube  -- returned solevd cube
createSolvedCube n = listArray (startPos, endPos) $ map (createSolvedCubeCell n) $ range (startPos, endPos)
      where startPos = Position(1,1,1)
            endPos = Position(n,n,n)

------------
-- rotateList
--    Rotates the elements of a list clockwise by a single position
--
------------
rotateList
  :: [a]
  -> [a]
rotateList [] = []
rotateList l = last l : init l


------------
-- rotateListA
--    Rotates the elements of a list anti-clockwise by a single position
--
------------
rotateListA
  :: [a]
  -> [a]
rotateListA [] = []
rotateListA l = tail l ++ [head l] 

------------
-- rotateListN
--    Rotates the elements of a list clockwise by a N positions
--
------------
rotateListN 
  :: Integral b
  =>  b  -- number of rotations
  -> [a]  --  input list
  -> [a]  -- rotated list  
rotateListN _ [] = []
rotateListN 0 l = l
--rotateListN 1 l = last l : init l  -- !!!NOTE!!! If you uncomment this line, then you get an "Non-exhaustive patterns in function rotateListN" error when you call "rotateListN n [1,2,3,4]"" with n > 1
rotateListN n l 
          | n > 0 = rotateListN (n-1) (rotateList l) --(last l : init l)
          | otherwise = []

------------
-- rotateListNA
--    Rotates the elements of a list clockwise by a N positions Anti-Clockwise
--
------------
rotateListNA 
  :: Integral b
  =>  b  -- number of rotations
  -> [a]  --  input list
  -> [a]  -- rotated list  
rotateListNA _ [] = []
rotateListNA 0 l = l
--rotateListN 1 l = last l : init l  -- !!!NOTE!!! If you uncomment this line, then you get an "Non-exhaustive patterns in function rotateListN" error when you call "rotateListN n [1,2,3,4]"" with n > 1
rotateListNA n l 
          | n > 0 = rotateListNA (n-1) (rotateListA l)
          | otherwise = []


------------
-- rotateCubePlane
--    Rotates the elements of a cube lying on a specific plane clockwise by a single position
--    Example:
--      rotate plane at y = 2 for a 2x2x2 cube c
--      let cc = rotateCubePlane c [(Position(x,y,z), c!Position(x,y,z)) | x <- [1..2], y <- [2], z <- [1..2]] Y
--
--      rotate plane at x = 1 for a 2x2x2 cube c
--      let cc = rotateCubePlane c [(Position(x,y,z), c!Position(x,y,z)) | x <- [1], y <- [1..2], z <- [1..2]] X
--
--      rotate plane at z = 2 for a 2x2x2 cube c
--      let cc = rotateCubePlane c [(Position(x,y,z), c!Position(x,y,z)) | x <- [1..2], y <- [1..2], z <- [2]] Z
-- 
--  C2 ----- C3       C1 ----- C2           
--  |        |  ===>  |        |
--  |        |        |        |   
--  C1 ----- C4       C4 ----- C3
------------
rotateCubePlane
  :: Cube -- source cube
  -> [(Position,Cell)]  -- target list of (Position,Cell) associations, corresponding to the plane to be rotated
                        -- Typically, expressed via a list comprehension.
  -> Plane              -- plane characterization
  -> Cube -- new cube (rotated)t
rotateCubePlane cb ls pl = cb // finalList  -- update the cube with the (//) array function
   where
      -- order the list of associations in a "cyclic" order instead of the row-major order of the array      
      newList = case pl of  Y -> nub $ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x == getX (getStartPos cb), z `elem` [getZ (getStartPos cb) .. getZ (getEndPos cb)]]
                                 ++ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], z == getZ (getEndPos cb)]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x == getX (getEndPos cb), z `elem`  [getZ (getStartPos cb) .. getZ (getEndPos cb)]]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], z == getZ (getStartPos cb)]
                            X -> nub $ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, y == getY (getStartPos cb), z `elem` [getZ (getStartPos cb) .. getZ (getEndPos cb)]]
                                 ++ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, y `elem` [getY (getStartPos cb) .. getY (getEndPos cb)], z == getZ (getEndPos cb)]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, y == getY (getEndPos cb), z `elem`  [getZ (getStartPos cb) .. getZ (getEndPos cb)]]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, y `elem` [getY (getStartPos cb) .. getY (getEndPos cb)], z == getZ (getStartPos cb)]                                
                            Z -> nub $ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x == getX (getEndPos cb), y `elem` [getY (getStartPos cb) .. getY (getEndPos cb)]]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], y == getY (getEndPos cb)]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x == getX (getStartPos cb), y `elem`  [getY (getStartPos cb) .. getY (getEndPos cb)]]
                                 ++ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], y == getY (getStartPos cb)]                                 
      -- break the list in two: positions and cells
      posList = map (fst) newList
      cellList = map (snd) newList
      -- rotate the cell list
      newCellList = map (switchColor pl) $ rotateListN ((cbSize cb) - 1) cellList -- rotateList cellList
      -- create final list of associations by zipping the two lists
      finalList = zipWith (,) posList newCellList


------------
-- rotateCubePlaneA
--    Rotates the elements of a cube lying on a specific plane anti-clockwise by a single position
--    Example:
--      rotate plane at y = 2 for a 2x2x2 cube c
--      let cc = rotateCubePlaneA c [(Position(x,y,z), c!Position(x,y,z)) | x <- [1..2], y <- [2], z <- [1..2]] Y
--
--      rotate plane at x = 1 for a 2x2x2 cube c
--      let cc = rotateCubePlaneA c [(Position(x,y,z), c!Position(x,y,z)) | x <- [1], y <- [1..2], z <- [1..2]] X
--
--      rotate plane at z = 2 for a 2x2x2 cube c
--      let cc = rotateCubePlaneA c [(Position(x,y,z), c!Position(x,y,z)) | x <- [1..2], y <- [1..2], z <- [2]] Z
-- 
--  C2 ----- C3       C3 ----- C4           
--  |        |  ===>  |        |
--  |        |        |        |   
--  C1 ----- C4       C2 ----- C1
------------
rotateCubePlaneA
  :: Cube -- source cube
  -> [(Position,Cell)]  -- target list of (Position,Cell) associations, corresponding to the plane to be rotated
                        -- Typically, expressed via a list comprehension.
  -> Plane              -- plane characterization
  -> Cube -- new cube (rotated)t
rotateCubePlaneA cb ls pl = cb // finalList  -- update the cube with the (//) array function
   where
      -- order the list of associations in a "cyclic" order instead of the row-major order of the array      
      newList = case pl of  Y -> nub $ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x == getX (getStartPos cb), z `elem` [getZ (getStartPos cb) .. getZ (getEndPos cb)]]
                                 ++ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], z == getZ (getEndPos cb)]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x == getX (getEndPos cb), z `elem`  [getZ (getStartPos cb) .. getZ (getEndPos cb)]]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], z == getZ (getStartPos cb)]
                            X -> nub $ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, y == getY (getStartPos cb), z `elem` [getZ (getStartPos cb) .. getZ (getEndPos cb)]]
                                 ++ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, y `elem` [getY (getStartPos cb) .. getY (getEndPos cb)], z == getZ (getEndPos cb)]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, y == getY (getEndPos cb), z `elem`  [getZ (getStartPos cb) .. getZ (getEndPos cb)]]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, y `elem` [getY (getStartPos cb) .. getY (getEndPos cb)], z == getZ (getStartPos cb)]                                
                            Z -> nub $ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x == getX (getEndPos cb), y `elem` [getY (getStartPos cb) .. getY (getEndPos cb)]]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], y == getY (getEndPos cb)]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x == getX (getStartPos cb), y `elem`  [getY (getStartPos cb) .. getY (getEndPos cb)]]
                                 ++ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], y == getY (getStartPos cb)]                                 
      -- break the list in two: postiitions and cells
      posList = map (fst) newList
      cellList = map (snd) newList
      -- rotate the cell list
      newCellList = map (switchColor pl) $ rotateListNA ((cbSize cb) - 1) cellList -- rotateListA cellList
      -- create final list of associations by zipping the two lists
      finalList = zipWith (,) posList newCellList

------------
-- uMove (U)
--
--    Accepts as input a cube and returns a new cube after applying the U (Up) move.
--
------------
uMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
uMove cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)], y <- [getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] Y

------------
-- uPrMove (U')
--
--    Accepts as input a cube and returns a new cube after applying the U' (Up Prime) move.
--
------------
uPrMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
uPrMove cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)], y <- [getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] Y

------------
-- dMove (D)
--
--    Accepts as input a cube and returns a new cube after applying the D (Down) move.
--
------------
dMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
dMove cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)], y <- [getY (getStartPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] Y

------------
-- dPrMove (D')
--
--    Accepts as input a cube and returns a new cube after applying the D' (Down Prime) move.
--
------------
dPrMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
dPrMove cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)], y <- [getY (getStartPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] Y

------------
-- lMove (L)
--
--    Accepts as input a cube and returns a new cube after applying the L (Left) move.
--
------------
lMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
lMove cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] X

------------
-- lPrMove (L)
--
--    Accepts as input a cube and returns a new cube after applying the L' (Left Prime) move.
--
------------
lPrMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
lPrMove cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] X


------------
-- rMove (R)
--
--    Accepts as input a cube and returns a new cube after applying the R (Right) move.
--
------------
rMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
rMove cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] X

------------
-- rPrMove (R')
--
--    Accepts as input a cube and returns a new cube after applying the R' (Right Prime) move.
--
------------
rPrMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
rPrMove cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] X


------------
-- fMove (F)
--
--    Accepts as input a cube and returns a new cube after applying the F (Front) move.
--
------------
fMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
fMove cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getEndPos cb)]] Z

------------
-- fPrMove (F')
--
--    Accepts as input a cube and returns a new cube after applying the F' (Front Prime) move.
--
------------
fPrMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
fPrMove cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getEndPos cb)]] Z

------------
-- bMove (B)
--
--    Accepts as input a cube and returns a new cube after applying the B (Back) move.
--
------------
bMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
bMove cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)]] Z

------------
-- bPrMove (B')
--
--    Accepts as input a cube and returns a new cube after applying the B' (Back Prime) move.
--
------------
bPrMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
bPrMove cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)]] Z


------------
-- mMove (M)
--
--    Accepts as input a cube and returns a new cube after applying the M (Middle) move.
--
--    NOTE
--      If an even sized cube (e.g. a 2x2x2) is passed as input then an empyt cube is returned
------------
mMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
mMove cb =  if odd (cbSize cb) then  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [ceiling (fromIntegral(getX (getEndPos cb))/2)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] X
            else array (Position(1,1,1), Position(-1,-1,-1)) [] -- return an empty cube, since there is not a middle layer


------------
-- mPrMove (M')
--
--    Accepts as input a cube and returns a new cube after applying the M' (Middle Prime) move.
--
--    NOTE
--      If an even sized cube (e.g. a 2x2x2) is passed as input then an empyt cube is returned
------------
mPrMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
mPrMove cb =  if odd (cbSize cb) then  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [ceiling (fromIntegral(getX (getEndPos cb))/2)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] X
              else array (Position(1,1,1), Position(-1,-1,-1)) [] -- return an empty cube, since there is not a middle layer

------------
-- eMove (E)
--
--    Accepts as input a cube and returns a new cube after applying the E (Equatorial) move.
--
--    NOTE
--      If an even sized cube (e.g. a 2x2x2) is passed as input then an empyt cube is returned
------------
eMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
eMove cb =  if odd (cbSize cb) then  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [ceiling (fromIntegral(getY (getEndPos cb))/2)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] Y
            else array (Position(1,1,1), Position(-1,-1,-1)) [] -- return an empty cube, since there is not a middle layer


------------
-- ePrMove (E')
--
--    Accepts as input a cube and returns a new cube after applying the E' (Equatorial Prime) move.
--
--    NOTE
--      If an even sized cube (e.g. a 2x2x2) is passed as input then an empyt cube is returned
------------
ePrMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
ePrMove cb =  if odd (cbSize cb) then  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [ceiling (fromIntegral(getY (getEndPos cb))/2)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] Y
            else array (Position(1,1,1), Position(-1,-1,-1)) [] -- return an empty cube, since there is not a middle layer


------------
-- sMove (S)
--
--    Accepts as input a cube and returns a new cube after applying the S (Standing) move.
--
--    NOTE
--      If an even sized cube (e.g. a 2x2x2) is passed as input then an empyt cube is returned
------------
sMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
sMove cb =  if odd (cbSize cb) then  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [ceiling (fromIntegral(getZ (getEndPos cb))/2)]] Z
            else array (Position(1,1,1), Position(-1,-1,-1)) [] -- return an empty cube, since there is not a middle layer


------------
-- sPrMove (S')
--
--    Accepts as input a cube and returns a new cube after applying the S' (Standing Prime) move.
--
--    NOTE
--      If an even sized cube (e.g. a 2x2x2) is passed as input then an empyt cube is returned
------------
sPrMove
  :: Cube  -- input cube
  -> Cube  -- new cube after the application of the move
sPrMove cb =  if odd (cbSize cb) then  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [ceiling (fromIntegral(getZ (getEndPos cb))/2)]] Z
            else array (Position(1,1,1), Position(-1,-1,-1)) [] -- return an empty cube, since there is not a middle layer

------------
-- uMoveI (Ui)
--
--    Accepts as input a cube and returns a new cube after applying the Ui (Up on plane U i) move.
--
------------
uMoveI
  :: Int   -- 1 means the closest to U, 2 means the 2nd closest to U and so on before the E plane
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
uMoveI i cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)], y <- [getY (getEndPos cb) - i], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] Y

------------
-- uPrMoveI (U'i)
--
--    Accepts as input a cube and returns a new cube after applying the U'i (Up Prime on plane U i) move.
--
------------
uPrMoveI
  :: Int   -- 1 means the closest to U, 2 means the 2nd closest to U and so on before the E plane
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
uPrMoveI i cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)], y <- [getY (getEndPos cb) - i], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] Y


------------
-- dMoveI (Di)
--
--    Accepts as input a cube and returns a new cube after applying the Di (Down on plane D i) move.
--
------------
dMoveI
  :: Int   -- 1 means the closest to D, 2 means the 2nd closest to D and so on before the E plane
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
dMoveI i cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)], y <- [getY (getStartPos cb) + i], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] Y

------------
-- dPrMoveI (D'i)
--
--    Accepts as input a cube and returns a new cube after applying the D'i (Down Prime on plane D i) move.
--
------------
dPrMoveI
  ::  Int
  ->  Cube  -- input cube
  ->  Cube  -- new cube after the application of the move
dPrMoveI i cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)], y <- [getY (getStartPos cb) + i], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] Y


------------
-- lMoveI (Li)
--
--    Accepts as input a cube and returns a new cube after applying the Li (Left on plane L i) move.
--
------------
lMoveI
  :: Int 
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
lMoveI i cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getEndPos cb) - i] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] X

------------
-- lPrMoveI (L'i)
--
--    Accepts as input a cube and returns a new cube after applying the L'i (Left Prime on the plane L i) move.
--
------------
lPrMoveI
  :: Int
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
lPrMoveI i cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getEndPos cb) - i] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] X


------------
-- rMoveI (Ri)
--
--    Accepts as input a cube and returns a new cube after applying the Ri (Right on plane R i) move.
--
------------
rMoveI
  :: Int
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
rMoveI i cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb) + i] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] X

------------
-- rPrMoveI (R'i)
--
--    Accepts as input a cube and returns a new cube after applying the R'i (Right Prime on plan R i) move.
--
------------
rPrMoveI
  :: Int
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
rPrMoveI i cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb) + i] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb)..getZ (getEndPos cb)]] X

------------
-- fMoveI (Fi)
--
--    Accepts as input a cube and returns a new cube after applying the Fi (Front on plane F i) move.
--
------------
fMoveI
  :: Int
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
fMoveI i cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getEndPos cb) - i]] Z

------------
-- fPrMoveI (F'i)
--
--    Accepts as input a cube and returns a new cube after applying the F'i (Front Prime on plane F i) move.
--
------------
fPrMoveI
  :: Int
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
fPrMoveI i cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getEndPos cb) - i]] Z

------------
-- bMoveI (Bi)
--
--    Accepts as input a cube and returns a new cube after applying the Bi (Back on plane B i) move.
--
------------
bMoveI
  :: Int
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
bMoveI i cb =  rotateCubePlane cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb) + i]] Z

------------
-- bPrMoveI (B'i)
--
--    Accepts as input a cube and returns a new cube after applying the B'i (Back Prime on plane B i) move.
--
------------
bPrMoveI
  :: Int
  -> Cube  -- input cube
  -> Cube  -- new cube after the application of the move
bPrMoveI i cb =  rotateCubePlaneA cb [(Position(x,y,z), cb!Position(x,y,z)) | x <- [getX (getStartPos cb)..getX (getEndPos cb)] , y <- [getY (getStartPos cb)..getY (getEndPos cb)], z <- [getZ (getStartPos cb) + i]] Z


------------
-- singleMove
--
--    Accepts as input a single cube move and a cube and returns a function that applies the corrsponding move.
--
------------
singleMove
  :: Notation
  -> (Cube -> Cube) -- move funtion
singleMove n = case n of  U   -> uMove
                          U'  -> uPrMove
                          D   -> dMove
                          D'  -> dPrMove
                          F   -> fMove
                          F'  -> fPrMove
                          B   -> bMove
                          B'  -> bPrMove
                          L   -> lMove
                          L'  -> lPrMove
                          R   -> rMove
                          R'  -> rPrMove
                          M   -> mMove
                          M'  -> mPrMove
                          E   -> eMove
                          E'  -> ePrMove
                          S   -> sMove
                          S'  -> sPrMove
                          Ui i  -> uMoveI i 
                          U'i i -> uPrMoveI i 
                          Di i  -> dMoveI i 
                          D'i i -> dPrMoveI i 
                          Li i  -> lMoveI i 
                          L'i i -> lPrMoveI i 
                          Ri i  -> rMoveI i 
                          R'i i -> rPrMoveI i 
                          Fi i  -> fMoveI i 
                          F'i i -> fPrMoveI i 
                          Bi i  -> bMoveI i 
                          B'i i -> bPrMoveI i 


------------
-- listMove
--
--    Accepts as input a list of cube moves and a cube and returns a new cube after applying all the moves from left to right.
--
------------
listMove
  :: Algorithm
  -> Cube
  -> Cube
listMove (m:[]) c = singleMove m c
listMove (m:al) c = listMove al (singleMove m c)
