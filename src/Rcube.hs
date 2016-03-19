module Rcube
(
   Color (..),
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
   
   Cube (..),
   getStartPos,
   getEndPos,
   createSolvedCubeCell,
   createSolvedCube
   
) where

import qualified Data.Map as Map
import Data.Array
import Data.List

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


--createCube n = array (Position(1,1,1), Position(n,n,n)) [(p, c) | p <- range (Position(1,1,1), Position(n,n,n)), c <- [Just Yellow, Just Red, Just White, Just Orange, Just Blue, Just Green]]

--Array Position(Int, Int, Int),Position(Int, Int, Int)) [(Position(Int, Int, Int), Cell(Maybe Color, Maybe Color, Maybe Color))]
--type Cube = array ((1,1,1), (N,N,N)) ([((x,y,z), C) | x <- [1..N], y <- [1..N], z <- [1..N], c <-  ])           

--data Cube N = Cube  array ((1,1,1), (N,N,N)) ([((x,y,z), C) | x <- [1..N], y <- [1..N], z <- [1..N], c <- [Nothing .. Just Green])

--             (Map.Map(Position,Cell),Map.Map(Position,Cell),Map.Map(Position,Cell)), 
 --                 array(Map.Map(Position,Cell),Map.Map(Position,Cell),Map.Map(Position,Cell)), 
   --               array(Map.Map(Position,Cell),Map.Map(Position,Cell),Map.Map(Position,Cell))) 

{--
let a = array (Position(1,1,1), Position(10,10,10)) [(Position(x,y,z), Cell(a,b,c)) | x <- [1..10], y <- [1..10], z <- [1..10], a <- [Just Yellow , Just Green], b <- [Just Yellow , Just Green], c <- [Just Yellow , Just Green]]
nikos-ghci> :t a
a :: Array Position Cell
--}
