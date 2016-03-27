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

   Notation(..),

   Plane (..),

   Algorithm (..),
   
   Cube (..),
   getStartPos,
   getEndPos,
   createSolvedCubeCell,
   createSolvedCube,
   rotateList,
   rotateListA,
   rotateCubePlane,
   rotateCubePlaneA,
   uMove,
   uPrMove
   
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
-- * Notation
--
--    Rubiks Cube Notation
--      http://ruwix.com/the-rubiks-cube/notation/
-- *************************

data Notation = U | D | L | R | F | B | M | E | S | U' | D' | L' | R' | F' | B' | M' | E' | S'
             deriving  (Eq,  Ord,  Show,  Read,  Bounded,  Enum)

-- *************************
-- * Algorithm
--
-- *************************             

type Algorithm = [Notation]

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
                                 ++ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], y == getY (getEndPos cb)]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x == getX (getStartPos cb), y `elem`  [getY (getStartPos cb) .. getY (getEndPos cb)]]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], y == getY (getStartPos cb)]                                 
      -- break the list in two: postiitions and cells
      posList = map (fst) newList
      cellList = map (snd) newList
      -- rotate the cell list
      newCellList = rotateList cellList
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
                                 ++ [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], y == getY (getEndPos cb)]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x == getX (getStartPos cb), y `elem`  [getY (getStartPos cb) .. getY (getEndPos cb)]]
                                 ++ reverse [(Position(x,y,z),c) | (Position(x,y,z),c) <- ls, x `elem` [getX (getStartPos cb) .. getX (getEndPos cb)], y == getY (getStartPos cb)]                                 
      -- break the list in two: postiitions and cells
      posList = map (fst) newList
      cellList = map (snd) newList
      -- rotate the cell list
      newCellList = rotateListA cellList
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
-- singleMove
--
--    Accepts as input a single cube move and a cube and returns a new cube after applying the move.
--
------------
--singleMove
--  :: Notation
--  -> Cube
--  -> Cube

------------
-- listMove
--
--    Accepts as input a list of cube moves and a cube and returns a new cube after applying the move.
--
------------
--listMove
--  :: [Notation]
--  -> Cube
--  -> Cube

