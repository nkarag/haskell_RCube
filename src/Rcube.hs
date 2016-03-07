module Rcube
(
   Color (..),
   Cell (..),
   
   Position (..),
   isCorner,
   
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
-- *************************
data Color = Yellow | Red | White | Orange | Blue | Green 
             deriving  (Eq,  Ord,  Show,  Read,  Bounded,  Enum)

-- *************************
-- * Cell
-- *************************
data Cell = Cell (Maybe Color, Maybe Color, Maybe Color)
            deriving  (Eq,  Ord,  Show,  Read)

-- *************************
-- * Position
-- *************************
data Position = Position (Int, Int, Int)
                deriving  (Eq,  Ord,  Show,  Read, Ix)
--type Position = Ix Int            

--
-- getX
--
getX :: Position -> Int
getX (Position(x,_,_)) = x

--
-- getY
--
getY :: Position -> Int
getY (Position(_,y,_)) = y

--
-- getZ
--
getZ :: Position -> Int
getZ (Position(_,_,z)) = z

-----------------
--  NOTES:
-- Assume a cube c nxnxn
-- For each association of the form: (Position(x,y,z), Cell(a,b,c))
-- the following constraints hold:
-- If ALL out of x,y,z IN [1,n] then c = "corner cell" i.e., it has 3 colors
--  if ONLY ONE out of x,y,z NOT IN [1,n] then c = "edge cell" i.e., it has only 2 colors
--  if ONLY ONE out of x,y,z IN [1,n] then c = "center cell", i.e., it has only 1 color
--  If ALL out of x,y,z NOT IN [1,n] then c = "internal cell" i.e., it has no colors
--
-----------------

--
-- isCorner 
-- Returns True if a Position corresponds to a corner in the Cube
--
isCorner :: Cube -> Position -> Bool
isCorner c pos = if pos `elem` allCombinationsStartEnd (getStartPos c) (getEndPos c) then True else False
-- isCorner c pos(x,y,z) = if (x, y, z) `elem` allCombinationsStartEnd
-- -- if x 'elem' [startX, endX] && y `elem` [startY, endY] && z `elem` [startZ, endZ] then True else False
   -- where startX = getX $ getStartPos c
         -- startY = getY $ getStartPos c
       -- startZ = getZ $ getStartPos c
       -- endX = getX $ getEndPos c
       -- endY = getY $ getEndPos c
       -- endZ = getZ $ getEndPos c
       -- allCombinationsStartEnd = zip3 (take 4 [startX,startX..] ++ take 4 [endX,endX..]) (take 8 $ cycle [startY,startY,endY,endY]) (take 8 $ cycle [startY,endY])

-- isEdge :: Position -> Bool

-- isCenter :: Position -> Bool

--
-- isInternal 
-- Returns True if a Position corresponds to an internal position in the Cube
--
--isInternal :: Position -> Bool
-- isInternal c pos(x,y,z) = if x `notElem` [startX, endX] && y `notElem` [startY, endY] && z `notElem` [startZ, endZ] then True else False
   -- where startX = getX $ getStartPos c
         -- startY = getY $ getStartPos c
       -- startZ = getZ $ getStartPos c
       -- endX = getX $ getEndPos c
       -- endY = getY $ getEndPos c
       -- endZ = getZ $ getEndPos c

--
-- allCombinationsStartEnd
-- Get a start and an end Position and returns a list of Positions corresponding to all posible combinations of the coordinates
-- of the input Positions.
-- Example: 
--    allCombinationsStartEnd Position(1,1,1) Position(3,3,3) = [Position(0,0,0),Position(0,0,3),Position(0,3,0),Position(0,3,3),Position(3,0,0),Position(3,0,3),Position(3,3,0),Position(3,3,3)]
-- Note: this is different from the "range" function of Type Class  Ix which returns ALL positions between the two end points.
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

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && (not (x && y))

--
-- getStartPos
--    Returns the starting position of a Cube
--
getStartPos :: Cube -> Position
getStartPos c = fst $ bounds c

--
-- getEndPos
--    Returns the ending position of a Cube
--
getEndPos :: Cube -> Position
getEndPos c = snd $ bounds c


-- !!!CHANGE !!! input paramters tfrom Int -> Int to Position -> Position

--------------------------------------------------------------------------------------------------
--  createSolvedCubeCell:
--    Description: Receive as input a specific position and return the appopriate (Position, Cell) association
--      that corresponds to a solved cube
--
--    Input Parameters:
--       s -> the start index value of the Position (typically this is 1)
--       e -> the end index value of the Position (e.g. for a 3x3x3 cube this is 3)
--       pos -> the Position in question
--    Output:
--       the (pos, Cell) association of a solved cube
--
-----------------
--  NOTES:
-- Assume a cube c nxnxn
-- For each association of the form: (Position(x,y,z), Cell(a,b,c))
-- the following constraints hold:
-- If ALL out of x,y,z IN [1,n] then c = "corner cell" i.e., it has 3 colors
--  if ONLY ONE out of x,y,z NOT IN [1,n] then c = "edge cell" i.e., it has only 2 colors
--  if ONLY ONE out of x,y,z IN [1,n] then c = "center cell", i.e., it has only 1 color
--  If ALL out of x,y,z NOT IN [1,n] then c = "internal cell" i.e., it has no colors
--
--------------------------------------------------------------------------------------------------
-- createSolvedCubeCell :: Position -> Position -> Position -> Cell -- -> (Position, Cell)
createSolvedCubeCell :: Int -> Int -> Position -> Cell -- -> (Position, Cell)
createSolvedCubeCell s e pos  
            -- corner cells
            | pos == Position (s,s,s) = Cell (Just Blue, Just Red, Just Yellow) --(pos, Cell (Just Blue, Just Red, Just Yellow))
            | pos == Position (s,s,e) = Cell (Just Green, Just Red, Just Yellow) 
            | pos == Position (s,e,s) = Cell (Just Blue, Just Red, Just White)   
            | pos == Position (s,e,e) = Cell (Just Green, Just Red, Just White)           
            | pos == Position (e,s,s) = Cell (Just Blue, Just Orange, Just Yellow)                    
            | pos == Position (e,s,e) = Cell (Just Green, Just Orange, Just Yellow)                            
            | pos == Position (e,e,s) = Cell (Just Blue, Just Orange, Just White)                                       
            | pos == Position (e,e,e) = Cell (Just Green, Just Orange, Just White)                                               
         -- edge cells
   --         | pos == Position (s,s,z) && not(z `elem` [s,e]) = Cell (Nothing, Just Red, Just Yellow)                                          
         -- center cells
         -- internal cells
            | otherwise = Cell (Nothing, Nothing, Nothing) --(pos, Cell (Nothing, Nothing, Nothing))
 
createSolvedCube :: Int -> Cube
createSolvedCube n = listArray (posStart, posEnd) $ map (createSolvedCubeCell 1 n) $ range (posStart, posEnd)
      where posStart = Position(1,1,1)
            posEnd = Position(n,n,n)


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
