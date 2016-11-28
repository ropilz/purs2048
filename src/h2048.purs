{-

h2048
A Haskell implementation of 2048.

Gregor Ulm

last update:
2014-06-18

Please consult the file README for further information
on this program.

-}
module Main (main) where
import Prelude
import Data.Array as Array
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Foldable (sum)
import Data.List (List(..), concat, fromFoldable, length, filter, reverse, transpose, zip, (..), concatMap, (!!), take, drop, mapMaybe, find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))


-- helper functions -----------------------------
foreign import clearConsole :: Eff ( console :: CONSOLE ) Unit
foreign import getChar :: forall c. (Char -> Eff c Unit) -> Eff ( console :: CONSOLE ) Unit
foreign import log :: String -> Eff ( console :: CONSOLE ) Unit
foreign import printf :: Int -> Int -> String

appendList :: forall a. List a -> List a -> List a
appendList a b = concat $ fromFoldable [a, b]
replicate :: forall a. Int -> a -> List a
replicate l a = fromFoldable $ Array.replicate l a
gridIndex :: Int -> Grid -> List Int
gridIndex index grid = fromMaybe (0..3) $ grid!!index
gridCoord :: Tuple Int Int -> Grid -> Int
gridCoord (Tuple row col) grid = fromMaybe 0 $ (gridIndex row grid)!!col


infixl 8 append as ++
infixr 1 Cons as :
type IntTuple = Tuple Int Int


data Move = Up | Down | Left | Right
type Grid = List (List Int)

start :: Eff ( random :: RANDOM ) Grid
start = do grid'  <- addTile $ replicate 4 $ fromFoldable [0, 0, 0, 0]
           addTile grid'
           pure grid'

merge :: List Int -> List Int
merge xs = merged ++ padding
    where padding = replicate (length xs - length merged) 0
          merged  = combine $ filter (notEq 0) xs
          combine (x:y:xs) | x == y = x * 2 : combine xs
                           | otherwise = x  : combine (y:xs)
          combine x = x

move :: Move -> Grid -> Grid
move Left  = map merge
move Right = map (reverse <<< merge <<< reverse)
move Up    = transpose <<< move Left  <<< transpose
move Down  = transpose <<< move Right <<< transpose

getZeroes :: Grid -> List IntTuple
getZeroes grid = filter (\coord -> (gridCoord coord grid) == 0) coordinates
    where singleRow n = zip (replicate 4 n) (0..3)
          coordinates = concatMap singleRow $ 0..3

setSquare :: Grid -> IntTuple -> Int -> Grid
setSquare grid (Tuple row col) val = pre ++ (fromFoldable [mid]) ++ post
    where pre  = take row grid
          mid  = take col (gridIndex row grid) ++ (fromFoldable [val]) ++ drop (col + 1) (gridIndex row grid)
          post = drop (row + 1) grid

isMoveLeft :: Grid -> Boolean
isMoveLeft grid = sum allChoices > 0
    where allChoices = map (length <<< getZeroes <<< flip move grid) directions
          directions = fromFoldable [Left, Right, Up, Down]

printGrid :: Grid -> Eff ( console :: CONSOLE ) Unit
printGrid grid = do 
    clearConsole
    log $ joinWith "\n" $ Array.fromFoldable $ mapMaybe (\ r -> Just $ showRow r) grid

showRow :: List Int -> String
showRow r =  joinWith "" $ Array.fromFoldable $ mapMaybe (\ i -> Just $ printf 5 i) r

moves :: List (Tuple Char Move)
moves = keys (fromFoldable ['w','a','s','d']) ++ keys (fromFoldable ['c','h','t','n'])
    where keys chars = zip chars (fromFoldable [Up, Left, Down, Right])

matchMove :: Char -> Tuple Char Move -> Boolean
matchMove key (Tuple k _) = key == k

captureMove :: (Move -> Unit) -> Eff ( console :: CONSOLE ) Unit
captureMove f = do
    getChar (\ c -> case find (matchMove c) moves of
      Just (Tuple x m) -> do
        let t = f m
        pure t
      Nothing -> unsafePerformEff $ do 
        log "Use WASD or CHTN as input"
        pure $ captureMove f
    )

check2048 :: Grid -> Boolean
check2048 grid = (fromFoldable[]) /= filter (eq 2048) (concat grid)
--                 
addTile :: Grid -> Eff ( random :: RANDOM ) Grid
addTile grid = do
    let candidates = getZeroes grid
    pick <- chooseTuple candidates
    val  <- choose $ fromFoldable [2,2,2,2,2,2,2,2,2,4]
    let new_grid = setSquare grid pick val
    pure new_grid
-- 

choose :: List Int -> Eff ( random :: RANDOM ) Int
choose xs = do
    i <- randomInt 0 (length xs-1)
    pure $ fromMaybe 0 (xs !! i)
    
chooseTuple :: List IntTuple -> Eff ( random :: RANDOM ) IntTuple
chooseTuple xs = do
    i <- randomInt 0 (length xs-1)
    pure $ fromMaybe (Tuple 0 0) (xs !! i)

newGrid :: Grid -> (Grid -> Unit) -> Eff( console :: CONSOLE) Unit
newGrid grid f = do
    captureMove (\ m -> do
      let new_grid = f $ move m grid
      unit)

gameLoop :: Grid -> Unit
gameLoop grid
    | isMoveLeft grid = unsafePerformEff $ do
        printGrid grid
        if check2048 grid
          then log "You won!"
          else do
            newGrid grid (\ new_grid -> do
              if grid /= new_grid
                then unsafePerformEff $ do
                  new <- addTile new_grid
                  pure $ gameLoop new
                else gameLoop grid)
    | otherwise = unsafePerformEff $ do
        printGrid grid
        log "Game over"

main :: Unit
main = do
  let grid = unsafePerformEff $ start
  gameLoop grid
