module Lib where
import Data.List.Split
import Data.List
import Control.Lens
--import qualified Data.Text as T
import Text.Read
import Data.Either

--newtype Pos = Int
data Piece = X | O | Blank deriving (Eq,Read)

instance Show Piece where
    show X = "X"
    show O = "O"
    show Blank = " "

startboard :: [Piece]
startboard = take 9 $ cycle [Blank]

reverseAndFlip :: [Piece] -> [Piece]
reverseAndFlip board = reverse $ concat $ (reverse) <$> chunksOf 3 board
   
showBoard :: Either String [Piece] -> IO ()
showBoard (Right board) = do  
    let boardText = show <$> reverseAndFlip board
    mapM_ putStrLn $ chunksOf 5 $ intercalate "-+-+-" $ (intercalate "|") <$> (chunksOf 3 boardText)
showBoard (Left text) = putStrLn text

placePiece :: [Piece] -> Piece -> Maybe Int -> Either String [Piece]
placePiece board piece position
    | position == Nothing = Left "invalid move, parse error"
    | 1+defPos > length board || defPos < 0 = Left "invalid move, must be in 1-9 range"
    | (board !! defPos == Blank) = Right $ set (element defPos) piece board
    | otherwise = Left "cannot move onto an existing square"
    where defPos = (\(Just a) -> a-1) position

flipPiece :: Piece -> Piece
flipPiece X = O 
flipPiece O = X

victory :: [Piece] -> Piece 
victory [X,X,X,_,_,_,_,_,_] = X
victory [_,_,_,X,X,X,_,_,_] = X
victory [_,_,_,_,_,_,X,X,X] = X
victory [O,O,O,_,_,_,_,_,_] = O
victory [_,_,_,O,O,O,_,_,_] = O
victory [_,_,_,_,_,_,O,O,O] = O  --maybe theres a better way to do this but the patters look
victory [X,_,_,X,_,_,X,_,_] = X  --pretty cool
victory [_,X,_,_,X,_,_,X,_] = X
victory [_,_,X,_,_,X,_,_,X] = X
victory [O,_,_,O,_,_,O,_,_] = O
victory [_,O,_,_,O,_,_,O,_] = O
victory [_,_,O,_,_,O,_,_,O] = O
victory [X,_,_,_,X,_,_,_,X] = X
victory [_,_,X,_,X,_,X,_,_] = X
victory [O,_,_,_,O,_,_,_,O] = O
victory [_,_,O,_,O,_,O,_,_] = O
victory board = Blank

findRight :: (Piece -> Bool) -> Either String [Piece] -> Maybe Piece
findRight bool list = (\(Right x) -> find bool x) list 

gameLoop :: Either String [Piece] -> Piece -> IO()
gameLoop board piece = do 
    let oldboard = (\(Right x) -> x) board
    putStrLn $ concat["player ", show(piece), " move"]
    posstr <- getLine
    let pos = readMaybe posstr :: Maybe Int
    --putStrLn $ show pos
    let newboard = placePiece oldboard piece pos
    showBoard newboard
    if(isLeft newboard)
        then gameLoop board piece
        else
            if ((\(Right x) -> victory x) newboard == Blank) 
                then
                    if (findRight (== Blank) newboard == Just Blank)
                        then gameLoop newboard (flipPiece piece)
                        else putStrLn "it's a draw"
                else putStrLn $ concat["player ", show(piece), " wins !!"]
            

someFunc :: IO ()
someFunc = putStrLn "someFunc"
