module Main (main) where
import Lib
import Options.Applicative

data Sample = Sample
  { startpiece :: Bool}

sample :: Parser Sample
sample = Sample
      <$> switch
          (short 'o'
         <> help "start with O instead" )



main :: IO ()
main = main2 =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "use -o to instead start with O"
     <> header "tic tac toe" )

boolToPiece :: Bool -> Piece
boolToPiece False = X
boolToPiece True = O

main2 :: Sample -> IO ()
main2 (Sample h) = do
	putStrLn "7|8|9\n-+-+-\n4|5|6\n-+-+-\n1|2|3"
	gameLoop (Right startboard) (boolToPiece h)
main2 _ = return ()