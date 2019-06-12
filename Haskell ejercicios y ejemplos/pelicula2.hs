module Main(main) where

    import Control.Monad 
    import System.IO

    main :: IO ()
    main = do
     res <- menu
     putStrLn ""

    menu :: IO ()
    menu = do
     putStrLn "[1] Jugar \n [2] Instrucciones \n [3] Salir"
     x <- getLine
     if x == '3'
        then do  
            putStrLn "Ok. Ciao"
     else if x == '2'
        then do
            putStrLn "Instrucciones: \n\nLorem ipsum dolor sit amet, at est odio corpora invidunt, ornatus voluptatum ei eos. Id quo partem sapientem gubergren, vim ei dico quidam aperiri. Natum exerci appellantur ne vix, ut mei utamur disputationi. Ut graeci eruditi mea. Minim elitr apeirian ei his, ut falli temporibus vel. Cu ius solum fugit sapientem. Iriure nusquam at eum, mel dicam efficiendi neglegentur te, eum munere complectitur no. \n\nPresiona intro para continuar"
            c <- getLine
            when (c == '\n') $ do
            main
        else
            menu