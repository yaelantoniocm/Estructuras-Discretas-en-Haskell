module Main(main) where

    import Data.List.Split
    import Data.Char
    import Emojis
    import Control.Monad 
    import System.IO
    import System.Random

    main :: IO ()
    main = do
     res <- menu
     putStrLn ""

    menu :: IO ()
    menu = do
     putStrLn "[1] Jugar \n [2] Instrucciones \n [3] Salir"
     x <- getLine
     if x == "3"
        then do  
            putStrLn "Ok. Ciao"
     else if x == "2"
        then do
            opcion2
     else if x == "1"
        then do
            busca
        else
            menu 

    opcion2 :: IO ()
    opcion2 = do 
     putStrLn "Instrucciones: \n\nLorem ipsum dolor sit amet, at est odio corpora invidunt, ornatus voluptatum ei eos. Id quo partem sapientem gubergren, vim ei dico quidam aperiri. Natum exerci appellantur ne vix, ut mei utamur disputationi. Ut graeci eruditi mea. Minim elitr apeirian ei his, ut falli temporibus vel. Cu ius solum fugit sapientem. Iriure nusquam at eum, mel dicam efficiendi neglegentur te, eum munere complectitur no. \n\nPresiona intro para continuar"
     x <- getChar
     c <- getChar
     if c == '\n'
        then do
           main
        else
           opcion2

    busca :: IO () 
    busca = do
        x <- randomRIO (0,119) :: IO Int
        let y = alea x
        putStrLn $ snd y
        --k <- getLine
        m <- getLine
        let respuesta = ignora $ fst y
        if ignora m == respuesta 
            then do
                putStrLn "\9989"
            else
                putStrLn "\10060"
        
        putStrLn "¿Quieres jugar de nuevo? Da enter si quieres volver a jugar, escribe n minuscula si quieres volver al menu?"
        u <- getLine
        if u == "n" 
            then do
                menu
            else
                busca         

    ignora :: String -> String
    ignora y = map toLower $ filter (\x -> ((x/= ' ') && (x/= '.'))) y  







    

       
   
   

    

    