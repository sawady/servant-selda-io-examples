data Persona = P String Int

instance Show Persona where
    show (P nom ed) = "{ nombre: " ++ nom ++ ", " ++ "edad: " ++ show ed ++ " }"

echoN :: Int -> String -> IO ()
echoN n str = mapM_ (\_ -> putStrLn str) [1..n]

secuenciar :: [IO ()] -> IO ()
secuenciar [] = return ()
secuenciar (c:cs) =
  do
    c
    secuenciar cs

secuenciar' cs = foldr (\c r -> do c; r) (return ()) cs

porSiempre :: IO () -> IO ()
porSiempre c =
  do
    c
    porSiempre c

main =
    do
      putStrLn "Decime algo y yo hago eco"
      str <- getLine
      putStrLn "Decime cuántas veces"
      strN <- getLine
      putStrLn "Okay"
      let n = read strN
      if n < 0
         then putStrLn "error: me pasaste un número negativo"
         else echoN n str
      
