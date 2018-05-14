main = do
  putStrLn "No. of Triangles:"
  line <- getLine
  if null line then do
    return "HAHAH"
    else do
      let triangles = printTriangles (read line)
      if  triangles /= [] then do
        putStrLn $ show triangles
        main
        else do putStrLn "There don't seem to be any right angle triangles that fit this description."
                main

printTriangles :: Integer -> [(Integer, Integer, Integer)]
printTriangles x = [ (a,b,c) | c <- [1..x], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]
