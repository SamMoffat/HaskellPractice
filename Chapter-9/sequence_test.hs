main = do
  rs <- sequence [getLine, getLine, getLine]
  print rs

-- mapM print [1,2,3,4,5] === sequence $ map print [1,2,3,4,5]
-- mapM_ drops IO stuff at the end
