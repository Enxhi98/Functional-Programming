

strlen :: IO()
strlen = do
  putStrLn "Enter the username:"
  xs <- getLine
  putStrLn("Username is " ++ xs)
  putStrLn("You have 3 direction to walk: 1-> left, 2-> forward, 3-> right")
  putStrLn("1 has the giftbox to increase the blood;")
  putStrLn("2 has the giftbox to increase the speed")
  putStrLn("3 has the the giftbox to have the weapon")
  
