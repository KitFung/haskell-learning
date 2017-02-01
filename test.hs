exOr :: Bool -> Bool -> Bool
exOr a b
    | a == True && b == False = True
    | b == True && a == False = True
    | otherwise = False
