
import Data.List

shortList = ["alpha", "beta", "delta", "gamma", "lambda"]

letterList = ["ynu", "bpl", "acr", "ktf"]

--conditions
-- no double letters
-- none on the same side

--whether or not a list has two repeated elements next to each other
filterDouble [] = False
filterDouble (x:xs) =
    let filterIt prev [] = False
        filterIt prev (x:xs)
            | prev == x = True
            | otherwise = filterIt x xs
    in filterIt x xs

-- returns all 2 length permutations
flipCombinate lst =
    let combinate [] = []
        combinate (x:xs) =
            [x : [y]| y <- xs] ++ combinate xs
        cmb = combinate lst
    in cmb ++ [reverse x | x <- cmb]

--finds the combinations of a list
superCombo letters =
    let subs = [flipCombinate x | x <- letters]
        feld = foldr1 (++) subs
    in feld

--filters by combos
filterManyDouble [] word = False
filterManyDouble (x:xs) word
    | x `isInfixOf` word = True
    | otherwise = filterManyDouble xs word    

