module Main exposing (..)
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith l bs cs  = 
    case (bs, cs) of
    ([], _) -> []
    (_, []) -> []
    (b::bss, c::css) -> l b c ::zipWith l bss css

groupBy : (a -> Bool) -> List a -> (List a, List a) 
groupBy n list = (corre n list, nocorre n list)

corre s list = case list of 
    [] -> []
    (b::bss) -> if s b then corre s bss else b:: corre s bss

nocorre s list = case list of 
    [] -> []
    (l::lss) -> if s l then l:: nocorre s lss else nocorre s lss

bind : Maybe a1 -> (a1 -> Maybe a) -> Maybe a
bind ls f = case ls of 
    Nothing -> Nothing 
    Just a -> f a 