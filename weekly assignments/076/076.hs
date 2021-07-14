import Data.List

gerador1 = 0:concatMap (\x -> [x, -x]) [1..]

gerador2 = concatMap (\x -> [x, negate (x+1)]) [1,3..]

gerador3 = iterate (*2) 1

gerador4 x = takeWhile (>=1) $ map floor $ iterate (/2) x

gerador5 x = map floor $ unfoldr (\y -> if y < 1 then Nothing else Just(y, y/2 ) ) x

digitos = reverse . unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 10, x `div` 10) )