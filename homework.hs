-- Nguyễn Kim Đại Nghĩa --
-- 2212229 --

separateSigns :: [Int] -> ([Int], [Int])
separateSigns xs = (am, duong)
    where
        am = [x | x <- xs, x < 0]
        duong = [x | x <- xs, x > 0]

transform :: [Int] -> [Int]
transform xs = [x * 3 | x <- xs, even x]

specialNumbers :: [Int]
specialNumbers = [x | x <- [1..100], x `mod` 7 == 0, x `mod` 5 /= 0]

pairsWithSum :: Int -> [Int] -> [(Int, Int)]
pairsWithSum total xs = [(x, y) | x <- xs, y <- xs, x < y, x + y == total]

main :: IO ()
main = do
    let numbers = [-100, -90, 0, 5, 104542, -1, 1, -20, 100000]
    let (am, duong) = separateSigns numbers
    putStrLn "So am"
    print am
    putStrLn "So duong"
    print duong
    
    let ip = [1,2,3,4,5,6,7,8,9,10,13,16,88,100,99]
    putStrLn "out tranform"
    print $ transform ip
    
    putStrLn "Special numbers:"
    print specialNumbers
    
    let total = 13
    let num = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, -1, -2, -3,-4]
    putStrLn $ "Ket qua cac bo so co tong bang " ++ show total 
    print $ pairsWithSum total num
