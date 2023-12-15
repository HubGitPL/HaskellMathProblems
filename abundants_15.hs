divisors :: Int -> [Int]
divisors n = [x | x <- [1..n `div` 2], n `mod` x == 0]  --check if x is divisors of n number

isAbundant :: Int -> Bool
isAbundant n = sum (divisors n) > n     --sum of the disisors of n number

findNonSumOfAbundant :: Int -> Int
findNonSumOfAbundant n =
  if isAbundant n       --if is abundant do findNonSumOfAbundant (n-1)
  then findNonSumOfAbundant (n-1)
  else
  go n --else is not do go n (12) 12 is a lower limit. Minimal abundant number is 12.
  where
    go :: Int -> Int
    go m
      | m>=20161 = 20161 -- every numbers greater that 20161 is a sum of the two abundants numbers
      | m<24 = m    -- 24 is a lowest number which is sum of the two abundants numbers
      --12 is the lowest abundant number
      | not(isAbundant m) && all (\i -> not (isAbundant i) || not (isAbundant (m - i))) [12..(m `div` 2)] = m --If found two number which is sum of two abundants return m
      | otherwise = go (m-1)   --else do g m-1

main :: IO ()
main = do
  let n = 1000 -- Replace with your desired value of n
  putStrLn $ "Result for n = " ++ show n ++ ": " ++ show (findNonSumOfAbundant n)
