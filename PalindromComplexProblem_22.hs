isPalindrome :: Integer -> Bool
isPalindrome n = let strN = show n in strN == reverse strN

largestPalindromeProduct :: Integer -> Integer
largestPalindromeProduct n = maximum [x * y | x <- candidates, y <- candidates,isPalindrome (x * y)]
  where
    start = 10 ^ (n - 1)
    end = 10 ^ n - 1
    candidates = [start..end]

main :: IO ()
main = do
  let number = 3
  let result = largestPalindromeProduct number
  putStrLn $ "Największy palindrom dla n=" ++ show number ++": " ++ show result
