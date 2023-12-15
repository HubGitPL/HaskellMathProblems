--zadanie 33

-- Funkcja sprawdzająca, czy istnieje całkowite b, spełniające równanie dla danego a i m.
szukajB :: Int -> Int -> Int -> Bool
szukajB a b m =   if (1 / fromIntegral a + 1 / fromIntegral b) > 1 / fromIntegral m
    then szukajB a (b + 1) m
    else if (1 / fromIntegral a + 1 / fromIntegral b) == 1 / fromIntegral m
      then True
      else False

-- Funkcja zwracajaca ilosc rozwiazan dla danego m
zliczIloscRozwiazan :: Int -> Int
zliczIloscRozwiazan m = length [a | a <- [(m+1)..(2*m)], szukajB a a m] 

-- Funkcja znajdująca najmniejszą liczbę naturalną m, dla której liczba rozwiązań jest większa lub równa n
znajdzM :: Int -> Int
znajdzM n = head [m | m <- [1..], zliczIloscRozwiazan m >= n]

main :: IO ()
main = do
    putStrLn "Podaj liczbę naturalną n:"
    input <- getLine
    let n = read input :: Int
        m = znajdzM n
    putStrLn $ "Dla n = " ++ show n ++ ", m = " ++ show m
