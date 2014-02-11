isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x+n `div` x) `div` 2) (n `div` 2)}

isPrime 1 = False
isPrime k = null [x|x<-[2..isqrt k], k `mod` x == 0]

test2 [] = True
test2 l = (isPrime $ read l)&&(test2 $ take ((length l)-1) l)}

test [] = True
test l@(x:xs) = (isPrime $ read l)&&(test xs)}

list = take 11 $ filter (\l -> (((not).(==1).(`mod`10).read) l)&&(test l)&&(test2 l)&&(read l>7)) (map show [n | n<-[1..],(isPrime.(subtract 48).ord.head.show) n])

--["23","37","53","73","313","317","373","797","3137","3797","739397"]

result = sum $ map read $ take 11 $ filter (\l -> (((not).(==1).(`mod`10).read) l)&&(test l)&&(test2 l)&&(read l>7)) (map show [n | n<-[1..], (isPrime.(subtract 48).ord.head.show) n, (mod n 10)==3||(mod n 10)==7])

--748317