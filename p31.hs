import Data.List

f 3 = [1 | x1<-[1..200], x2<-[1..100], x3<-[1..40], x4<-[1..20], x5<-[1..4], x6<-[1,2], x1+2*x2+5*x3+10*x4+20*x5+50*x6+100==200]

p 0 = [[]]
p n = [(x:xs) | x<-[x|x<-[1..n], (x==1)||(x==2)||(x==5)], xs<-(p (n-x)), (null xs || x<=head xs)]

{--
1 way to sum with all 1's
99 ways to sum with 1's and 2's (you can use between 100 2's [only way to sum with 2's] or 0 2's [way to sum with 1's])
1 way to sum with all 2's
39 ways to sum with 1's and 5's (you can use between 40 5's [only way to sum with 5's] or 0 5's [way to sum with 1's])
1 to sum with all 5's

{
to find out how many ways you can sum using 1,2,5, you would consider each way you can sum with 1's and 2's and see how many different ways you can incorporate 5's to complete the sum

1 + 1 + ..... + 1 + 2 - > (1 + 1 +..+ 1) + (1 + 1 + 1 + 2) = 195 + 5 so how many ways can we break 195 into sum of 5's? 39
1 + 1 + ..... + 2 + 2 - > (1 + 1 +..+ 1) + (1 + 2 + 2) = 195 + 5, 39
1 + 1 + ...+ 2 + 2 + 2 -> (1 + 1 +..+ 1) + (1+1+1+1+2+2+2) = 190 + 10, 38
<1>, 4<2> -> (1+...) + (1+1+2+2+2+2) = 190+10, 38
<1>, 5<2> -> (1+...) + (2x5) = 190 + 10, 38
<1>, 6<2> -> (1+...) + (1+1+1+(2x6)) = 185 + 15, 37
37
36
36
36
35*2
34*3
S_{i=1,20} 2*(2i-1) = 4Si - S2 = 4*(n+1)*n/2 - 2n = 840 - 40 = 800
S_{i=1,19} 3*(2i) = 6Si = 6*(n+1)*n/2 = 1140

1939

[
an illustrative example with 20, which has 29 total integer partitions using 1,2,5

1 ways to sum with 1's
9 ways to sum with 1's and 2's
1 ways to sum with 2's
4 ways to sum with 1's and 5's
1 ways to sum with 5's
--
16

num of 5's used in way with only 5's = 4

15 + 5, 3
15, 3
10, 2
10, 2
10, 2
5, 1
5, 1 <- last one represents ways to do it using only 5's so it doesn't get factored in
--
13

s_{1,2} 2*(2i-1) = 4Si - S2 = 4(n+1)*n/2 - 2n @ n=2 = 12 - 4 = 8
s_{1} 3*(2i) = 4Si = 6(n+1)*n/2 = 6
]

}

so, overall we have
1
99
1
39
1 
1939

--}
folder a [] = a
folder a (x:xs) = folder (a++[(x+(if (null a) then 0 else (last a)))]) xs

{--
* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)
--}

compress' [] a = a
compress' (x:xs) a
	| (x==(head a)) = compress' xs a
	| otherwise = compress' xs (x:a)
	
compress [x] = [x]
compress (x:xs)
	| (x==(head a)) = a
	| otherwise = (x:a)
	where a = (compress xs)

--this function adds the previous element to the current elemen
right l 0 = l
right l count = right [ if (i>0 && (i==((length l)-count))) then (l!!i)+(l!!(i-1)) else (l!!i) | i<-[0..((length l)-1)]] (count-1)

rep n x l = [ if (i==n) then x else l!!i | i<-[0..((length l)-1)] ]

modl l t 0 = l
modl l t count = modl [ if ((i-t)>=0 && i==((length l)-count)) then ((l!!i)+(l!!(i-t))) else (l!!i)| i<-[0..((length l)-1)]] t (count-1)

ans l [] = tail l
ans l (x:xs) = ans (modl l x (length l)) xs 


f' t l x = if (index>0) then (5 + l!!(index-t)) else 2 where index = ((\(Just a)->a) (elemIndex x l))


ntake n skip l | (n==0 || (null l)) = []
ntake n skip l@(x:xs) = x:(ntake n skip (drop skip l))

addlist l = [(sum.(take i)) l | i<-[1..(length l)]]