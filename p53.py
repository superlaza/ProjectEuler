import math

digits = 7

def d(n):
	sum = 0
	for k in range(0,int(math.ceil(n/2.0))):
		sum += math.log(1+math.floor(n/2)/(k+1),10)
	return sum+1
def above(n):
        r = 0
        m = math.floor(n/2)
        a = d(n)+math.log((m-r)/(n-m+r+1), 10)
        r +=1 
        while a>=digits:
                a = a+math.log((m-r)/(n-m+r+1), 10)
                #print  a, r+1, math.log((m-r)/(n-m+r+1), 10)
                r += 1
        return r-1

count = 0
for n in range (1, 10100):
        if d(n) >= 7:
                if n%2 == 0:
                        count += 2*above(n)+1
                        #print n, 2*above(n)+1,"above is ", above(n)
                else:
                        count += 2*above(n)+2
                        #print n, 2*above(n)+2

print count
