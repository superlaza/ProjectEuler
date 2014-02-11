import math

a,b = 1,1
iter, count = 0,0

while iter<1000:
        atemp = 2*b+a
        b = b+a
        a = atemp
        iter+=1
        if int(math.log(a,10))>int(math.log(b,10)):
                count+=1

print count
