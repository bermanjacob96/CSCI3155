# L4D9_inClass.python

def f(fy,x):
	'''
	:param fy: should be a funciton from Nothing to Number 
	:param x: should be a boolean
	:return: a number

	Note that fy allows be to lazily pass in the real y that
	I want. I go on to store the call fy() into y so that I
	only evaluate it once if I evaluate it at all.
	'''
	if (x):
		return 2
	else:
		y = fy()
		return y + y

	# less good use of lazy eval
	# return 2 if x else fy() + fy()




def myFy(): return 2*2*2*2*2
print(f(myFy, True))
print (f(myFy, False))

# using a lambda
print(f(lambda: 2*2*2*2*2, True))
print(f(lambda: 2*2*2*2*2, False))


# passing lazily but actually evaluating eagerly...
# the def of `f` still is lazy... 
# but my work is eager as it evaluates the body of the labda before
# 	the func call
x = 2*2*2*2*2
f(lambda: x, True)
f(lambda: x, False)


# and to see why it matters...

import time
import random

def f_lazy_good(fy,x):
	if (x):
		return 2
	else:
		y = fy()
		return y + y

def f_lazy_lame(fy,x):
	if (x):
		return 2
	else:
		return fy() + fy()

def f_eager(y,x):  # to the extent that eager is whatever isn't lazy
	if (x):
		return 2
	else:
		return y + y


def mySpecialCallBack():
	time.sleep(60)
	return 32


print( "Working on f_lazy_good. It will be pretty fast")
print( f_lazy_good(mySpecialCallBack, True) )
print( "Working on f_lazy_lame. It will be pretty fast")
print( f_lazy_lame(mySpecialCallBack, True) )
print( "Working on f_eager. This will take a minute")
n = mySpecialCallBack()
print( f_eager(n,True) )


print( "Working on f_lazy_good. This will take a minute")
print( f_lazy_good(mySpecialCallBack, False) )
print( "Working on f_lazy_lame. This will take a few minutes...")
print( f_lazy_lame(mySpecialCallBack, False) )
print( "Working on f_eager. This will take a minute")
n = mySpecialCallBack()
print( f_eager(n,False) )


# f_lazy_good would be the best* way to write the code if
# 	execution speed needs to be maximized





