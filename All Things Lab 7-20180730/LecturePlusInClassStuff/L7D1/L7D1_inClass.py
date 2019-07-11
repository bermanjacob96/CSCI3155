# L7D1_inClass.py

'''
What shall our linear structure be? We could make our own, but that seems like
it would require a decent amount of effort. Surely python has a linnear structure.

LINNEAR STRUCT:
'''


def foldLeft(l,acc,cb):
	if (len(l) == 0):
		return acc
	else:
		l0 = l[0]
		accp = cb(acc,l0)
		t = l[1:]
		return foldLeft(t,accp,cb)

l = [3,5]
def mySpecialHelper(n1,n2):
	return n1+n2
n = foldLeft(l,0,mySpecialHelper)
print(n)


# Shall we curry the inputs?
# Shall we use lambda or def?


'''
foldLeft seems rather useful, does python already have this built into it?

I (spencer) am not totally sure. I know it has some things like it... consider list comprehension
in place of map...

l = [1,5,20]
lp = [ li + 1 for li in l ]
# lp == [2,6,21]
'''















