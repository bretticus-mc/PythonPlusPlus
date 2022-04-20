# The GCD algorithm in PythonPP
def gcd (a: int, b: int) -> int:
  while (a != b):
    if (b < a):
		a = a - b
    else:
		b = b - a
  return a

a: int = 18
b: int = 9
x: int = 2
y: int = 14
print(gcd(x,y))
print(gcd(3,15))
print(gcd(99,121))
print(gcd(a,b))
