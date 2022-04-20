def test_while (a: int, b: int) -> int:
	# fails because while loop is only taking one statement
  while (a < b):
	print(a)
	a = a + 1
  return a

x: int = 1
y: int = 3
test_while(1,3)
