def test_while (a: int, b: int) -> int:
	while (a < b):
		a = a + 1
	return a

x: int = 1
y: int = 3
print(test_while(1,3))
