def add_five(num: *int) -> None:
    *num = *num + 5

x: *int = 10
add_five(x)
print(*x)