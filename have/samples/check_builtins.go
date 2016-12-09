package main

func main() {
	var x = make([]int, 4)

	print(len(x))
	print("\n")

	print(cap(x))
	print("\n")

	x = append(x, 5)
	x = append(x, 6, 7)
	print(len(x))
	print("\n")

	print(x[0])
	print("\n")

	var y = []int{1, 2, 3, 4}
	copy(x, y)

	print(x[0])
	print("\n")

	var z = map[int]string{5: "b"}
	z[10] = "a"

	print(len(z))
	print("\n")

	delete(z, 10)

	print(len(z))
	print("\n")
}
