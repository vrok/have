package main

func main() {
	var x = make([]int, 10)
	for i := range x {
		x[i] = i * 2
	}
	for _, e := range x {
		print(e, "\n")
	}
}
