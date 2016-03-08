package main

func main() {
	for i := 1; i <= 100; i++ {
		if i%3 == 0 && i%5 == 0 {
			print("Fizz Buzz\n")
		} else if i%3 == 0 {
			print("Fizz\n")
		} else if i%5 == 0 {
			print("Buzz\n")
		} else {
			print("i\n")
		}
	}
}
