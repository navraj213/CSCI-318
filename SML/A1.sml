(* 
Assignment
1. Write a function hello_world that takes an integer argument, n, and prints out "hello world" n
times on separate lines. For example, the expression: *)
print("Assignment 1\n");
fun hello_world(n:int) =
    if n = 0 
    then ()
    else (
        print "hello world\n"; 
        hello_world(n-1)
        );

hello_world(5);


(* 2. Modify the function below to print out the value of x, fibonacci(x-1) and fibonacci(x-
2) in every iteration of the function call: *)
print("\nAssignment 2\n");
fun fibonacci (x:int) =
    if (x<2) then (
        print("x = " ^ Int.toString x ^ "\n");
        x
        )
    else
    let val x1 = fibonacci(x-1)
        val x2 = fibonacci(x-2)
    in
        print ("x = " ^ Int.toString x ^ ", f(x-1) = " ^ Int.toString x1 ^ ", f(x-2) = " ^ Int.toString x2 ^ "\n");
        x1 + x2
    end;

fibonacci(5);

