(*
1. Write a function count_list with type
int list -> int
that returns the number of items in a list. An item that is repeated is counted each time it
appears in the list. *)
fun count_list(xs : int list) = 
    if null xs
    then 0
    else 1 + count_list(tl xs);

count_list([1, 2, 3, 4, 5]);

(*
2. Write an ML function sum_list with type
int list -> int
that returns the sum of all the elements within a list *)
fun sum_list(xs : int list) = 
    if null xs
    then 0
    else (hd xs) + sum_list(tl xs);

sum_list([1, 2, 3, 4, 5]);

(* 
3. Write a function countdown with the type
int -> int list
that returns a list of numbers from its argument down to 1.
countdown (5) = [5, 4, 3, 2, 1]
countdown (10) = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] *)
fun countdown(n : int) = 
    if n = 1
    then [1]
    else n :: countdown(n - 1);

countdown(5);

(*
4. Write a function countup with type
int * int -> int list
that takes two arguments (start and finish) and returns a list with all the numbers between start
and finish.
countup (1, 10) = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
countup (2, 7) = [2, 3, 4, 5, 6, 7] *)
fun countup(start : int, finish : int) = 
    if start = finish
    then [start]
    else start :: countup(start + 1, finish);

countup(1, 10);

(*
5. Write a function find_last with type
int list -> int
that returns the last element in a list:
findlast [1, 3, 5, 7, 9] = 9
findlast [2, 3, 4, 5] = 5 *)
fun find_last(xs : int list) = 
    if null (tl xs)
    then hd xs
    else find_last(tl xs);

find_last([1, 3, 5, 7, 9]);