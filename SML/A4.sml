(* 1 . Write a function is older that takes two dates and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument. (If the two dates are the same, the result is false.Evaluating a correct solution should generate this binding: *)
fun is_older((y1, m1, d1), (y2, m2, d2)) = 
    if y1 = y2
    then if m1 = 2
        then d1 < d2
        else m1 < m2
    else y1 < y2

val test1 = is_older((1,2,3),(2,3,4)) (*= true *)



(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month. *)
fun number_in_month(dates: (int * int * int) list, month: int) =
    case dates of
        [] => 0
        | (y, m, d)::rest => if m = month
                             then 1 + number_in_month(rest, month)
                             else number_in_month(rest, month)

val test2 = number_in_month([(2012,2,28),(2013,12,1)],2) (*= 1 *)

(* Write a function number_in_months that takes a list of dates and a list of months and returns how many dates in the list are in the given months. *)
fun number_in_months(dates: (int * int * int) list, months: (int) list) =
    case months of
        [] => 0
        | m::rest => number_in_month(dates, m) + number_in_months(dates, rest)

val test3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) (*= 3 *)



(* 3. Write a function get_nth that takes a list of strings and an int n and returns the nth element of the list where the head of the list is 1st. Do not worry about the case where the list has too few elements: your function may apply hd or tl to the empty list in this case, which is okay. *)
fun get_nth(strings: string list, n: int) =
    case n of
        1 => hd(strings)
        | _ => get_nth(tl(strings), n-1)

(* Write a function date_to_string that takes a date and returns a string of the form January 20, 2013 (for example). Use the operator ^ for concatenating strings and the library function Int.toString for converting an int to a string. For producing the month part, do not use a bunch of conditionals. Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a comma following the day and use capitalized English month names: January, February, March, April, May, June, July, August, September, October, November, December. *)
fun date_to_string(date: (int * int * int)) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        val month = get_nth(months, #2 date)
    in
        month ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

val test4 = get_nth(["hi", "there", "how", "are", "you"], 2) (*= "there"*)
val test5 = date_to_string((2013, 6, 1)) (*= "June 1, 2013"*)



(*4. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.*)
fun oldest(dates: (int * int * int) list) =
    case dates of
        [] => NONE
        | date::[] => SOME date
        | date::rest => 
            let
                val oldest_rest = oldest(rest)
            in
                if is_older(date, valOf oldest_rest)
                then SOME date
                else oldest_rest
            end

val test6 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) (*= SOME (2011,3,31)*)
val test7 = oldest([]) (* = NONE *)