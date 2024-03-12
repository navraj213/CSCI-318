(* Use these function definitions for the problems below: *)
fun map (f,xs) =
    case xs of
        [] => []
        | first::rest => (f first)::(map(f, rest))

fun filter (f,xs) =
    case xs of
        [] => []
        | first::rest => if f first
                        then first::(filter (f, rest))
                        else filter (f, rest)



(* 1. Write ML expressions that do the following: *)
(* 1. Defines a datatype investment with two subtypes:
● Stock - a tuple consisting of a symbol (string), price (int), shares (int) and date
(int*int*int) of purchase
● Cash - a tuple consisting of an amount (int) of money and date (int*int*int) of
deposit *)
type date = int*int*int
datatype investment = Stock of string * int * int * date
                    | Cash of int * date

(* 2. Defines a function valueOf that takes one investment as its argument and:
● if the investment is a Stock, returns the value of price x shares
● if the investment is Cash, returns the amount of money in cash *)
fun valueOf(inv : investment) =
    case inv of
        Stock(_, price, shares, _) => price * shares
        | Cash(amt, _) => amt

(* 3. Binds a Stock investment with values ("AAPL", 100, 5, (2015, 5, 10)) to the
variable appleStock *)
val appleStock = Stock("AAPL", 100, 5, (2015, 5, 10))

(* 4. Binds a Cash investment with the value (200, (2014, 3, 7)) to the variable myCash *)
val myCash = Cash(200, (2014, 3, 7))

(* 5. Binds the value ("GOOG", 70, 15, (2014, 8, 3)) to googleStock *)
val googleStock = Stock("GOOG", 70, 15, (2014, 8, 3))

(* 6. Binds the value ("FB", 20, 8, (2015, 2, 9)) to facebookStock *)
val facebookStock = Stock("FB", 20, 8, (2015, 2, 9))

(* 7. Binds the value (“IBM”, 30, 10, (2010, 6, 20)) to ibmStock *)
val ibmStock = Stock("IBM", 30, 10, (2010, 6, 20))

(* 8. Creates a list of all five investments (appleStock, ibmStock, googleStock,
facebookStock and myCash) bound to investmentList *)
val investmentList = [appleStock, ibmStock, googleStock, facebookStock, myCash]

(* 9. Uses the map function above to bind a list with the value of all the investments in
investmentList to the variable ans *)
val ans = map(valueOf, investmentList)




(* 2. Write ML expressions that do the following:
1. Defines a function olderThan that takes a date and an investment as arguments and
returns true if the investment is older than the given date, and false otherwise. *)
fun olderThan((y1, m1, d1), (y2, m2, d2)) =
    if y1 = y2
    then if m1 = m2
         then d1 < d2
         else m1 < m2
    else y1 < y2

(* 2. Defines a function olderThanJan_01_205 that takes an investment as an argument and
returns true if the investment is older than January 1, 2015, and false otherwise *)
fun olderThan01_01_15(i) =
    let
        val d1 = (2015, 1, 1)
    in
        case i of 
            Stock(_, _, _, d2) => olderThan(d2, d1)
            | Cash(_, d2) => olderThan(d2, d1)
    end

(* 3. Uses the function filter above to bind a list of all investments older than January 1, 2015
to the variable ans2 *)
val ans2 = filter(olderThan01_01_15, investmentList)




(* 3. Write ML expressions that do the following:
1. Uses the function filter above with an anonymous function to bind a list of all investments
with a value greater than 250 to the variable ans3 *)
val ans3 = filter((fn i => valueOf(i) > 250), investmentList)




(* 4. Write ML expressions that do the following:
1. Uses the map and filter functions to bind a list of the values of all investments older than
January 1, 2015 to the variable ans4 *)
val ans4 = map(valueOf, filter(olderThan01_01_15, investmentList))