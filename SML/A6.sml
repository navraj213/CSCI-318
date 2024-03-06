(* 1. Defines a datatype investment with two subtypes:
● Stock - a tuple consisting of a symbol (string), price (int) and shares (int)
● Cash - a single int representing an amount of money *)
datatype investment = Stock of string * int * int
                    | Cash of int

(* 2. Defines a function valueOf that takes one investment as its argument and:
● if the investment is a Stock, returns the value of price x shares
● if the investment is Cash, returns the amount of money in cash *)
fun valueOf(inv : investment) =
    case inv of
        Stock(_, price, shares) => price * shares
        | Cash(amt) => amt

(* 3. Binds a Stock investment with values ("AAPL", 100, 5) to the variable appleStock *)
val appleStock = Stock("AAPL", 100, 5)

(* 4. Calls the function valueOf with the variable appleStock *)
val ans1 = valueOf(appleStock)

(* 5. Binds a Cash investment with the value 200 to the variable myCash *)
val myCash = Cash(200)

(* 7. Calls the function valueOf with the variable myCash *)
val ans2 = valueOf(myCash)





(* 2. Write ML expressions that do the following: *)
(* 1. Defines a NegativeValue exception *)
exception NegativeValue

(* 2. Defines a function printValue that takes an investment as an argument and:
● if the investment is a Stock, and price x shares is less than zero, raises a
NegativeValue exception
● otherwise, if the investment is a Stock, prints out the stock symbol and the value of
price x shares
● if the investment is Cash, prints out “Cash = “ and the amount of money in cash *)
fun printValue(inv: investment) = 
    case inv of 
        Stock(symb, price, shares) => if price * shares < 0 then
                                         raise NegativeValue 
                                      else
                                         (print(symb ^ " = " ^ Int.toString (price * shares)))
      | Cash(amt) => (print("Cash = " ^ Int.toString amt))

val ans3 = printValue(appleStock)

(* 3. Write ML expressions that do the following: *)
(* 1. Defines a function compareValue that takes two investments as arguments and returns
true if the first argument is greater than the second, and false otherwise. *)
fun compareValue(inv1: investment, inv2: investment) =
    valueOf(inv1) > valueOf(inv2)

(* 2. Calls the function compareValue with appleStock and myCash as arguments *)
val ans4 = compareValue(appleStock, myCash)

(* 3. Calls the function compareValue with myCash and appleStock as arguments *)
val ans5 = compareValue(myCash, appleStock)





(* 4. Write ML expressions that do the following: *)
(* 1. Defines a function findMaxValue that takes a list of investments and returns the investment
with the greatest value. *)
fun findMaxValue(investments: investment list) =
    case investments of 
        inv::[] => inv
        | inv::rest => let
                            val max_rest = findMaxValue(rest)
                        in
                            if compareValue(inv, max_rest)
                            then inv
                            else max_rest
                        end

(* 2. Binds the value ("GOOG", 70, 15) to googleStock *)
val googleStock = Stock("GOOG", 70, 15)

(* 3. Binds the value ("FB", 20, 8) to facebookStock *)
val facebookStock = Stock("FB", 20, 8)

(* 4. Binds the value (“IBM”, 30, 10) to ibmStock *)
val ibmStock = Stock("IBM", 30, 10)

(* 4. Creates a list of all five investments (appleStock, ibmStock, googleStock,
facebookStock and myCash) bound to investmentList *)
val investmentList = [myCash, appleStock, googleStock, facebookStock, ibmStock]

(* 5. Calls findMaxValue with investmentList as its argument *)
val ans6 = findMaxValue(investmentList)





(* 5. Write ML expressions that do the following:
1. Defines findMaxValueTwo, which uses compareValue as a nested function *)
fun findMaxValueTwo(investments: investment list) = 
    let fun compareValue2(inv1: investment, inv2: investment) =
        valueOf(inv1) > valueOf(inv2)
    in 
        case investments of
            inv::[] => inv
            | inv::rest => let
                                val max_rest = findMaxValue(rest)
                            in
                                if compareValue(inv, max_rest)
                                then inv
                                else max_rest
                            end
    end

val ans7 = findMaxValueTwo(investmentList)