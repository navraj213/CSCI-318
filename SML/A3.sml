(* 1. Write ML expressions that do the following:
1. Defines a function printValue that takes a tuple consisting of symbol, price and shares as
arguments and prints out the stock symbol and the value of price x shares*)
fun printValue(stock : string * int * int) =
    print( (#1 stock) ^ " : " ^ Int.toString( (#2 stock) * (#3 stock) ) ^ "\n" );

(* 2. Binds the value ("AAPL", 100, 5) to the variable appleStock *)
val appleStock = ("AAPL", 100, 5);

(* 3. Calls the function printValue with the variable appleStock *)
val ans1 = printValue(appleStock);




(* 2. Write ML expressions that do the following: *)
(* 1. Defines a function compareValue that takes two tuples consisting of symbol, price and shares
as arguments and returns the symbol for the stock with the greater value (price x shares). *)
fun compareValue(stock1 : string * int * int, stock2 : string * int * int) =
    if ( (#2 stock1) * (#3 stock1) ) > ( (#2 stock2) * (#3 stock2) ) 
    then
        ((#2 stock1) * (#3 stock1))
    else
        ((#2 stock2) * (#3 stock2))

(* 2. Binds the value ("IBM", 30, 10) to the variable ibmStock *)
val ibmStock = ("IBM", 30, 10);

(* 3. Calls the function compareValue with appleStock and ibmStock as arguments *)
val ans2 = compareValue(appleStock, ibmStock);




(* 3. Write ML expressions that do the following:
1. Defines a function findMaxValue that takes a list of stock tuples consisting of symbol, price
and shares as arguments and returns the symbol of the stock with the greatest value (price x
shares). *)
(* Make recurisve *)
fun stockValue(stock : string * int * int) =
    (#2 stock) * (#3 stock);

fun findMaxValue(stocks : (string * int * int) list) =
    if (tl stocks) = []
    then (hd stocks)
    else let 
        val tlMax = findMaxValue(tl stocks)
        val tlMaxValue = stockValue(tlMax)
        in
            if stockValue(hd stocks) > tlMaxValue
            then (hd stocks)
            else tlMax
        end;

(* 2. Binds the value ("GOOG", 70, 15) to googleStock *)
val googleStock = ("GOOG", 70, 15);

(* 3. Binds the value ("FB", 20, 8) to facebookStock *)
val facebookStock = ("FB", 20, 8);

(* 4. Creates a list of all four stock tuples (appleStock, ibmStock, googleStock and facebookStock)
bound to stockList *)
val stockList = [appleStock, ibmStock, googleStock, facebookStock];

(* 5. Calls findMaxValue with stockList as its argument *)
val ans3 = findMaxValue(stockList);





(*4. Write ML expressions that do the following:
1. Defines findMaxMalueTwo, which uses compareValue as a nested function*)
fun findMaxMalueTwo(stocks : (string * int * int) list) =
    let
        fun compareValue(stock1 : string * int * int, stock2 : string * int * int) =
            if ( (#2 stock1) * (#3 stock1) ) > ( (#2 stock2) * (#3 stock2) ) 
            then
                ((#2 stock1) * (#3 stock1))
            else
                ((#2 stock2) * (#3 stock2))
    in
        if (tl stocks) = []
        then (hd stocks)
        else let 
            val tlMax = findMaxMalueTwo(tl stocks)
            val tlMaxValue = stockValue(tlMax)
            in
                if stockValue(hd stocks) > tlMaxValue
                then (hd stocks)
                else tlMax
            end
    end;

val ans4 = findMaxMalueTwo(stockList);