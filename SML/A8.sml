(*
 * These are the standard definitions for map, filter, fold/reduce and zip
 *)
fun map f xs =
    case xs of
        [] => []
      | x::xs' => (f x)::(map f xs')


fun filter f xs =
    case xs of
        [] => []
      | x::xs' => if (f x)
                     then x::(filter f xs')
                     else (filter f xs')


fun fold f acc xs =
  case xs of
    []     => acc
  | x::xs' => fold f (f (acc, x)) xs'


fun zip xs ys =
    case (xs,ys) of
        ([],[]) => []
      | (x::xs', y::ys') => (x,y) :: (zip xs' ys')
      | _ => []


(*
 * These are bindings for individual stocks
 *)

val ibm = ("IBM", 137, 50)
val facebook = ("FB", 106, 10)
val google = ("GOOG", 700, 15)
val apple = ("AAPL", 100, 25)
val microsoft = ("MSFT", 51, 25)
val yahoo = ("YHOO", 33, 25)


(* -------------------------------------------------------------------- *)
(* 
 * 1. Finish the binding for oracle: symbol = "ORCL", price = 38, shares = 12 
 *)

val oracle = ("ORCL", 38, 12)


(* 
 * 2. Create a binding for hewlettPackard: symbol = "HPQ", price = 11, shares = 30
 *)

val hewlettPackard = ("HPQ", 11, 30)

(* -------------------------------------------------------------------- *)


(*
 * A list of all your investments
 *)
val investments = [ibm, facebook, google, apple, microsoft, yahoo, oracle, hewlettPackard]



(*
 * An example of using map to create a list of stock symbols
 *)
fun symbolOf investment =
    case investment of
       (symbol, price, shares) => symbol


fun symbols investments =
    map symbolOf investments


val ans1 = symbols investments


(* -------------------------------------------------------------------- *)
(* 
 * 3. Complete the definition of valueOf to return price * shares
 *)

fun valueOf investment =
    case investment of
       (symbol, price, shares) => price * shares


(*
 * 4. Complete the definition of values to use map to return a list of stock values
 *)
fun values investments = 
    map valueOf investments
    

val ans2 = values investments

(* -------------------------------------------------------------------- *)


(*
 * An example of using filter to create a list of stocks with values greater than $1000
 *)
fun greater_than_1000 investments =
    filter (fn x => (valueOf x) > 1000) investments


val ans3 = greater_than_1000 investments


(* -------------------------------------------------------------------- *)
(*
 * 5. Complete the definition of less_than_20 to use filter to return a list of stocks with less than 20 shares
 *)
fun sharesOf investment = 
    case investment of
       (symbol, price, shares) => shares

fun less_than_20 investments =
    filter (fn x => (sharesOf x) < 20) investments


val ans4 = less_than_20 investments

(* -------------------------------------------------------------------- *)



(*
 * An example of using zip to create a list of tuples: (symbols, values)
 *)
fun symbols_and_values investments =
    let
        val tmp = greater_than_1000 investments
    in
        zip (symbols tmp) (values tmp)
    end


val ans5 = symbols_and_values investments


(* -------------------------------------------------------------------- *)
(*
 * 6. Use zip to create a list of tuples for stocks with less than 20 shares
 *)
fun symbols_and_values2 investments =
    let
        val tmp = less_than_20 investments
    in
        zip (symbols tmp) (values tmp)
    end


val ans6 = symbols_and_values2 investments

(* -------------------------------------------------------------------- *)


(*
 * An example of using fold to add up the value of all stocks
 *)
fun total_value investments =
    fold (fn (x, i) => x + (valueOf i)) 0 investments


val ans7 = total_value investments


(* -------------------------------------------------------------------- *)
(*
 * 7. Use fold to add up the value of all stocks with less than 20 shares
 *)
fun total_value2 investments =
    fold (fn (x, i) => x + (valueOf i)) 0 (less_than_20 investments)



val ans8 = total_value2 investments