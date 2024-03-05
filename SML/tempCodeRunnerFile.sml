fun printValue(inv: investment) = 
    case inv of 
        Stock(symb, price, shares) => if price * shares < 0 then
                                         raise NegativeValue 
                                      else
                                         print(Int.toString (price * shares))
      | Cash(amt) => print("Cash = " ^ Int.toString amt)



printValue(appleStock)