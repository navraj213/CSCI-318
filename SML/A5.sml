datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* 5. Write a function card_color which takes an argument of type card and returns its color
(spades and clubs are black, diamonds and hearts are red). *)
fun card_color(c: card) =
    case c of 
        (Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

fun card_color1(c : card) =
    if (#1 c) = Clubs orelse (#1 c) = Spades 
    then Black 
    else Red

fun card_color2(s: suit, r: rank) =
    if s = Clubs orelse s = Spades 
    then Black 
    else Red


(* 6. Write a function card_value, which takes a card and returns its value (numbered cards
have their number as the value, aces are 11, everything else is 10) *)
fun card_value(c: card) =
    case c of 
        (_, Num n) => n
      | (_, Ace) => 11
      | _ => 10


(* 7. Write a function remove_card, which takes a list of cards cs, a card c, and an exception e.
It returns a list that has all the elements of cs except c. If c is in the list more than once, remove
only the first one. If c is not in the list, raise the exception e. You can compare cards with =. *)
fun remove_card(cs: card list, c: card, e: exn) =
   if null cs then raise e
   else if (hd cs) = c then (tl cs)
   else (hd cs) :: remove_card(tl cs, c, e)


(* 8. Write a function all_same_color, which takes a list of cards and returns true if all the cards
in the list are the same color. *)
fun all_same_color(cs: card list) =
   if null cs orelse null (tl cs) then true
   else if card_color(hd cs) = card_color(hd (tl cs)) then all_same_color(tl cs)
   else false

(* 9. Write a function sum_cards, which takes a list of cards and returns the sum of their values. *)
fun sum_cards(cs: card list) =
   case cs of 
       [] => 0
     | (h::t) => card_value(h) + sum_cards(t)