1+1;
print("hello world!");
val e = "nav";
print("e = " ^ e ^ "\n");

fun pow (x:int, y:int) = 
    if y = 0
    then 1
    else let val temp = x * pow(x, y-1)
        in print ("x = " ^ Int.toString x ^ 
                "\ny = " ^ Int.toString y ^ 
                "\ntemp = " ^ Int.toString temp ^ "\n\n"); 
        temp
    end;

pow(2,3);

(* Exits the process *)
OS.Process.exit(OS.Process.success);