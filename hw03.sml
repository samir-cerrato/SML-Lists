
(* for testing *)

fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)

fun slToString(l : string list) : string =
    case l of
        [] => "[]"
      | x :: xs => x ^ "::" ^ slToString(xs)

fun islToString(l : (int * string) list) : string =
    case l of
        [] => "[]"
      | (n,s) :: xs => "(" ^ Int.toString n ^ "," ^ s ^ ")" ^ "::" ^ islToString(xs)

fun testisl (s : string) (n : (int * string) list) (m : (int * string) list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ islToString m ^ "\n    Got: " ^ islToString n ^ "\n")

fun testilsl (s : string) ((is,ss) : int list * string list) ((is',ss') : int list * string list) : unit =
    case (is,ss) = (is',ss') of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString is' ^ "," ^ slToString ss' ^ "\n    Got: " ^ ilToString is ^ "," ^ slToString ss ^  "\n")

fun testb (s : string) (n : bool) (m : bool) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Bool.toString m ^ "\n    Got: " ^ Bool.toString n  ^ "\n")

fun testili (s : string) ((is,i) : int list * int) ((is',i') : int list * int) : unit =
    case (is,i) = (is',i') of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString is' ^ "," ^ Int.toString i' ^ "\n    Got: " ^ ilToString is ^ "," ^ Int.toString i ^  "\n")
            
fun testil (s : string) (n : int list) (m : int list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString m ^ "\n    Got: " ^ ilToString n ^ "\n")


(* ---------------------------------------------------------------------- *)
(*Purpose: pairs the nth element from the first list with the nth element of the second list
Example:
zip ([1,2],["a","b"]) = [(1,"a"),(2,"b")]
zip ([2,3],["c","d"]) = [(2,"c"),(3,"d")]
zip ([3,4],["e","f"]) = [(3,"e"),(4,"f")]
zip ([4,5],["g","h"]) = [(4,"g"),(5,"h")]
zip ([5,6],["i","j"]) = [(5,"i"),(6,"j")]*)
fun zip (l1 : int list, l2 : string list) : (int * string) list = 
    case l1 of
        [] => []
        | x :: xs => case l2 of
            [] => []
            | y :: ys => (x,y) :: zip(xs,ys)

(* Tests for zip *)
fun test_zip() =
    (testisl "z1" (zip ([1,2], ["a","b"])) [(1,"a"), (2,"b")];
     testisl "z2" (zip ([2,3], ["c","d"])) [(2,"c"), (3,"d")];
     testisl "z3" (zip ([3,4], ["e","f"])) [(3,"e"), (4,"f")];
     testisl "z4" (zip ([4,5], ["g","h"])) [(4,"g"), (5,"h")];
     testisl "z5" (zip ([5,6], ["i","h"])) [(5,"i"), (6,"j")]
     )

(*Purpose: takes a list of tuples and returns a tuple of lists, where the first list
in the tuple is the list of first elements and the second list is the list of second elements
Example:
unzip [(1,"a"),(2,"b")] = ([1,2], ["a","b"])
unzip [(2,"c"),(3,"d")] = ([2,3], ["c","d"]
unzip [(3,"e"),(4,"f")] = ([3,4], ["e","f"]
unzip [(4,"g"),(5,"h")] = ([4,5], ["g","h"]*)
fun unzip (l : (int * string) list) : int list * string list = 
    case l of 
        [] => ([],[])
        | x :: xs => let val (c,d) = x
                        in let val (cs,ds) = unzip(xs)
                        in (c :: cs, d :: ds)
                        end
                    end

(* Tests for unzip *)
fun test_unzip() =
    (testilsl "u1" (unzip [(1,"a"), (2,"b")]) ([1,2], ["a","b"]);
     testilsl "u2" (unzip [(2,"c"), (3,"d")]) ([2,3], ["c","d"]);
     testilsl "u3" (unzip [(3,"e"), (4,"f")]) ([3,4], ["e","f"]);
     testilsl "u4" (unzip [(4,"g"), (5,"h")]) ([4,5], ["g","h"])
     )



(*Purpose: takes a list and int and computes the pair (tail,total) where:
tail - int list is the tail of l after any and all numbers equal to y at the front of
the list have been removed
total -  int is the total length of the run of numbers equal to y at the front of l.
Example:
lasHelp ([2], 1)) = ([2], 0)
lasHelp ([1,2,3], 4)) = ([1,2,3], 0)
lasHelp ([2,2,6,3], 2)) = ([6,3], 2)
lasHelp ([], 4)) = ([], 0)*)
fun lasHelp (l : int list, y : int) : int list * int = 
    case l of 
        [] => ([], 0)
        | x :: xs => case x = y of
                        true => let val (c,d) = lasHelp(xs,y)
                            in (c,d+1)
                            end
                        | false => let val (c,d) = lasHelp(xs,y)
                            in (x::c, d)
                            end


(* Tests for lasHelp *)
fun test_lasHelp() =
    (testili "help1" (lasHelp ([2], 1)) ([2], 0);
     testili "help2" (lasHelp ([1,2,3], 4)) ([1,2,3], 0);
     testili "help3" (lasHelp ([2,2,6,3], 2)) ([6,3], 2);
     testili "help4" (lasHelp ([], 4)) ([], 0)
     )

(*Purpose: takes an int list and returns an int list with the element being counted and its
multiplicity in the input list
Example:
look_and_say [1] = [1,1]
look_and_say [1,1,2,2,3] = [1,2,2,2,3,1]
look_and_say [] = []
look_and_say [3,4,4] = [3,1,4,2]*)
fun look_and_say (l : int list) : int list = 
    case l of 
        [] => []
        | x :: xs => let val (c,d) = lasHelp(l,x)
                    in x :: d :: look_and_say(c)
                    end

(* Tests for look_and_say *)
fun test_look_and_say() =
    (testil "las1" (look_and_say [1]) [1,1];
     testil "las2" (look_and_say [1,1,2,2,3]) [1,2,2,2,3,1];
     testil "las3" (look_and_say []) [];
     testil "las4" (look_and_say [3,4,4]) [3,1,4,2]
     )


(*Purpose: returns true if and only if the input list has a subset that sums to the target number
Example:
subset_sum ([],0) = true
subset_sum ([1,2,3,4], 5) = true
subset_sum ([4,6,7], 5) = false
subset_sum ([2,4,6], 12) = true
*)
fun subset_sum (l : int list, s : int) : bool = 
    case (l,s) of 
        ([], 0 ) => true
        |([], _ ) => false
        |(x::xs, _) => case subset_sum(xs,s) of 
                        true => true 
                        | false => subset(xs, s-x) 

(* Tests for subset_sum *)
fun test_subset_sum() =
    (testb "ss1" ( subset_sum ([], 0)) true;
     testb "ss2" ( subset_sum ([1,2,3,4], 5)) true;
     testb "ss3" ( subset_sum ([4,6,7], 5)) false;
     testb "ss4" ( subset_sum ([2,4,6], 12)) true
     )

(*helper function for inverse_adjacent function*)
fun add (m : int, n : int) =
  case m of
     0 => n
   | _ => 1 + (add (m - 1, n))

(*helper function for inverse_adjacent function*)
fun mult (m : int, n : int) : int =
   case m of
      0 => 0
    | _ => add(n, (mult (m - 1, n)))

(*Purpose: computes "the inverse of (the product of) adjacent numbers"
Example:
inverse_adjacent 0 = 0.00
inverse_adjacent 3 = 0.75
inverse_adjacent 9 = 0.90
inverse_adjacent 1 = 0.50*)
fun inverse_adjacent(n : int) : real = 
    case n of 
        0 => 0.0
        | _ => (1.0/(real( mult((n),(n+1))))) + inverse_adjacent(n-1)
    
fun show(r : real) : string = Real.fmt (StringCvt.FIX (SOME 20)) r

val s = show(200.0/201.0)

    
    
fun run() =
    (test_zip();
     test_unzip();
     test_lasHelp();
     test_look_and_say();
     test_subset_sum())