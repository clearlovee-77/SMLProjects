(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		             | Variable of string
		             | UnitP
		             | ConstP of int
		             | TupleP of pattern list
		             | ConstructorP of string * pattern

datatype valu = Const of int
	            | Unit
	            | Tuple of valu list
	            | Constructor of string * valu

fun g f1 f2 p =
    let 
	      val r = g f1 f2 
    in
	      case p of
	          Wildcard          => f1 ()
	        | Variable x        => f2 x
	        | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	        | ConstructorP(_,p) => r p
	        | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	           | UnitT
	           | IntT
	           | TupleT of typ list
	           | Datatype of string

(**** you can put all your code here ****)

(* Q1 *)
fun only_capitals (xs : string list) =
    List.filter (fn (x) => Char.isUpper(String.sub(x,0))) xs

(* Q2 *)
fun longest_string1 (xs : string list) =
    List.foldl (fn (s,x) => if String.size s > String.size x then s else x) "" xs

(* Q3 *)
fun longest_string2 (xs : string list) =
    List.foldl (fn (s,x) => if String.size s >= String.size x then s else x) "" xs

(* Q4 *)
fun greater (x,y) = x >= y
fun sgreater (x,y) = x > y
fun longest_string_helper f xs =
    List.foldl (fn (s,x) => if f ((String.size s),(String.size x)) then s else x) "" xs

val longest_string3 = longest_string_helper sgreater
val longest_string4 = longest_string_helper greater

(* Q5 *)
val cap = Char.isUpper o String.sub
fun longest_capitalized (xs : string list) =
    List.foldl (fn (x,acc) => if String.size x > String.size acc andalso cap(x,0) then x else acc) "" xs

(* Q6 *)
fun rev_string (str : string) =
    (String.implode o List.rev o String.explode) str

(* Q7 *)
fun first_answer f list =
    case list of
        [] => raise NoAnswer
      | x::list' => case f x of
                        SOME v => v
                      | NONE => first_answer f list'

(* Q8 *)
fun help f acc list =
    case list of
        [] => SOME acc
      | x::list' => (case f x of
                         SOME lst => help f (lst @ acc) list'
                       | NONE => NONE)

fun all_answers f list =
    help f [] list

(* Q9 *)
fun f1 () = 1
val f2 = String.size
fun compare x y =
    if x = y
    then 1
    else 0

fun count_wildcards p =
    g f1 (fn (_) => 0) p

fun count_wild_and_variable_lengths p =
    g f1 f2 p

fun count_some_var (str,p) =
    g (fn () => 0) (compare str) p

(* Q10 *)
fun contains strl str =
    case strl of
        [] => false
      | s::strl' => if str = s
                    then true
                    else contains strl' str

fun getStrl p =
    let 
	      val r = getStrl
        val acc = []
    in
	      case p of
	          Variable x        => [x]@acc
	        | TupleP ps         => List.foldl (fn (p,i) => (r p) @ i) acc ps
	        | ConstructorP(_,p) => r p
	        | _                 => acc
    end

fun ifRepeat strl =
    case strl of
        [] => false
      | s::strl' => if contains strl' s
                    then true
                    else ifRepeat strl'

fun check_pat p =
   (not o ifRepeat o getStrl) p

(* Q11 *)
fun match (p : valu * pattern) =
    let fun answer vs ps = all_answers match (ListPair.zip(vs,ps))
                      in
                          case p of
                              (_,Wildcard) => SOME []
                            | (v,Variable s) => SOME [(s,v)]
                            | (Unit,UnitP) => SOME []
                            | (Const v,ConstP s) => if s = v
                                                    then SOME []
                                                    else NONE
                            | (Tuple vs,TupleP ps) => if List.length vs = List.length ps andalso not ((answer vs ps) = NONE)
                                                      then answer vs ps
                                                      else NONE
                            | (Constructor(s2,v),ConstructorP(s1,p)) => if s1=s2 andalso not(match (v,p) = NONE)
                                                                        then SOME[]
                                                                        else NONE
                            | _ => NONE
                      end

(* Q12 *)
fun combine x list =
    case list of
        l::list' => [(x,l)] @ (combine x list')
      | [] => []

fun first_match v plist =
    SOME (first_answer match (combine v plist)) handle NoAnswer => NONE
