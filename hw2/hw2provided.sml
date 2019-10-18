(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*Qa*)
fun all_except_option(str,slist)=
    case (str,slist) of
        (_,[]) => NONE
      | (s,st::tl) => if same_string(s,st)=false
                      then (case all_except_option(s,tl) of
                                SOME i  => SOME (st::i)
                              | NONE => NONE)
                      else SOME tl

(*Qb*)
fun get_substitutions1(strll, str)=
    case (strll,str) of
        (strl::tl,str) => (case all_except_option(str,strl) of
                               SOME i => i@get_substitutions1(tl,str)
                             | NONE => get_substitutions1(tl,str))
      | ([],str) => []

(*Qc*)
fun get_substitutions2(strll, str)=
    let fun aux(strll,acc) =
            case strll of
                strl::tl => (case all_except_option(str,strl) of
                                 SOME i => aux(tl,i@acc)
                               | NONE => aux(tl,acc))
              | [] => acc
    in
        aux(strll,[])
    end

(*Qd*)
fun similar_names(strll:string list list,fullname) =
    case fullname of
        {first,middle,last} =>
        let fun help(strltl:string list,name)=
                (case strltl of
                     (str::tl) =>
                     help(tl, name@[{first=str,middle=middle,last=last}])
                   | [] => name)
        in
            help(get_substitutions2(strll,first),[fullname])
        end
            

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(*Qa*)
fun card_color(c)=
    case c of
        (Spades,_) => Black
      | (Clubs,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red

(*Qb*)
fun card_value(c)=
    case c of
        (_,Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
      | (_,Ace) => 11
      | (_,Num i) => i

(*Qc*)
fun remove_card(cs,c,e) =
    case (cs) of
        x::tl => if x=c
                 then tl
                 else remove_card(tl,c,e)
      | [] => raise e

(*Qd*)
fun all_same_color(clist)=
    case clist of
        c::(neck::tl) => (card_color(c)=card_color(neck) andalso all_same_color(neck::tl))
      | _ => true

(*Qe*)
fun sum_cards(clist)=
    let fun add(clist,sum)=
            case clist of
                c::tl => add(tl,sum+card_value(c))
              | [] => sum
    in
        add(clist,0)
    end

(*Qf*)
fun score(clist,goal)=
    let val sum = sum_cards(clist)
        val flag = all_same_color(clist)
    in
        if sum>goal andalso flag = false
        then 3*(sum-goal)
        else if sum<=goal andalso flag = false
        then goal - sum
        else if sum>goal andalso flag = true
        then 3*(sum-goal)div 2
        else (goal-sum)div 2
    end

(*Qe*)
fun officiate(clist,mlist,goal)=
    let val hlist = []
        fun continue(hlist,clist,mlist)=
            (case (clist,mlist) of
                 (c::ctl,m::mtl) =>(case m of
                                        Discard c => let val removedlist = remove_card(hlist,c,IllegalMove)
                                                     in
                                                         continue(removedlist,clist,mtl)
                                                     end
                                      | Draw => if sum_cards(c::hlist)>goal
                                                then score(c::hlist,goal)
                                                else continue(c::hlist,ctl,mtl))
               | (_,[]) => score(hlist,goal)
               | ([],m::mtl) => (case m of
                                     Draw => score(hlist,goal)
                                   | Discard c => let val removedlist = remove_card(hlist,c,IllegalMove)
                                                  in
                                                      continue(removedlist,[],mtl)
                                                  end))
    in
        continue(hlist,clist,mlist)
    end
