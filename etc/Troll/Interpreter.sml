structure Interpreter :> Interpreter =
struct

  exception RunError of string*Syntax.pos

  datatype value = VAL of int list
                 | TEXT of string list

  val seed = ref 3415176
  val seed2 = ref 4151763

  fun rand n =
    let
      val s1 = 73 * !seed + 26111
      val s2 = s1 mod 973177
      val max = (973177 div n) * n
      val s3 = 61 * !seed2 + 26111
      val s4 = s3 mod 990001
      val max2 = (990001 div n) * n
    in
      (seed := s2;
       seed2 := s4;
       if s2>=max orelse s4>=max2 then rand n
       else (s2+s4) mod n + 1)
    end handle Overflow => raise RunError ("rand overflow",(n,!seed))

  fun try p = try1 p 30 (* true with probability p *)

  and try1 p n = (* scale p until integral *)
    if n=0 then true
    else
      let
	val p1 = floor (10.0*p)
	val r = rand 10 - 1
      in
	if r<p1 then true
        else if r>p1 then false
        else try1 (10.0*p-Real.fromInt p1) (n-1)
      end

  fun merge [] l2 = l2
    | merge (a::l1) l2 = merge1 a l1 l2

  and merge1 a l1 [] = a :: l1
    | merge1 a l1 (b::l2) =
        if a<=b then a :: merge1 b l2 l1
	else b :: merge1 a l1 l2

  fun member a [] = false
    | member a (b::l2) = a=b orelse member a l2

  fun drop [] l2 = []
    | drop (a::l1) l2 =
        if member a l2 then drop l1 l2 else a :: drop l1 l2

  fun keep [] l2 = []
    | keep (a::l1) l2 =
        if member a l2 then a :: keep l1 l2 else keep l1 l2

  fun drop1 a [] = []
    | drop1 a (b::bs) = if a=b then bs else b :: drop1 a bs

  fun setminus [] l2 = []
    | setminus (a::l1) l2 =
        if member a l2 then setminus l1 (drop1 a l2)
	else a :: setminus l1 l2

  (* randomly pick m out of n elements *)
  fun pick 0 n xs = []
    | pick m n (xs as (x::xs1)) =
        if n<=m then xs
        else if rand n <= m then x :: pick (m-1) (n-1) xs1
        else pick m (n-1) xs1
    | pick _ _ _ = raise RunError ("Bad call to pick",(0,0))

  fun lookup x [] = NONE
    | lookup x ((y,v)::table) =
        if x=y then SOME v else lookup x table

  fun rollDice (decs, dice) = evalExp0 dice [] decs

  and evalExp0 exp table decs =
  let
   fun evalExp exp table =
    case exp of
      Syntax.NUM (n,p) => VAL [n]
    | Syntax.ID (x,p) =>
        (case lookup x table of
	   SOME v => v
	 | NONE => raise RunError ("unknown variable: "^x,p))
    | Syntax.EMPTY => VAL []
    | Syntax.CONC (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
           (VAL v1, VAL v2) => VAL (merge v1 v2)
         | _ => raise RunError ("Args to @ must be collections",p))
    | Syntax.DROP (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
           (VAL v1, VAL v2) => VAL (drop v1 v2)
         | _ => raise RunError ("Args to drop must be collections",p))
    | Syntax.KEEP (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
           (VAL v1, VAL v2) => VAL (keep v1 v2)
         | _ => raise RunError ("Args to keep must be collections",p))
    | Syntax.SETMINUS (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
           (VAL v1, VAL v2) => VAL (setminus v1 v2)
         | _ => raise RunError ("Args to setminus must be collections",p))
    | Syntax.CHOOSE (e1,p) =>
        (case (evalExp e1 table) of
	   VAL [] => raise RunError ("Arg to choose most be non-empty",p)
         | VAL ns => VAL [List.nth (ns, rand (List.length ns) - 1)]
         | _  => raise RunError ("Arg to choose must be a collection",p))
    | Syntax.PICK (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
           (VAL ns, VAL [n]) => VAL (pick n (List.length ns) ns)
         | _ => raise RunError ("The first arg to pick must be a collection, and the second a number",p))
    | Syntax.DIFFERENT (e1,p) =>
        let
          fun noDups [] = []
            | noDups (x::xs) =
                let val ys = noDups xs in (drop [x] ys)@ys end
        in
          case evalExp e1 table of
            VAL v => VAL (noDups v)
          | _ => raise RunError ("Arg to different must be a collection",p)
        end
    | Syntax.PLUS (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [n1], VAL [n2]) => VAL [n1+n2]
	 | _ => raise RunError ("illegal arg to +",p))
    | Syntax.MINUS (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [n1], VAL [n2]) => VAL [n1-n2]
	 | _ => raise RunError ("illegal arg to -",p))
    | Syntax.UMINUS (e1,p) =>
        (case (evalExp e1 table) of
	   VAL [n1] => VAL [~n1]
	 | _ => raise RunError ("illegal arg to -",p))
    | Syntax.TIMES (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [n1], VAL [n2]) => VAL [n1*n2]
	 | _ => raise RunError ("illegal arg to *",p))
    | Syntax.DIVIDE (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [], VAL [n2]) => VAL [0]
	 | (VAL [n1], VAL [n2]) =>
	      if n2=0 then raise RunError ("division by 0",p)
	      else VAL [n1 div n2]
	 | _ => raise RunError ("illegal arg to /",p))
    | Syntax.D (e1,p) =>
        (case (evalExp e1 table) of
	   VAL [n]   =>
	      if n<=0 then raise RunError ("Arg to d or D most be >0",p)
	      else VAL [rand n]
	 | _ => raise RunError ("illegal arg to d or D",p))
    | Syntax.Z (e1,p) =>
        (case (evalExp e1 table) of
	   VAL [n]   =>
	      if n<0 then raise RunError ("Arg to z or Z most be >=0",p)
	      else VAL [rand (n+1) - 1]
	 | _ => raise RunError ("illegal arg to z or Z",p))
    | Syntax.SUM (e1,p) =>
        (case (evalExp e1 table) of
	   VAL v => VAL [List.foldl (op +) 0 v]
	 | _ => raise RunError ("illegal arg to sum",p))
    | Syntax.COUNT (e1,p) =>
        (case (evalExp e1 table) of
	   VAL v => VAL [List.length v]
	 | _ => raise RunError ("illegal arg to count",p))
    | Syntax.LEAST (e1,e2,p) =>
        (case (evalExp e1 table,evalExp e2 table) of
	   (VAL [n], VAL l) =>
	      if n<0 then raise RunError ("Negative arg to least",p)
	      else if List.length l <= n then VAL l
	      else VAL (List.take (l, n))
	 | _ => raise RunError ("illegal arg to least",p))
    | Syntax.LARGEST (e1,e2,p) =>
        (case (evalExp e1 table,evalExp e2 table) of
	   (VAL [n], VAL l) =>
	      if n<0 then raise RunError ("Negative arg to largest",p)
	      else if List.length l <= n then VAL l
	      else VAL (List.drop (l, List.length l - n))
	 | _ => raise RunError ("illegal arg to largest",p))
    | Syntax.MEDIAN (e1,p) =>
        (case (evalExp e1 table) of
           VAL [] => raise RunError ("Can't take median of empty collection",p)
         | VAL vs => VAL [List.nth (vs, List.length vs div 2)]
         | _ => raise RunError  ("Can't take median of text",p))
    | Syntax.MINIMAL (e,p) =>
        (case (evalExp e table) of
	   VAL [] => VAL []
	 | VAL (a::v) => let fun g [] = []
                               | g (b::bs) = if a=b then b:: g bs else []
			 in VAL (g v) end
	 | _ => raise RunError ("illegal arg to minimal",p))
    | Syntax.MAXIMAL (e,p) =>
        (case (evalExp e table) of
	   VAL [] => VAL []
	 | VAL (a::v) => let fun g [] x xs = x::xs
                               | g (b::bs) x xs =
				   if b=x then g bs x (b::xs) else g bs b []
			 in VAL (g v a []) end
	 | _ => raise RunError ("illegal arg to maximal",p))
    | Syntax.HASH (e1,e2,p) =>
        (case (evalExp e1 table) of
	   VAL [n] =>
	      if n<0 then raise RunError ("Negative arg to #",p)
	      else
	        VAL (foldr (fn (a,b) => merge a b) []
	              (List.tabulate
                         (n,
                          (fn x =>
                             case evalExp e2 table of
                               VAL v => v
                             | _ => raise RunError ("illegal arg2 to #",p)))))
	 | _ => raise RunError ("illegal arg1 to #",p))
    | Syntax.AND (e1,e2,p) =>
        (case (evalExp e1 table) of
	   VAL [] => VAL []
         | _      => evalExp e2 table)
    | Syntax.EQ (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [n1], VAL l) => VAL (List.filter (fn x=>n1=x) l)
	 | _ => raise RunError ("illegal arg to =",p))
    | Syntax.NEQ (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [n1], VAL l) => VAL (List.filter (fn x=>n1<>x) l)
	 | _ => raise RunError ("illegal arg to =/=",p))
    | Syntax.LT (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [n1], VAL l) => VAL (List.filter (fn x=>n1<x) l)
	 | _ => raise RunError ("illegal arg to <",p))
    | Syntax.GT (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [n1], VAL l) => VAL (List.filter (fn x=>n1>x) l)
	 | _ => raise RunError ("illegal arg to >",p))
    | Syntax.LE (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [n1], VAL l) => VAL (List.filter (fn x=>n1<=x) l)
	 | _ => raise RunError ("illegal arg to <=",p))
    | Syntax.GE (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [n1], VAL l) => VAL (List.filter (fn x=>n1>=x) l)
	 | _ => raise RunError ("illegal arg to >=",p))
    | Syntax.FROMTO (e1,e2,p) =>
        (case (evalExp e1 table, evalExp e2 table) of
	   (VAL [n1], VAL [n2]) => VAL (List.tabulate (n2-n1+1,fn x=>x+n1))
	 | _ => raise RunError ("illegal arg to ..",p))
    | Syntax.LET (x,e1,e2,p) =>
        evalExp e2 ((x,evalExp e1 table)::table)
    | Syntax.ACCUM (x,e1,e2,continue,p) =>
        (case evalExp e1 table of
           VAL v =>
 	     VAL (List.foldr (fn (a,b) => merge a b) []
		             (iterate v x e1 e2 continue table decs p))
         | _ => raise RunError ("illegal arg to accumulate",p))
    | Syntax.REPEAT (x,e1,e2,continue,p) =>
        (case evalExp e1 table of
           VAL v =>
	     VAL (List.last (iterate v x e1 e2 continue table decs p))
         | _ => raise RunError ("illegal arg to repeat",p))
    | Syntax.FOREACH (x,e1,e2,p) =>
        (case evalExp e1 table of
          VAL v =>
            VAL (foldr (fn (a,b) => merge a b) []
	          (List.map (fn w =>
                               (case evalExp e2 ((x,VAL [w])::table) of
                                  VAL v1 => v1
                                | _ => raise RunError
                                             ("illegal arg2 to foreach",p)))
			    v))
         | _ => raise RunError ("illegal arg1 to foreach",p))
    | Syntax.IF (e1,e2,e3,p) =>
        (case evalExp e1 table of
           VAL [] => evalExp e3 table  (* false *)
	 | VAL _ => evalExp e2 table   (* true *)
         | _ => raise RunError ("illegal arg to if",p))
    | Syntax.CALL (f,args,p) =>
        callFun (f, List.map (fn e => evalExp e table) args, decs, p)
    | Syntax.STRING (ss,p) => TEXT [ss]
    | Syntax.SAMPLE (e,p) =>
        makeText (evalExp e table)
    | Syntax.SAMPLES (e1,e2,p) =>
        (case (evalExp e1 table) of
	   VAL [n] =>
	      if n<0 then raise RunError ("Negative arg1 to '",p)
	      else
                let
                  fun samples 0 = TEXT []
                    | samples 1 = makeText (evalExp e2 table)
                    | samples n = vconcr (makeText (evalExp e2 table),
                                          samples (n-1))
                in
		  samples n
		end
	 | _ => raise RunError ("illegal arg1 to '",p))
    | Syntax.HCONC (e1,e2,p) =>
        hconc (evalExp e1 table, evalExp e2 table)
    | Syntax.VCONCL (e1,e2,p) =>
        vconcl (evalExp e1 table, evalExp e2 table)
    | Syntax.VCONCR (e1,e2,p) =>
        vconcr (evalExp e1 table, evalExp e2 table)
    | Syntax.VCONCC (e1,e2,p) =>
        vconcc (evalExp e1 table, evalExp e2 table)
    | Syntax.QUESTION (prob,p) =>
        if try prob then VAL [1] else VAL []
  in
    evalExp exp table
  end


  and iterate v x e1 e2 continue table decs pos =
        case evalExp0 e2 ((x,VAL v)::table) decs of
          VAL test =>
            v :: (if not (null test) = continue
	          then
                    (case evalExp0 e1 table decs of
                       VAL v1 => iterate v1 x e1 e2 continue table decs pos
                     | _ => raise RunError ("illegal arg to iterator",pos))
                  else [])
        | _ => raise RunError ("illegal arg to iterator",pos)

  and callFun (f, vs, decs, p) =
    case lookup f decs of
      NONE => raise RunError ("Unknown function: "^f,p)
    | SOME (Syntax.Func(pars, body, pos)) =>
        let
          fun zip [] [] = []
            | zip (x::xs) (y::ys) = (x,y) :: zip xs ys
            | zip _ _ = raise RunError ("Wrong number of args to "^f,p)
        in 
          evalExp0 body (zip pars vs) decs
        end
    | SOME (Syntax.Comp(empty, single, union, pos)) =>
        (case vs of
           [VAL v] => compositional (v, empty, single, union, decs, pos)
         | _ => raise RunError ("Wrong number of args to "^f,p))

  and compositional ([], empty, single, union, decs, p) =
        evalExp0 empty [] decs
    | compositional ((x::xs), empty, single, union, decs, p) =
        let
	  val v1 = callFun (single, [VAL [x]], decs, p)
          val v2 = compositional (xs, empty, single, union, decs, p)
        in
	  callFun (union, [v1,v2], decs, p)
        end


   (* text functions *)

   (* converts non-text values into text values *)
  and makeText (VAL v) =
        TEXT [String.concat
	        (List.map
	           (fn n => if n>=0 then Int.toString n ^ " "
                            else "-" ^ Int.toString (~n) ^ " ")
	           v)]
     | makeText text = text
 
  (* make string of n spaces *)
  and spaces 0 = ""
    | spaces n = " " ^ spaces (n-1)

   (* Horisontal concatenation (top aligned) *) 
  and hconc (TEXT ss1,TEXT ss2) =
        let
          val l1 = String.size (hd ss1)
	  val l2 = String.size (hd ss2)
	  val pad1 = spaces l1
	  val pad2 = spaces l2
	  fun hc [] [] = []
	    | hc [] (s2::ss2) = (pad1^s2) :: hc [] ss2
	    | hc (s1::ss1) [] = (s1^pad2) :: hc ss1 []
	    | hc (s1::ss1) (s2::ss2) = (s1^s2) :: hc ss1 ss2
	in
	  TEXT (hc ss1 ss2)
	end
    | hconc (v1, v2) = hconc (makeText v1, makeText v2)
  

  (* left-aligned vertical concatenation *)
  and vconcl (TEXT ss1,TEXT ss2) =
        let
	  val l1 = String.size (hd ss1)
	  val l2 = String.size (hd ss2)
	  val pad = spaces (abs (l1-l2))
        in
          if l1 = l2 then TEXT (ss1 @ ss2)
          else if l1<l2 then
            TEXT (List.map (fn s=>s^pad) ss1 @ ss2)
	  else
            TEXT (ss1 @ List.map (fn s=>s^pad) ss2)
        end
     | vconcl (v1, v2) = hconc (makeText v1, makeText v2)

  (* right-aligned vertical concatenation *)
  and vconcr (TEXT ss1,TEXT ss2) =
	let
	  val l1 = String.size (hd ss1)
	  val l2 = String.size (hd ss2)
	  val pad = spaces (abs (l1-l2))
        in
          if l1 = l2 then TEXT (ss1 @ ss2)
          else if l1<l2 then
            TEXT (List.map (fn s=>pad^s) ss1 @ ss2)
	  else
            TEXT (ss1 @ List.map (fn s=>pad^s) ss2)
        end
     | vconcr (v1, v2) = hconc (makeText v1, makeText v2)

  (* center-aligned vertical concatenation *)
  and vconcc (TEXT ss1,TEXT ss2) =
	let
	  val l1 = String.size (hd ss1)
	  val l2 = String.size (hd ss2)
          val l3 = abs (l1-l2)
	  val padl = spaces (l3 div 2)
          val padr = spaces (l3 - l3 div 2)
        in
          if l1 = l2 then TEXT (ss1 @ ss2)
          else if l1<l2 then
            TEXT (List.map (fn s=>padl^s^padr) ss1 @ ss2)
	  else
            TEXT (ss1 @ List.map (fn s=>padl^s^padr) ss2)
        end
     | vconcc (v1, v2) = hconc (makeText v1, makeText v2)

end
