structure Distribution :> Distribution =
struct

  type value = Interpreter.value (* was int list *)
 
  type pValue = value * real

  exception DistribError of string*Syntax.pos
  exception Recursive


  (* maximum number of iterations of accummulate *)

  val maxiterations = ref 12
  val maxcalls = ref 12

  (* look up key in list *)

  fun lookup x [] = NONE
    | lookup x ((y,v)::table) =
        if x=y then SOME v else lookup x table

  (* a number of auxiliary functions *)

  (* list membership *)

  fun member x [] = false
    | member x (y::ys) = x=y orelse member x ys

  (* merge two sorted lists of integers *)

  fun mergeI [] l2 = l2
    | mergeI l1 [] = l1
    | mergeI (l1 as (a::l11)) (l2 as (b::l22)) =
        (case Int.compare (a,b) of
           LESS    => a :: mergeI l11 l2
	 | EQUAL   => a :: b :: mergeI l11 l22
         | GREATER => b :: mergeI l1 l22)

  (* sort a list *)

  fun mergeSort [] = []
    | mergeSort [n] = [n]
    | mergeSort (a::b::l) = splitAndSort l [a] [b]

  and splitAndSort [] l1 l2 = mergeI (mergeSort l1) (mergeSort l2)
    | splitAndSort [a] l1 l2 = mergeI (mergeSort (a::l1)) (mergeSort l2)
    | splitAndSort (a::b::l) l1 l2 = splitAndSort l (a::l1) (b::l2)

  (* comparison of lists of integers *)

  fun orderList [] [] = EQUAL
    | orderList []  (b::l2) = LESS
    | orderList (a::l1)  [] = GREATER
    | orderList (a::l1) (b::l2) =
        if a<b then LESS
	else if a=b then orderList l1 l2
	else GREATER

  (* comparison of lists of strings *)

  fun orderStringList [] [] = EQUAL
    | orderStringList []  (b::l2) = LESS
    | orderStringList (a::l1)  [] = GREATER
    | orderStringList (a::l1) (b::l2) =
        (case String.compare (a,b) of
           LESS => LESS
	 | EQUAL => orderStringList l1 l2
	 | GREATER => GREATER)

  (* comparison of Interpreter.value *)

  fun orderVal (Interpreter.VAL v) (Interpreter.VAL w) = orderList v w
    | orderVal (Interpreter.VAL v) (Interpreter.TEXT w) = LESS
    | orderVal (Interpreter.TEXT v) (Interpreter.VAL w) = GREATER
    | orderVal (Interpreter.TEXT v) (Interpreter.TEXT w) =
        orderStringList v w

  (* datatype for distribution expressionss *)

  datatype dist = VAL of value  (* only possible value *)
                | CHOICE of real * dist * dist  (* probabilistic choice *)
                | UNION of dist * dist (* union *)
                | TWICE of dist (* TWICE d == UNION (d,d) *)
		| BOTTOM  (* No possible value or iteration limit exceeded *)
		| STAR of real * dist * dist (* accumulating loop *)
		(* STAR (p,d1,d2) == CHOICE (p,d1,UNION(d2,STAR(p,d1,d2))) *)

  (* print distribution expression (for testing purposes) *)

  fun printD (VAL v) = "VAL " ^ printVal v
    | printD (CHOICE (p,d1,d2)) = "CHOICE(" ^ Real.toString p ^ ","
                                  ^ printD d1 ^ "," ^ printD d2 ^ ")"
    | printD (UNION (d1,d2)) =  "UNION(" ^ printD d1 ^ ","^ printD d2 ^ ")"
    | printD (TWICE d) = "TWICE("^ printD d ^")"
    | printD BOTTOM = "BOTTOM"
    | printD (STAR (p,d1,d2)) = "STAR(" ^ Real.toString p ^ ","
                                  ^ printD d1 ^ "," ^ printD d2 ^ ")"

  and printL [] = "]"
    | printL [a] = Int.toString a ^ "]"
    | printL (a::l) = Int.toString a ^ "," ^ printL l


  and printVal (Interpreter.VAL v) = "[" ^ printL v
    | printVal (Interpreter.TEXT ts) =
        "\"" ^ String.concat (List.map (fn t=> t ^"\\n") ts) ^ "\""


  fun printDist d = (TextIO.output (TextIO.stdErr,printD d ^ "\n"); d)

  (* build CHOICE (p,d1,d2) with optimisations *)
  (* if d1 and d2 are both normalized, result must also be *)

  fun choice (1.0, d1, d2) = d1
    | choice (0.0, d1, d2) = d2
    | choice (p, BOTTOM, BOTTOM) = BOTTOM
    | choice (p, BOTTOM, d2) = choice (1.0-p, d2, BOTTOM)
    | choice (p, CHOICE (q,d1,d2), BOTTOM) =
        choice (p*q, d1, choice((p-p*q)/(1.0-p*q), d2, BOTTOM))
    | choice (p, d1 as (VAL v), d2 as (VAL w)) =
        (case orderVal v w of
           EQUAL => d1
         | LESS  => CHOICE (p, d1, d2)
         | GREATER => CHOICE (1.0-p, d2, d1))
    | choice (p, d1 as (VAL  v),
	         d2 as (CHOICE (p1, d3 as (VAL w), d4))) =
        (case orderVal v w of
           EQUAL => choice (p+p1-p*p1, d1, d4)
         | LESS  => CHOICE (p, d1, d2)
         | GREATER => let
                        val q = (1.0-p)*p1
                      in
                        choice (q, d3,
				choice (p/(1.0-q), d1, d4))
                      end)
    | choice (p, d1, d2 as (VAL w)) = choice (1.0-p, d2, d1)
    | choice (p, d1 as (CHOICE (p1,d2 as (VAL v),d3)),
                 d4 as (CHOICE (p2,d5 as (VAL w),d6))) =
        (case orderVal v w of
           EQUAL => let
                      val q = p*p1+(1.0-p)*p2
                    in
                      CHOICE (q, d2,
                              choice (p*(1.0-p1)/(1.0-q), d3, d6))
                    end
         | LESS  => CHOICE (p*p1, d2,
                            choice (p*(1.0-p1)/(1.0-p*p1), d3, d4))
         | GREATER => choice (1.0-p, d4, d1))
    | choice (p, d11 as (UNION (d1,d2)),
                 d22 as (CHOICE (p2, UNION (d3,d4), d5))) =
         if d1=d3 then
           choice (p+p2-p*p2,
                   UNION (choice (p/(p+p2-p*p2), d2, d4), d1),
                   d5)
         else if d1=d4 then
           choice (p+p2-p*p2,
                   UNION (choice (p/(p+p2-p*p2), d2, d3), d1),
                   d5)
         else if d2=d3 then
           choice (p+p2-p*p2,
                   UNION (choice (p/(p+p2-p*p2), d1, d4), d2),
                   d5)
         else if d2=d4 then
           choice (p+p2-p*p2,
                   UNION (choice (p/(p+p2-p*p2), d1, d3), d2),
                   d5)
         else CHOICE (p, d11, d22)
    | choice (p, d as (CHOICE (p1,d1,d2)), d3) =
         if d=d3 then d
         else if d1=d3 then choice(p*p1-p+1.0, d1, d2)
         else if d2=d3 then choice(p*p1, d1, d2)
         else choice (p*p1, d1, choice ((p-p*p1)/(1.0-p*p1), d2, d3))
    | choice (p, d1, d as (CHOICE (p1,d2,d3))) =
        if d1=d then d1
        else if d1=d2 then choice (p+p1-p*p1, d1, d3)
        else if d1=d3 then choice (p-p1+p*p1, d1, d2)
        else CHOICE (p,d1,d)
    | choice (p,d1,UNION (d2,d as STAR (q,d3,d4))) =
        if p=q andalso d1=d3 andalso d2=d4 then d
	else CHOICE (p,d1,UNION (d2,d))
    | choice (p, d1, d2) =
        if d1=d2 then d1
        else CHOICE (p, d1, d2)

  (* build UNION (d1,d2) with optimisations *)

  fun union (VAL (Interpreter.VAL []),d)     = d
    | union (d, VAL (Interpreter.VAL []))    = d
    | union (VAL (Interpreter.VAL v),
	     VAL (Interpreter.VAL w)) =
        VAL (Interpreter.VAL (mergeI v w))
    | union (BOTTOM, d)    = BOTTOM
    | union (d, BOTTOM)    = BOTTOM
    | union (d1 as UNION (d3,d4), d2 as UNION (d5,d6)) =
        if d1=d2 then TWICE d1
	else if d3=d2 then union (d4,twice d2)
	else if d4=d2 then union (d3,twice d2)
	else if d5=d1 then union (d6,twice d1)
	else if d6=d1 then union (d5,twice d1)
	else if d3=d5 then union (union (d4,d6), twice d3)
	else if d4=d5 then union (union (d3,d6), twice d4)
	else if d3=d6 then union (union (d4,d5), twice d3)
	else if d4=d6 then union (union (d3,d5), twice d4)
	else union (d3, union (d4, d2))
    | union (d1,d2) = UNION (d1,d2)

  (* build TWICE d with optimisations *)
  and twice (VAL (Interpreter.VAL v)) = VAL (Interpreter.VAL (mergeI v v))
    | twice BOTTOM  = BOTTOM
    | twice d       = TWICE d

  (* build STAR (p,d1,d2) with optimisations *)
  fun star (0.0,d1,d2) = BOTTOM
    | star (1.0,d1,d2) = d1
    | star (p,d1,VAL (Interpreter.VAL [])) = d1
    | star (p,d1,d2) = STAR (p,d1,d2)

  (* combine two UNION-free distribution expressions with function g *)

  fun unionWith d1 d2 g pos =
    let
       fun uw (VAL v) (VAL w) = g (v, w)
	 | uw BOTTOM _ = BOTTOM
	 | uw _ BOTTOM = BOTTOM
         | uw (d0 as (VAL v)) (CHOICE (p,d1,d2)) =
             choice (p, uw d0 d1, uw d0 d2)
         | uw (d0 as (CHOICE (p,d1,d2))) d3 =
	     choice (p, uw d1 d3, uw d2 d3)
         | uw _ _ =
             raise DistribError ("Singleton expected", pos)
    in
      uw d1 d2
    end

  (* twiceWith d g pos == unionWith d d g pos *)

  and twiceWith d g pos =
    let
      fun tw (VAL v) = g (v, v)
        | tw (CHOICE (p,d1,d2)) =
            choice (p*p,
                    tw d1,
                    choice((1.0-p)*(1.0-p)/(1.0-p*p),
                       tw d2,
                       unionWith d1 d2 g pos))
        | tw BOTTOM = BOTTOM
        | tw _ =
        raise DistribError ("internal error twiceWith",pos)
      in
        tw d
      end

(* was: 

  fun starWith p d1 d2 g 0 pos = BOTTOM
    | starWith p d1 d2 g i pos =
        choice (p,
		d1,
		unionWith d2 (starWith p d1 d2 g (i-1) pos) g pos)
*)

  fun starWith1 p d1 d2 d3 g 0 pos = BOTTOM
    | starWith1 p d1 d2 d3 g i pos =
        choice(p,
	       d3,
	       starWith1 p d1 d2 (unionWith d2 d3 g pos) g (i-1) pos)

  fun starWith p d1 d2 g i = starWith1 p d1 d2 d1 g i

  (* apply homomorphic function f to distribution expression *)
  (* using f(a U b) = g(f(a),f(b)) *)
  (* Note: the f and g used as arguments below *)
  (* must return a distribution expression, i.e., VAL v instead of v *)

  fun homomorphic f g d pos =
    let
      fun h (VAL v) = f v
        | h (CHOICE (p,d1,d2)) = choice (p, h d1, h d2)
        | h (UNION (d1,d2)) = unionWith (h d1) (h d2) g pos
        | h (TWICE d) = twiceWith (h d) g pos
        | h BOTTOM = BOTTOM
	| h (STAR (p,d1,d2)) =
	      starWith p (h d1) (h d2) g (!maxiterations) pos
    in
      h d
    end


  (* apply arithmetic operator to distribution expression
     of singleton collections *)

  fun arith d1 d2 f pos =
        unionWith d1 d2
                 (fn (Interpreter.VAL [m],Interpreter.VAL [n]) =>
		       VAL (Interpreter.VAL [f (m,n)])
                      | _ => raise DistribError ("Singleton expected",pos))
                 pos

  (* apply linear function f to distribution expression *)
  (* using f(a U b) = f(a) U f(b) *)
  (* f is of type value -> dist *)

  fun linear2 f (VAL v) = f v
    | linear2 f (CHOICE (p, d1, d2)) =
        choice (p, linear2 f d1, linear2 f d2)
    | linear2 f (UNION (d1,d2)) =
        union (linear2 f d1, linear2 f d2)
    | linear2 f (TWICE d) = twice (linear2 f d)
    | linear2 f BOTTOM = BOTTOM
    | linear2 f (STAR (p, d1, d2)) =
        star (p, linear2 f d1, linear2 f d2)

  (* as above for f : value -> value *)

  fun linear f d = linear2 (VAL o f) d

  (* probability of empty value in distribution expression *)

  fun pEmpty (VAL (Interpreter.VAL [])) = 1.0
    | pEmpty (VAL _) = 0.0
    | pEmpty (CHOICE (p,d1,d2)) =
        p*(pEmpty d1)+(1.0-p)*(pEmpty d2)
    | pEmpty (UNION (d1,d2)) =
        let
          val p1 = pEmpty d1
        in
          if p1 = 0.0 then 0.0
          else p1*(pEmpty d2)
        end
    | pEmpty (TWICE d1) =
        let
          val p1 = pEmpty d1
        in
          p1*p1
        end
    | pEmpty BOTTOM = 0.0
    | pEmpty (STAR (p,d1,d2)) =
        let
	  val q1 = pEmpty d1
	  val q2 = pEmpty d2
	in (* solve x = p*q1+(1-p)*q2*x *)
	  p*q1/(1.0+(p-1.0)*q2)
	end

  (* probability of nonempty value in distribution expression *)

  fun pNonempty (VAL (Interpreter.VAL [])) = 0.0
    | pNonempty (VAL _) = 1.0
    | pNonempty (CHOICE (p,d1,d2)) =
        p*(pNonempty d1)+(1.0-p)*(pNonempty d2)
    | pNonempty (UNION (d1,d2)) =
        let
          val p1 = pNonempty d1
        in
          if p1 = 1.0 then 1.0
          else
            let
              val p2 = pNonempty d2
            in
             p1+p2-p1*p2
	   end
        end
    | pNonempty (TWICE d1) =
        let
          val p1 = pNonempty d1
        in
          p1+p1-p1*p1
        end
    | pNonempty BOTTOM = 0.0
    | pNonempty (STAR (p,d1,d2)) =
        let
	  val q1 = pNonempty d1
	  val q2 = pNonempty d2
	  val q3 = (1.0-p)*q2
	in (* solve x = p*q1+(1-p)*(q2+x-q2*x) *)
	  (p*q1+q3)/(p+q3)
	end

  (* normalize distribution expression to right-associated and sorted
     CHOICE of VALs ending in either a VAL or BOTTOM *)

  fun normalize d pos =
    homomorphic VAL
		(fn (Interpreter.VAL v,Interpreter.VAL w) =>
		       VAL (Interpreter.VAL (mergeI v w))
		  | (_,_) => raise DistribError
				   ("Cannot apply union to texts",pos))
		d pos

  (* convert normalized distribution expression
     to list of (value,probability) pairs *)

  fun toList p (VAL v) = [(v,p)]
    | toList p (CHOICE (p1, VAL v, d)) =
        (v,p*p1) :: toList (p*(1.0-p1)) d
    | toList p BOTTOM = []
    | toList _ _ = raise DistribError ("Internal error toList",(0,0))

  (* memo table for function calls *)
  val memo = ref []

  fun lookupM (fenv,n) [] = NONE
    | lookupM (fenv,0) (m::ms) = lookup fenv m
    | lookupM (fenv,n) (m::ms) = lookupM (fenv,n-1) ms

  fun addM ((fenv,0),v) [] = [[(fenv,v)]]
    | addM ((fenv,0),v) (m::ms) = ((fenv,v)::m)::ms
    | addM ((fenv,n),v) [] = [] :: addM ((fenv,n-1),v) []
    | addM ((fenv,n),v) (m::ms) = m :: addM ((fenv,n-1),v) ms


  (* tests if an expression will always return a singleton *)

  fun alwaysSingleton e =
    case e of
      Syntax.NUM _ => true
    | Syntax.CHOOSE _ => true
    | Syntax.PLUS _ => true
    | Syntax.MINUS _ => true
    | Syntax.TIMES _ => true
    | Syntax.DIVIDE _ => true
    | Syntax.UMINUS _ => true
    | Syntax.D _ => true
    | Syntax.Z _ => true
    | Syntax.SUM _ => true
    | Syntax.COUNT _ => true
    | Syntax.LARGEST (Syntax.NUM (1,_),_,_) => true
    | Syntax.LEAST (Syntax.NUM (1,_),_,_) => true
    | Syntax.MEDIAN _ => true
    | _ => false

  (* return list of (value,probability) pairs for Troll expression *)

  fun distribDice (decs,e) = 
    (maxcalls := !maxiterations;
     toList 1.0 (normalize (dExp0 e [] decs) (0,0)))

  (* return distribution expression for Troll expression *)

  and dExp0 exp table decs =
  let
    fun dExp exp table =
    case exp of
      Syntax.NUM (n,p) => VAL (Interpreter.VAL [n])
    | Syntax.ID (x,p) =>
        (case lookup x table of
           SOME v => VAL v
         | NONE => raise DistribError ("unknown variable: "^x,p))
    | Syntax.EMPTY => VAL (Interpreter.VAL [])
    | Syntax.CONC (e1,e2,p) =>
        let
          fun simpleList (Syntax.NUM (n,_)) = SOME [n]
	    | simpleList (Syntax.CONC (e1,e2,_)) =
	        (case (simpleList e1, simpleList e2) of
		   (SOME l1, SOME l2) => SOME (l1@l2)
		 | _ => NONE)
            | simpleList _ = NONE
        in
	  case (simpleList e1, simpleList e2) of
	    (SOME l1, SOME l2) =>
	       VAL (Interpreter.VAL (mergeI (mergeSort l1) (mergeSort l2)))
          | _ => union (dExp e1 table, dExp e2 table)
        end
    | Syntax.CHOOSE (e1,p) =>
        let
          fun countEq x n [] = (n,[])
            | countEq x n (xs as (x1::xs1)) =
                if x=x1 then countEq x (n+1) xs1
                else (n,xs)
	  fun uniform [] _ =
                raise DistribError ("Can't choose from empty collection",p)
            | uniform (x::xs) n =
                let
                  val (m,xs2) = countEq x 1 xs
                  val n1 = n-m
                in
		  if n1=0 then VAL (Interpreter.VAL [x])
                  else
                    choice(Real.fromInt m / Real.fromInt n,
                           VAL (Interpreter.VAL [x]),
                           uniform xs2 n1)
                end
          fun choose (VAL (Interpreter.VAL v)) = uniform v (length v)
            | choose (CHOICE (p,d1,d2)) =
                choice (p, choose d1, choose d2)
            | choose BOTTOM = BOTTOM
            | choose _ = 
                 raise DistribError ("choose cannot be applied to text",p)
        in
          choose (normalize (dExp e1 table) p)
        end
    | Syntax.PICK (e1,e2,p) =>
        let
          val d1 = normalize (dExp e1 table) p
          fun pick m n xs =
	        if m=0 then VAL (Interpreter.VAL [])
	        else if n<=m then VAL (Interpreter.VAL xs)
		else case xs of
                       [] => VAL (Interpreter.VAL [])
                             (* should not occur as 0<m<n *)
		     | (x1::xs1) =>
                         CHOICE (Real.fromInt m / Real.fromInt n,
				 union (VAL (Interpreter.VAL [x1]),
					pick (m-1) (n-1) xs1),
				 pick m (n-1) xs1)
          fun pick1 0 d1 = VAL (Interpreter.VAL [])
            | pick1 m (VAL (Interpreter.VAL xs)) = pick m (List.length xs) xs
	    | pick1 m (CHOICE (p,d11,d12)) =
		 choice (p, pick1 m d11, pick1 m d12)
            | pick1 m _ =
                 raise DistribError ("pick can not be applied to text",p)
          fun pick0 (VAL (Interpreter.VAL [m])) = pick1 m d1
            | pick0 (CHOICE (p,d21,d22)) =
                CHOICE (p, pick0 d21, pick0 d22)
            | pick0 BOTTOM = BOTTOM
            | pick0 _ =
                raise DistribError ("Non-singleton 2nd arg to pick",p)
        in
          pick0 (normalize (dExp e2 table) p)
        end
    | Syntax.PLUS (e1,e2,p) =>
        arith (dExp e1 table) (dExp e2 table) (op +) p
    | Syntax.MINUS (e1,e2,p) =>
        arith (dExp e1 table) (dExp e2 table) (op -) p
    | Syntax.UMINUS (e1,p) =>
        arith (VAL (Interpreter.VAL [0])) (dExp e1 table) (op -) p
    | Syntax.TIMES (e1,e2,p) =>
        arith (dExp e1 table) (dExp e2 table) (op * ) p
    | Syntax.DIVIDE (e1,e2,p) =>
        arith (dExp e1 table) (dExp e2 table) (op div) p
    | Syntax.D (e1,p) =>
        let
          fun d 1 m = VAL (Interpreter.VAL [m])
            | d n m =
                if n>0 then
                  CHOICE (1.0/Real.fromInt n,
			  VAL (Interpreter.VAL [m]),
			  d (n-1) (m+1))
                else raise DistribError ("arg to d or D must be >0",p)
          fun ds (VAL (Interpreter.VAL [n])) = d n 1
            | ds (CHOICE (p,d1,d2)) =
                choice (p, ds d1, ds d2)
            | ds BOTTOM = BOTTOM
            | ds _ = raise DistribError ("Non-singleton used with D or d",p)
        in
          ds (dExp e1 table)
        end
    | Syntax.Z (e1,p) =>
        let
          fun d 0 m = VAL (Interpreter.VAL [m])
            | d n m =
                if n>0 then
                  CHOICE (1.0/Real.fromInt (n+1),
			  VAL (Interpreter.VAL [m]),
			  d (n-1) (m+1))
                else raise DistribError ("arg to z or Z must be >=0",p)
          fun ds (VAL (Interpreter.VAL [n])) = d n 0
            | ds (CHOICE (p,d1,d2)) =
                choice (p, ds d1, ds d2)
            | ds BOTTOM = BOTTOM
            | ds _ = raise DistribError ("Non-singleton used with z or Z",p)
        in
          ds (dExp e1 table)
        end
    | Syntax.SUM (e1,p) =>
        homomorphic
            (fn (Interpreter.VAL v) =>
		VAL (Interpreter.VAL [List.foldr (op +) 0 v])
	      | _ => raise DistribError ("Internal error SUM",p))
	    (fn (Interpreter.VAL [m],Interpreter.VAL [n]) =>
		VAL (Interpreter.VAL [m+n])
              | _ => raise DistribError ("Internal error SUM",p))
	    (dExp e1 table)
	    p
    | Syntax.COUNT (e1,p) =>
        homomorphic
            (fn (Interpreter.VAL v) => VAL (Interpreter.VAL [List.length v])
	      | _ => raise DistribError ("Internal error SUM",p))
            (fn (Interpreter.VAL [m],Interpreter.VAL [n]) =>
		VAL (Interpreter.VAL [m+n])
              | _ => raise DistribError ("Internal error COUNT",p))
	    (dExp e1 table)
	    p
    | Syntax.DIFFERENT (e1,p) =>
        let
        (* merge two strictly ascending lists of integers,
           discarding duplicates *)
	  fun mergeS [] l2 = l2
	    | mergeS l1 [] = l1
	    | mergeS (l1 as (a::l11)) (l2 as (b::l22)) =
                (case Int.compare (a,b) of
		   LESS    => a :: mergeS l11 l2
		 | EQUAL   => mergeS l11 l2
		 | GREATER => b :: mergeS l1 l22)
          fun noDups [] = []
            | noDups (x::xs) = mergeS [x] (noDups xs)
        in
          homomorphic
            (fn (Interpreter.VAL v) => VAL (Interpreter.VAL (noDups v))
	      | _ => raise DistribError ("Can't apply different to text",p))
            (fn (Interpreter.VAL m, Interpreter.VAL n) =>
		VAL (Interpreter.VAL (mergeS m n))
	      | _ => raise DistribError ("Can't apply different to text",p))
	    (dExp e1 table)
	    p
        end
    | Syntax.LEAST (e1,e2,p) =>
        let
          val d2 = dExp e2 table
          fun least n (v as Interpreter.VAL w) =
                if List.length w <= n then VAL v
                else VAL (Interpreter.VAL (List.take (w, n)))
	    | least n _ = raise DistribError ("can't apply least to text",p)
          fun ls (VAL (Interpreter.VAL [n])) =
                if n<0 then
                  raise DistribError ("1st arg to least must be >=0",p)
                else homomorphic
		       (least n)
		       (fn (Interpreter.VAL v,Interpreter.VAL w) =>
			   least n (Interpreter.VAL (mergeI v w))
			 | _ => raise DistribError ("can't apply least to text",p))
		       d2
		       p
            | ls (CHOICE (p,d1,d2)) =
                choice (p, ls d1, ls d2)
            | ls BOTTOM = BOTTOM
            | ls _ = raise DistribError ("Non-singleton used with least",p)
        in
          ls (dExp e1 table)
        end
    | Syntax.LARGEST (e1,e2,p) =>
        let
          val d2 = dExp e2 table
          fun largest n (v as Interpreter.VAL w) =
                let val ll = List.length w in
                  if ll <= n then VAL v
                  else VAL (Interpreter.VAL (List.drop (w, ll - n)))
                end
	    | largest n _ = raise DistribError ("can't apply least to text",p)
          fun ls (VAL (Interpreter.VAL [n])) =
                if n<0 then
                  raise DistribError ("1st arg to largest must be >=0",p)
                else homomorphic
		       (largest n)
		       (fn (Interpreter.VAL v,Interpreter.VAL w) =>
			   largest n (Interpreter.VAL (mergeI v w))
			 | _ => raise DistribError ("can't apply largest to text",p))
		       d2
		       p
            | ls (CHOICE (p,d1,d2)) =
                choice (p, ls d1, ls d2)
            | ls BOTTOM = BOTTOM
            | ls _ = raise DistribError ("Non-singleton used with largest",p)
        in
          ls (dExp e1 table)
        end
    | Syntax.MEDIAN (e1,p) =>
        let
	  fun median xs = List.nth (xs, List.length xs div 2)
	  fun medianD BOTTOM = BOTTOM
            | medianD (VAL (Interpreter.VAL []))
	         = raise DistribError ("Can't take median of empty collection",p)
	    | medianD (VAL  (Interpreter.VAL vs)) = VAL (Interpreter.VAL [median vs])
	    | medianD (CHOICE (p,d1,d2)) = CHOICE (p, medianD d1, medianD d2)
	    | medianD _ = raise DistribError ("Can't take median of text",p)
        in
          case e1 of
            Syntax.HASH (Syntax.NUM (n,p1), e2, p3)
	       => if alwaysSingleton e2 then
                    dExp (Syntax.LEAST (Syntax.NUM (1,p1),
	                                Syntax.LARGEST (Syntax.NUM ((n+1) div 2,p1),
				                        e1, p1), p1))
                         table
                  else medianD (normalize (dExp e1 table) p)
	  | _ => medianD (normalize (dExp e1 table) p)
        end
    | Syntax.MINIMAL (e1,p) =>
        let
          fun minimal [] = []
	    | minimal [x] = [x]
	    | minimal (x1::(xs as x2::_)) =
	        if x1=x2 then x1::minimal xs
		else [x1]
	in
          homomorphic
            (VAL o Interpreter.VAL
	         o (fn (Interpreter.VAL v) => minimal v
		     | _ => raise DistribError ("can't apply minimal to text",p)))
            (fn (Interpreter.VAL [],ys) => VAL ys
	      | (xs,Interpreter.VAL []) => VAL xs
	      | (xs as Interpreter.VAL (xs1 as (x::_)),
		 ys as Interpreter.VAL (ys1 as (y::_))) =>
		  if x<y then VAL xs
		  else if x>y then VAL ys
		  else VAL (Interpreter.VAL (xs1@ys1))
	      | _ => raise DistribError ("can't apply minimal to text",p))
	    (dExp e1 table)
	    p
	end
    | Syntax.MAXIMAL (e1,p) =>
        let
          fun maximal [] = []
	    | maximal (x::xs) =
	        (case maximal xs of
	           [] => [x]
		 | (ys as (y::_)) => if x=y then x::ys else ys)
	in
          homomorphic
            (VAL o Interpreter.VAL
	         o (fn (Interpreter.VAL v) => maximal v
		     | _ => raise DistribError ("can't apply maximal to text",p)))
            (fn (Interpreter.VAL [],ys) => VAL ys
	      | (xs,Interpreter.VAL []) => VAL xs
	      | (xs as Interpreter.VAL (xs1 as (x::_)),
		 ys as Interpreter.VAL (ys1 as (y::_))) =>
		  if x<y then VAL ys
		  else if x>y then VAL xs
		  else VAL (Interpreter.VAL (xs1@ys1))
	      | _ => raise DistribError ("can't apply maximal to text",p))
	    (dExp e1 table)
	    p
	end
    | Syntax.HASH (e1,e2,p) =>
        let
          val d2 = dExp e2 table
          fun h 0 d = VAL (Interpreter.VAL [])
            | h 1 d = d
            | h n d = if n mod 2 = 0 then
                        twice (h (n div 2) d)
                      else union (h (n-1) d, d)
          fun hs (VAL (Interpreter.VAL [n])) =
	        if n<0 then
		  raise DistribError ("first arg to # must be >=0",p)
                else h n d2
            | hs (CHOICE (p,d1,d2)) =
                choice (p, hs d1, hs d2)
            | hs BOTTOM = BOTTOM
            | hs _ = raise DistribError ("Non-singleton used with #",p)
        in
          hs (dExp e1 table)
        end
    | Syntax.AND (e1,e2,p) =>
        let
          val p1 = pEmpty (dExp e1 table)
        in
          if p1 = 0.0 then dExp e2 table
          else if p1 = 1.0 then VAL (Interpreter.VAL [])
          else choice (p1, VAL (Interpreter.VAL []), dExp e2 table)
        end
    | Syntax.LT (e1,e2,p) =>
        let
          val d2 = dExp e2 table
          fun lt n = linear (fn (Interpreter.VAL w) =>
				  Interpreter.VAL (List.filter (fn a => n<a) w)
			      | _ => raise DistribError ("can't use < on text",p))
			    d2
          fun filter (VAL (Interpreter.VAL [n])) = lt n
            | filter (CHOICE (p,d1,d2)) =
                choice (p, filter d1, filter d2)
            | filter BOTTOM = BOTTOM
            | filter _ = raise DistribError ("Non-singleton used with <",p)
        in
          filter (dExp e1 table)
        end
    | Syntax.LE (e1,e2,p) =>
        let
          val d2 = dExp e2 table
          fun le n = linear (fn (Interpreter.VAL w) =>
				  Interpreter.VAL (List.filter (fn a => n<=a) w)
			      | _ => raise DistribError ("can't use <= on text",p))
			    d2
          fun filter (VAL (Interpreter.VAL [n])) = le n
            | filter (CHOICE (p,d1,d2)) =
                choice (p, filter d1, filter d2)
            | filter BOTTOM = BOTTOM
            | filter _ = raise DistribError ("Non-singleton used with <=",p)
        in
          filter (dExp e1 table)
        end
    | Syntax.GT (e1,e2,p) =>
        let
          val d2 = dExp e2 table
          fun gt n = linear (fn (Interpreter.VAL w) =>
				  Interpreter.VAL (List.filter (fn a => n>a) w)
			      | _ => raise DistribError ("can't use > on text",p))
			    d2
          fun filter (VAL (Interpreter.VAL [n])) = gt n
            | filter (CHOICE (p,d1,d2)) =
                choice (p, filter d1, filter d2)
            | filter BOTTOM = BOTTOM
            | filter _ = raise DistribError ("Non-singleton used with >",p)
        in
          filter (dExp e1 table)
        end
    | Syntax.GE (e1,e2,p) =>
        let
          val d2 = dExp e2 table
          fun ge n = linear (fn (Interpreter.VAL w) =>
				  Interpreter.VAL (List.filter (fn a => n>=a) w)
			      | _ => raise DistribError ("can't use >= on text",p))
			    d2
          fun filter (VAL (Interpreter.VAL [n])) = ge n
            | filter (CHOICE (p,d1,d2)) =
                choice (p, filter d1, filter d2)
            | filter BOTTOM = BOTTOM
            | filter _ = raise DistribError ("Non-singleton used with >=",p)
        in
          filter (dExp e1 table)
        end
    | Syntax.EQ (e1,e2,p) =>
        let
          val d2 = dExp e2 table
          fun eq n = linear (fn (Interpreter.VAL w) =>
				  Interpreter.VAL (List.filter (fn a => n=a) w)
			      | _ => raise DistribError ("can't use = on text",p))
			    d2
          fun filter (VAL (Interpreter.VAL [n])) = eq n
            | filter (CHOICE (p,d1,d2)) =
                choice (p, filter d1, filter d2)
            | filter BOTTOM = BOTTOM
            | filter _ = raise DistribError ("Non-singleton used with =",p)
        in
          filter (dExp e1 table)
        end
    | Syntax.NEQ (e1,e2,p) =>
        let
          val d2 = dExp e2 table
          fun neq n = linear (fn (Interpreter.VAL w) =>
				  Interpreter.VAL (List.filter (fn a => n<>a) w)
			      | _ => raise DistribError ("can't use =/= on text",p))
			    d2
          fun filter (VAL (Interpreter.VAL [n])) = neq n
            | filter (CHOICE (p,d1,d2)) =
                choice (p, filter d1, filter d2)
            | filter BOTTOM = BOTTOM
            | filter _ = raise DistribError ("Non-singleton used with =/=",p)
        in
          filter (dExp e1 table)
        end
    | Syntax.DROP (e1,e2,p) =>
        let
          val d1 = dExp e1 table
          val d2 = dExp e2 table
          fun subtract [] v = []
            | subtract (x::xs) v =
                if member x v then subtract xs v
                else x :: subtract xs v
          fun drop (VAL (Interpreter.VAL v)) (VAL (Interpreter.VAL w)) =
	        VAL (Interpreter.VAL (subtract v w))
	    | drop (VAL v) (VAL w) =
	        raise DistribError ("can't apply drop to text",p)
            | drop (CHOICE (p,d1,d2)) d3 =
                choice (p, drop d1 d3, drop d2 d3)
            | drop (UNION (d1,d2)) d3 =
                union (drop d1 d3, drop d2 d3)
            | drop (TWICE d1) d2 =
                twice (drop d1 d2)
            | drop BOTTOM _ = BOTTOM
	    | drop (STAR (p,d1,d2)) d3 =
	        star (p,drop d1 d3, drop d2 d3)
            | drop d1 (CHOICE (p,d2,d3)) =
                choice (p, drop d1 d2, drop d1 d3)
            | drop d1 (UNION (d2,d3)) =
                drop (drop d1 d2) d3
            | drop d1 (TWICE d2) =
                drop (drop d1 d2) d2
            | drop _ BOTTOM = BOTTOM
	    | drop d3 (STAR (p,d1,d2)) =
	        dropStar d3 p d1 d2 (!maxiterations)
	  and dropStar d3 p d1 d2 0 = BOTTOM
	    | dropStar d3 p d1 d2 i =
	        choice (p, drop d3 d1,
			   dropStar (drop d3 d2) p d1 d2 (i-1))
        in
          drop d1 d2
        end
    | Syntax.KEEP (e1,e2,p) =>
        let
          val d1 = dExp e1 table
          val d2 = dExp e2 table
          fun intersect [] v = []
            | intersect (x::xs) v =
                if member x v then x :: intersect xs v
                else intersect xs v
	  fun intersect1 (Interpreter.VAL v) w =
	        Interpreter.VAL (intersect v w)
	    | intersect1 _ _ = raise DistribError
					 ("can't apply keep to text",p)
	  fun keep d1 (VAL (Interpreter.VAL w)) =
	        linear (fn v => intersect1 v w) d1
	    | keep d1 (CHOICE  (p,d2,d3)) =
                choice (p, keep d1 d2, keep d1 d3)
            | keep _ BOTTOM = BOTTOM
	    | keep _ _ = raise DistribError
				   ("can't apply keep to text",p)
        in
          keep d1 (normalize d2 p)
        end
    | Syntax.SETMINUS (e1,e2,p) =>
        let
          val d1 = dExp e1 table
          val d2 = dExp e2 table
	  fun drop1 a [] = []
	    | drop1 a (b::bs) = if a=b then bs else b :: drop1 a bs

	  fun setminusV [] l2 = []
	    | setminusV (a::l1) l2 =
              if member a l2 then setminusV l1 (drop1 a l2)
	      else a :: setminusV l1 l2
	  fun setminus1 (VAL (Interpreter.VAL v)) w =
	        VAL (Interpreter.VAL (setminusV v w))
	    | setminus1 (CHOICE (p,d11,d12)) w =
	        choice (p, setminus1 d11 w, setminus1 d12 w)
	    | setminus1 BOTTOM w = BOTTOM
	    | setminus1 _ w = raise DistribError
				   ("can't apply -- to text",p)
	  fun setminus d1 (VAL (Interpreter.VAL w)) = setminus1 d1 w
	    | setminus d1 (VAL _) = raise DistribError
					  ("can't apply -- to text",p)
	    | setminus d1 (CHOICE (p,d21,d22)) =
	        choice (p, setminus d1 d21, setminus d1 d22)
	    | setminus d1 (UNION (d21,d22)) =
	        setminus (setminus d1 d21) d22
	    | setminus d1 (TWICE d2) =
	        setminus (setminus d1 d2) d2
	    | setminus d1 BOTTOM = BOTTOM
	    | setminus d1 d2 = setminus d1 (normalize d2 p)
        in
          setminus (normalize d1 p) d2
        end
    | Syntax.FROMTO (e1,e2,p) =>
        unionWith
          (dExp e1 table) (dExp e2 table)
          (fn (Interpreter.VAL [m],Interpreter.VAL [n]) =>
	      VAL (Interpreter.VAL (List.tabulate (n-m+1,fn x=>x+m)))
            | _ => raise DistribError ("Non-singleton used with ..",p))
          p
    | Syntax.LET (x,e1,e2,p) =>
        let
          fun bind (VAL v) =
                dExp e2 ((x,v)::table)
            | bind (CHOICE (p,d1,d2)) =
                choice (p, bind d1, bind d2)
            | bind BOTTOM = BOTTOM
            | bind _ = raise DistribError ("Internal error LET ",p)
        in
          case occurrences x e2 of
            0 => dExp e2 table
          | 1 => dExp (substitute x e1 e2) table
          | _ => bind (normalize (dExp e1 table) p)
        end
    | Syntax.REPEAT (x,e1,e2,continue,p) =>
        let
          (* returns probability of repeating 
             and distribution expression when not repeating *)
          fun repeat (VAL v) x e2 =
	        let
		  val p = if continue
                          then pNonempty (dExp e2 ((x,v)::table))
                          else pEmpty (dExp e2 ((x,v)::table))
                in
                  if p = 1.0 then (1.0, BOTTOM)
                  else (p, VAL v)
		end
            | repeat (CHOICE (p,d1,d2)) x e2 =
                let
                  val (q,d1s) = repeat d1 x e2
                  val (r,d2s) = repeat d2 x e2
                  val p1 = p*q+(1.0-p)*r (* prob of repeating *)
                in
                  if p1 = 1.0 then (1.0, BOTTOM)
                  else (p1, choice(p*(1.0-q)/(1.0-p1),d1s,d2s))
                end
	    | repeat BOTTOM x e2 = (0.0, BOTTOM)
            | repeat _ x e2 = raise DistribError ("Internal error REPEAT ",p)
        in
          #2 (repeat (normalize (dExp e1 table) p) x e2)
        end
    | Syntax.ACCUM (x,e1,e2,continue,p) =>
        let
          (* returns probability of repeating
             and distribution expression for repeat/stop *)
          fun repeatStop (VAL v) x e2 =
                let
                  val p = if continue
                          then pNonempty (dExp e2 ((x,v)::table))
                          else pEmpty (dExp e2 ((x,v)::table))
                in
                  (p, VAL v, VAL v) (* pRepeat, dRepeat, dStop *)
                end
            | repeatStop (CHOICE (p,d1,d2)) x e2 =
                let
                  val (q,d1r,d1s) = repeatStop d1 x e2
                  val (r,d2r,d2s) = repeatStop d2 x e2
                  val p1 = p*q+(1.0-p)*r (* prob of repeating *)
                in
                  (p1, if p1 = 0.0 then BOTTOM
		       else choice(p*q/p1,d1r,d2r),
                       if p1 = 1.0 then BOTTOM
		       else choice( p*(1.0-q)/(1.0-p1),d1s,d2s))
                end
	    | repeatStop BOTTOM x e2 = (0.0, BOTTOM, BOTTOM)
            | repeatStop _ x e2 = raise DistribError ("Internal error ACCUM ",p)
          val (pr,dr,ds) = repeatStop (normalize (dExp e1 table) p) x e2
        in
	  star (1.0-pr, ds, dr)
        end
    | Syntax.FOREACH (x,e1,e2,p) =>
        let
          fun foreach (Interpreter.VAL v) =
                foldr (fn (n,y) =>
			  union (dExp e2 ((x,Interpreter.VAL [n])::table),y))
                      (VAL (Interpreter.VAL []))
		v
	    | foreach _ = raise DistribError ("can't apply foreach to text",p)
        in
          linear2 foreach (dExp e1 table)
        end
    | Syntax.IF (e1,e2,e3,p) =>
        let
	  val d1 = dExp e1 table
          val pe = pEmpty d1
          val pn = pNonempty d1
        in
          if pe = 1.0 then dExp e3 table
          else if pn = 1.0 then dExp e2 table
          else if pe+pn = 1.0 then choice (pn, dExp e2 table, dExp e3 table)
          else  choice (pn, dExp e2 table,
			    choice (pe/(1.0-pn),dExp e3 table, BOTTOM))
        end
    | Syntax.CALL (f,args,p) =>
        callFun f (List.map (fn e => dExp e table) args) decs p
    | Syntax.QUESTION (prob,p) =>
        choice (prob, VAL (Interpreter.VAL [1]), VAL (Interpreter.VAL []))
                 
    | Syntax.STRING (s,p) => VAL (Interpreter.TEXT [s])
    | Syntax.SAMPLE(e1,p) =>
        let
          val d = dExp e1 table
	in
	  linear Interpreter.makeText (normalize d p)
	end
    | Syntax.SAMPLES(e1,e2,p) =>
        let
          val d1 = normalize (dExp e1 table) p
          val d2 = dExp e2 table
	  val d2' = linear Interpreter.makeText (normalize d2 p)
	  fun samples 0 = VAL (Interpreter.TEXT [])
	    | samples 1 = d2'
	    | samples n = unionWith d2'
				    (samples (n-1))
				    (VAL o Interpreter.vconcr)
				    p
	  fun samples0 (VAL (Interpreter.VAL [m])) =
	        if m<0 then raise DistribError ("Negative arg1 to '",p)
		else samples m
            | samples0 (CHOICE (p,d21,d22)) =
                CHOICE (p, samples0 d21, samples0 d22)
            | samples0 BOTTOM = BOTTOM
            | samples0 _ =
                raise DistribError ("Non-singleton 1st arg to '",p)
	in
	  samples0 d1
	end
    | Syntax.HCONC (e1,e2,p) =>
        unionWith (normalize (dExp e1 table) p)
	          (normalize (dExp e2 table) p)
		  (VAL o Interpreter.hconc)
	          p
    | Syntax.VCONCL (e1,e2,p) =>
        unionWith (normalize (dExp e1 table) p)
	          (normalize (dExp e2 table) p)
		  (VAL o Interpreter.vconcl)
	          p
    | Syntax.VCONCR (e1,e2,p) =>
        unionWith (normalize (dExp e1 table) p)
	          (normalize (dExp e2 table) p)
		  (VAL o Interpreter.vconcr)
	          p
    | Syntax.VCONCC (e1,e2,p) =>
        unionWith (normalize (dExp e1 table) p)
	          (normalize (dExp e2 table) p)
		  (VAL o Interpreter.vconcc)
	          p
  in
    dExp exp table
  end

  and callFun f vs decs p =
    case lookup f decs of
      NONE => raise DistribError ("Unknown function: "^f,p)
    | SOME (Syntax.Func (pars, body, pos)) =>
        let
          fun call [] [] env body =
              let
	        val info = ((f,env),!maxcalls)
	      in
	        case lookupM info (!memo) of
	          SOME v => v
		| NONE =>
		    let
		      val v = dExp0 body env decs
		    in
		      memo := addM (info,v) (!memo);
		      v
		    end
	      end
            | call (x::xs) (v::vs) env body =
              let
                fun call1 (VAL v) =
                    call xs vs ((x,v)::env) body
                  | call1 (CHOICE (p,d1,d2)) =
                    choice (p, call1 d1, call1 d2)
                  | call1 BOTTOM = BOTTOM
                  | call1 _ =
                    raise DistribError ("Internal error CALL ",p)
              in
                call1 (normalize v pos)
              end
            | call _ _ _ _ =
              raise DistribError ("Wrong number of args to "^f,p)
        in
          if !maxcalls = 0
          then BOTTOM
          else
            (maxcalls := !maxcalls - 1;
             call pars vs [] body
             before
             maxcalls := !maxcalls + 1)
        end
     | SOME (Syntax.Comp (empty, single, union, pos)) =>
         let
           fun f0 [] = dExp0 empty [] decs
             | f0 (n::ns) = callFun union
                                   [callFun single [VAL (Interpreter.VAL [n])] decs pos, f0 ns]
				   decs pos
           fun g (x,y) = callFun union [VAL x, VAL y] decs pos
         in
           case vs of
	     [v] => homomorphic (fn (Interpreter.VAL xs) => f0 xs
				  | _ => raise DistribError ("Can't apply compositional fuction to text",p))
				g v pos
           | _   => raise DistribError ("Wrong number of args to "^f,p)
         end

  and occurrences x exp =
    case exp of
      Syntax.ID (y,p) => if x=y then 1 else 0
    | Syntax.CONC (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.CHOOSE (e1,p) => occurrences x e1
    | Syntax.PICK (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.PLUS (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.MINUS (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.UMINUS (e1,p) => occurrences x e1
    | Syntax.TIMES (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.DIVIDE (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.D (e1,p) => occurrences x e1
    | Syntax.Z (e1,p) => occurrences x e1
    | Syntax.SUM (e1,p) => occurrences x e1
    | Syntax.COUNT (e1,p) => occurrences x e1
    | Syntax.DIFFERENT (e1,p) => occurrences x e1
    | Syntax.MEDIAN (e1,p) => occurrences x e1
    | Syntax.LEAST (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.LARGEST (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.MINIMAL (e1,p) => occurrences x e1
    | Syntax.MAXIMAL (e1,p) => occurrences x e1
    | Syntax.HASH (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.AND (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.LT (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.LE (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.GT (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.GE (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.EQ (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.NEQ (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.DROP (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.SETMINUS (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.KEEP (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.FROMTO (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.LET (y,e1,e2,p) =>
        if y=x then occurrences x e1
        else occurrences x e1 + occurrences x e2
    | Syntax.REPEAT (y,e1,e2,continue,p) =>
        if y=x then 2*occurrences x e1
        else 2*(occurrences x e1 + occurrences x e2)
    | Syntax.ACCUM (y,e1,e2,continue,p) =>
        if y=x then 2*occurrences x e1
        else 2*(occurrences x e1 + occurrences x e2)
    | Syntax.FOREACH (y,e1,e2,p) =>
        if y=x then occurrences x e1
        else occurrences x e1 + 2*occurrences x e2
    | Syntax.IF (e1,e2,e3,p) =>
        occurrences x e1 + Int.max (occurrences x e2, occurrences x e3)
    | Syntax.CALL (f,args,p) =>
        List.foldr (op +) 0 (List.map (occurrences x) args)
    | Syntax.SAMPLE (e1,p) => occurrences x e1
    | Syntax.SAMPLES (e1,e2,p) => occurrences x e1 + 2*(occurrences x e2)
    | Syntax.HCONC (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.VCONCL (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.VCONCR (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.VCONCC (e1,e2,p) => occurrences x e1 + occurrences x e2
    | _ => 0


  and substitute x exp1 exp2 =
    case exp2 of
      Syntax.ID (y,p) => if x=y then exp1 else exp2
    | Syntax.CONC (e1,e2,p) =>
        Syntax.CONC (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.CHOOSE (e1,p) => Syntax.CHOOSE (substitute x exp1 e1,p)
    | Syntax.PICK (e1,e2,p) =>
        Syntax.PICK (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.PLUS (e1,e2,p) =>
        Syntax.PLUS (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.MINUS (e1,e2,p) =>
        Syntax.MINUS (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.UMINUS (e1,p) => Syntax.UMINUS (substitute x exp1 e1,p)
    | Syntax.TIMES (e1,e2,p) =>
        Syntax.TIMES (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.DIVIDE (e1,e2,p) =>
        Syntax.DIVIDE (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.D (e1,p) => Syntax.D (substitute x exp1 e1,p)
    | Syntax.Z (e1,p) => Syntax.Z (substitute x exp1 e1,p)
    | Syntax.SUM (e1,p) => Syntax.SUM (substitute x exp1 e1,p)
    | Syntax.COUNT (e1,p) => Syntax.COUNT (substitute x exp1 e1,p)
    | Syntax.DIFFERENT (e1,p) => Syntax.DIFFERENT (substitute x exp1 e1,p)
    | Syntax.MEDIAN (e1,p) => Syntax.MEDIAN (substitute x exp1 e1,p)
    | Syntax.LEAST (e1,e2,p) =>
        Syntax.LEAST (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.LARGEST (e1,e2,p) =>
        Syntax.LARGEST (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.MINIMAL (e1,p) => Syntax.MINIMAL (substitute x exp1 e1,p)
    | Syntax.MAXIMAL (e1,p) => Syntax.MAXIMAL (substitute x exp1 e1,p)
    | Syntax.HASH (e1,e2,p) =>
        Syntax.HASH (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.AND (e1,e2,p) =>
        Syntax.AND (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.LT (e1,e2,p) =>
        Syntax.LT (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.LE (e1,e2,p) =>
        Syntax.LE (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.GT (e1,e2,p) =>
        Syntax.GT (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.GE (e1,e2,p) =>
        Syntax.GE (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.EQ (e1,e2,p) =>
        Syntax.EQ (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.NEQ (e1,e2,p) =>
        Syntax.NEQ (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.DROP (e1,e2,p) =>
        Syntax.DROP (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.SETMINUS (e1,e2,p) =>
        Syntax.SETMINUS (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.KEEP (e1,e2,p) =>
        Syntax.KEEP (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.FROMTO (e1,e2,p) =>
        Syntax.FROMTO (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.LET (y,e1,e2,p) =>
        if y=x then
          Syntax.LET (y, substitute x exp1 e1, e2, p)
        else
          Syntax.LET (y, substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.REPEAT (y,e1,e2,continue,p) =>
        if y=x then
          Syntax.REPEAT (y, substitute x exp1 e1, e2,continue, p)
        else
          Syntax.REPEAT (y, substitute x exp1 e1, substitute x exp1 e2,
			 continue, p)
    | Syntax.ACCUM (y,e1,e2,continue,p) =>
        if y=x then
          Syntax.ACCUM (y, substitute x exp1 e1, e2,
			continue, p)
        else
          Syntax.ACCUM (y, substitute x exp1 e1, substitute x exp1 e2,
			continue, p)
    | Syntax.FOREACH (y,e1,e2,p) =>
        if y=x then
          Syntax.FOREACH (y, substitute x exp1 e1, e2, p)
        else
          Syntax.FOREACH (y, substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.IF (e1,e2,e3,p) =>
        Syntax.IF (substitute x exp1 e1,
		   substitute x exp1 e2,
		   substitute x exp1 e3, p)
    | Syntax.CALL (f,args,p) =>
        Syntax.CALL (f, List.map (substitute x exp1) args, p)
    | Syntax.SAMPLE (e1,p) => Syntax.SAMPLE (substitute x exp1 e1,p)
    | Syntax.SAMPLES (e1,e2,p) =>
        Syntax.SAMPLES (substitute x exp1 e1,substitute x exp1 e2,p)
    | Syntax.HCONC (e1,e2,p) =>
        Syntax.HCONC (substitute x exp1 e1,substitute x exp1 e2,p)
    | Syntax.VCONCL (e1,e2,p) =>
        Syntax.VCONCL (substitute x exp1 e1,substitute x exp1 e2,p)
    | Syntax.VCONCR (e1,e2,p) =>
        Syntax.VCONCR (substitute x exp1 e1,substitute x exp1 e2,p)
    | Syntax.VCONCC (e1,e2,p) =>
        Syntax.VCONCC (substitute x exp1 e1,substitute x exp1 e2,p)
    | _ => exp2

end

