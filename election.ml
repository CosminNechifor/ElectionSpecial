exception MyError

val infile = "E:\\Facultate\\PF\\ElectionSpecial\\input.txt" ;

fun readlist (infile : string) = let
  val ins = TextIO.openIn infile
  fun loop ins =
   case TextIO.inputLine ins of
      SOME line => line :: loop ins
    | NONE      => []
in
  loop ins before TextIO.closeIn ins
end ;

(* destroy \n at the end of last chars*)
fun stripchars (string, chars) = let
  fun aux c =
    if String.isSubstring (str c) chars then
      ""
    else
      str c
in
  String.translate aux string
end;

fun inc x = x + 1;


(* Split elements of list *)
val splitter = String.tokens (fn c => c = #",");

(* Get list from file *)
val listFile =  readlist(infile);

(* Extract element nth of list *)
val element = (List.nth (listFile, 0));

val splitedList = splitter element;

fun transform X = stripchars (X, "\n" );
fun convert X = valOf(Int.fromString X);
(*val b = transform ("1992\n");*)
(* val b = convert("123");*)

fun getParty(_, p, _) = p;
fun getVotes(_, _, v) = v;
fun getName(n, _, _) = n;

(* type candidate = string * string * int; *)
fun insert (n:(string * string * int)) nil = [n]
| insert n (h::t) = if (getVotes h) < (getVotes n) then h:: insert n t else n::h::t;

fun flatten nil = nil
| flatten (h::t) = h @ flatten(t);

fun sort nil = nil
| sort (h::t) = insert h (sort t);

fun sum nil = 0
| sum(h::t) = h + sum t;

fun last nil = 0
| last (h::nil) = h
| last (h::t) = last(t);


fun isPartyP var a = if var = (getParty a) then true else false;

fun filter f nil = nil
|	filter f (h::t) = if f h then h::filter f t else filter f t;


fun recursiv (nil, acc) = acc
| recursiv(H::T, acc) = recursiv(T, ( List.nth((splitter H), 0), List.nth((splitter H), 1), convert (transform (List.nth((splitter H),2))))::acc);

fun isMember(name, party) = if (getParty(name) = party) then true else false;


(* nthElement of a list *)
(*val nthElement = (List.nth (listFile, 1));
val splitedElement = splitter nthElement;*)

val database = recursiv(listFile, []);

val numeCandidat = getName(List.nth(database, 0));
val partidCandidat = getParty(List.nth(database, 0));
val voturi = getVotes(List.nth(database, 0));

(* val isp = isPartyP ("Green" database); *)

val sorted = sort database;
val filtredList = filter (isPartyP "Independent") database;
