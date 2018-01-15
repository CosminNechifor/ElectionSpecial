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

val b = transform ("1992\n");


fun recursiv (nil, acc) = acc
| recursiv(H::T, acc) = recursiv(T, ( List.nth((splitter H), 0), List.nth((splitter H), 1), transform (List.nth((splitter H),2)))::acc);


(* nthElement of a list *)
val nthElement = (List.nth (listFile, 1));
val splitedElement = splitter nthElement;

val a = recursiv(listFile, []);
