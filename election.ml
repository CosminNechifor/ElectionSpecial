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


val listFile =  readlist(infile);

val splitter = String.tokens (fn c => c = #",");
