type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err (p1,p2) = ErrorMsg.error p1
fun eof () = let val pos = hd (!linePos)
             in  Tokens.EOF (pos, pos)
             end

%%
%header (functor TigerLexFun (structure Tokens: Tiger_TOKENS));
%s COMMENT;
format    = [\ \t\n\011\012\013];
digit     = [0-9];
alpha     = [a-zA-Z];
alphanum  = {digit} | {alpha};
spaces    = (" "|\t)+;
printable = [^\000-\032\127\"\\];
escape    = "\\n" | "\\t" | "\\^"[@-_] | "\\"{digit}{3} |
             "\\\"" | "\\\\" | "\\"{format}+"\\";
char      = {printable} | " " | {escape};
string    = "\""{char}*"\"";

%%

{spaces} => (continue ());
\n       => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL>"/*"     => (YYBEGIN COMMENT; continue ());
<INITIAL>"&"      => (Tokens.AND (yypos, yypos+2));
<INITIAL>array    => (Tokens.ARRAY (yypos, yypos+5));
<INITIAL>":="     => (Tokens.ASSIGN (yypos, yypos+2));
<INITIAL>break    => (Tokens.BREAK (yypos, yypos+5));
<INITIAL>":"      => (Tokens.COLON (yypos, yypos+1));
<INITIAL>","	  => (Tokens.COMMA (yypos,yypos+1));
<INITIAL>"/"      => (Tokens.DIVIDE (yypos, yypos+1));
<INITIAL>do       => (Tokens.DO (yypos, yypos+2));
<INITIAL>"."      => (Tokens.DOT (yypos, yypos+1));
<INITIAL>else     => (Tokens.ELSE (yypos, yypos+4));
<INITIAL>end      => (Tokens.END (yypos, yypos+3));
<INITIAL>"="      => (Tokens.EQ (yypos, yypos+2));
<INITIAL>for      => (Tokens.FOR (yypos, yypos+3));
<INITIAL>function => (Tokens.FUNCTION (yypos, yypos+8));
<INITIAL>">="     => (Tokens.GE (yypos, yypos+2));
<INITIAL>">"      => (Tokens.GT (yypos, yypos+1));
<INITIAL>if       => (Tokens.IF (yypos, yypos+2));
<INITIAL>in       => (Tokens.IN (yypos, yypos+2));
<INITIAL>"{"      => (Tokens.LBRACE (yypos, yypos+1));
<INITIAL>"["      => (Tokens.LBRACK (yypos, yypos+1));
<INITIAL>"("      => (Tokens.LPAREN (yypos, yypos+1));
<INITIAL>"<="     => (Tokens.LE (yypos, yypos+2));
<INITIAL>let      => (Tokens.LET (yypos, yypos+3));
<INITIAL>"<"      => (Tokens.LT (yypos, yypos+1));
<INITIAL>"-"      => (Tokens.MINUS (yypos, yypos+1));
<INITIAL>"<>"     => (Tokens.NEQ (yypos, yypos+2));
<INITIAL>nil      => (Tokens.NIL (yypos, yypos+3));
<INITIAL>of       => (Tokens.OF (yypos, yypos+2));
<INITIAL>"|"      => (Tokens.OR (yypos, yypos+1));
<INITIAL>"+"      => (Tokens.PLUS (yypos, yypos+1));
<INITIAL>"}"      => (Tokens.RBRACE (yypos, yypos+1));
<INITIAL>"]"      => (Tokens.RBRACK (yypos, yypos+1));
<INITIAL>")"      => (Tokens.RPAREN (yypos, yypos+1));
<INITIAL>";"      => (Tokens.SEMICOLON (yypos, yypos+1));
<INITIAL>then     => (Tokens.THEN (yypos, yypos+4));
<INITIAL>"*"      => (Tokens.TIMES (yypos, yypos+1));
<INITIAL>to       => (Tokens.TO (yypos, yypos+2));
<INITIAL>type     => (Tokens.TYPE (yypos, yypos+4));
<INITIAL>var      => (Tokens.VAR (yypos, yypos+3));
<INITIAL>while    => (Tokens.WHILE (yypos, yypos+5));
<INITIAL>{digit}+ => (Tokens.INT 
                       (valOf (Int.fromString yytext), yypos, yypos+size yytext));
<INITIAL>{alpha}({alphanum}|"_")*
                  => (Tokens.ID (yytext, yypos, yypos+size yytext));
<INITIAL>{string} => (Tokens.STRING (yytext, yypos, yypos+size yytext));
<INITIAL>.        => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<COMMENT>"*/"     => (YYBEGIN INITIAL; continue ());
<COMMENT>.        => (continue ());
