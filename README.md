Quasiquote 'str' implements multiline strings with interpolation.
Interpolating a value into the string is done by 
$<String expression>$
and interpolating anything with instance Show is 
$:<Show expression>$. Due to pretty deep limitations, the parser
is not able to properly deduce associtivity of infix operators,
so use lots and lots of parenthesis.

Repetitive patterns can be made with # symbol: 

    #<pattern> in <list>: <interpolated string> (|<interpolated string>)#
 
Where (|<interpolated string>) denotes optional separator for the 
elements.

Multiline indentation is handled by aligning on smallest un-empty
line after the first. Neither pattern matching nor nested #-patterns
are supported, however, see example below.

Normal '\n' style escaping of special characters
is intentionally not supported. Please use $endline$ or $"\n"$ 
style instead. 

As an example, let's plot set of vectors with gnuplot:
 
     plotVecs :: [(String,[Double])] -> String
     plotVecs vs =  
         [$str| ## Plot multiple vectors
                plot #(n,_) in vs:'-' with lines lw 5 title $:n$ |, #
                #d in map snd vs:$singleVec d$$endline$e$endline$# |]
     where
      singleVec n = [$str|#(e,i) in zip n [1..]: $:i$ $:e$|$endline$#|]
 
    *Gnuplotter> plotVecs [("A",[1..5]),("B",[2..6])]
    # Plot multiple vectors
     plot '-' with lines lw 5 title "A" , '-' with lines lw 5 title "B" 
      1 1.0
      2 2.0
      3 3.0
      4 4.0
      5 5.0
     
     e
      1 2.0
      2 3.0
      3 4.0
      4 5.0
      5 6.0
     
     e

 Change log
 0.2.5.2 - Possibly now compiles with GHC 6.12

