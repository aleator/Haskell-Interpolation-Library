{-# LANGUAGE QuasiQuotes#-}
import Data.String.Interpolation
import Data.List

main = do
    let x :: String
        (a,b) = (1,10)
        foo = [a..b]
        bar = zip foo (reverse foo)
        x = [$str|This is the starting line 
                  Second line. Notice that the indentation:
                     Interpolation can be done with $$a$$ where a is
                     a string or with $$:a$$ where a is instance of Show
                     $$$$ escapes to $$ and #### escapes to ##
                     Numbers from $:a$ to $:b$ are #i in foo:$:i$|,#
                        Sum of the numbers is $:sum foo$ or
                         $:(10*(a+b)) `div` 2$. Notice that fixity 
                         declarations don't work and you need 
                         parenthesis. $show '#'$ 
                     Pairs of numbers
                     => #(a,b) in reverse bar:$:a$+$:b$=$:a+b$|
                     #
                  Final line of text.|]
    putStr $ x
