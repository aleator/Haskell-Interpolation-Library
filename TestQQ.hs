{-# LANGUAGE QuasiQuotes,DeriveDataTypeable #-}
module Interpolation where
import Language.Haskell.TH as TH
import Control.Monad.Trans
import Language.Haskell.TH.Quote
import Data.Data
import Data.Generics.Aliases
import Data.Char
import Data.List.Split

quoteExprExp :: String -> TH.ExpQ
 
str  :: QuasiQuoter
str  =  QuasiQuoter quoteExprExp undefined
--
quoteExprExp s =  do  loc <- TH.location
                      let (fn,a,b) =  (TH.loc_filename loc,
                                 (TH.loc_start loc),
                                 (TH.loc_end loc))
                      x <- runIO $ readFile fn
                      --stringE $ getStrPart a b x
                      psToStringE (parParse $ norming $ getStrPart a b x)
--

psToStringE :: PieceString -> TH.Q TH.Exp
psToStringE [] = TH.stringE ""
psToStringE (x:xs) = TH.appE (TH.appE (TH.varE (TH.mkName "++"))
                                (sbitToExp x))
                        (psToStringE xs)

sbitToExp (Var x)  =  (TH.varE (TH.mkName x))
sbitToExp (SVar x) = TH.appE (TH.varE (TH.mkName "show"))
                        (TH.varE (TH.mkName x))
sbitToExp (Str x) =  TH.stringE x 

type PieceString = [StringBits]
data StringBits = Str String | Var String | SVar String
                deriving (Eq,Ord,Show,Typeable,Data)

-- Split to string into pieces. This needs to be done properly.
toStringBits str = map toB $ split (startsWithOneOf "$") str
    where 
        toB ('$':':':s) = SVar s
        toB ('$':x:s) | isAsciiLower x = Var (x:s)
                      | otherwise = Str (x:s) 
        toB s = Str s

parParse [] = []
parParse ('$':'$':s) = Str "$":parParse s
parParse ('$':':':s) = SVar (takeWhile (/='$') s)
                        :parParse (drop 1 (dropWhile (/='$') s))
parParse ('$':s) = Var (takeWhile (/='$') s)
                    :parParse (drop 1 (dropWhile (/='$') s))
parParse s = Str (takeWhile (/='$') s): parParse (dropWhile (/='$') s) 

-- Normalize the indentation to match second line
norming = unlines . norming' . lines
norming' (l:lst) = l:map (drop (n)) lst
    where 
     n = minimumD 0 $ map (length . takeWhile (==' ') ) 
         $ (filter (not.isEmpty) lst)
     isEmpty = all (isSpace)

minimumD d [] = d
minimumD d x  = minimum x

-- Pulling the correct part of the the file to be processed
get :: Int -> Int -> [a] -> [a]
get a b = take (b-a+1) . drop (a-1)

getStrPart :: (Int,Int) -> (Int,Int) -> String -> String
getStrPart s@(sl,sc) e@(el,ec) str = unlines (reverse $ (\(x:xs) -> take (ld-2) x:xs) $ reverse $ (\(x:xs) -> drop sc x:xs) $ get sl el . lines $ str)
    where ld = if sl==el then (ec-sc) else ec


