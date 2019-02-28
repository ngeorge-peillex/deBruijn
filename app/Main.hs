import System.Environment
import System.Exit
import Numeric.Natural
import Data.List
import Data.Char

main = getArgs >>= parse >>= putStr

sortString :: String -> String
sortString alphabet = map toLower (alphabet ++ "e")

parse [n, alphabet, "--unique"] = putStrLn (sortString alphabet) >> checkNumber n >> uniqueAlpha  >> exit
parse [n, alphabet, "--check"]  = checkNumber n >> checkAlpha    >> exit
parse [n, alphabet, "--clean"]  = checkNumber n >> cleanAlpha    >> exit
parse [n, "--unique"] = checkNumber n >> unique  >> exit
parse [n, "--check"]  = checkNumber n >> check   >> exit
parse [n, "--clean"]  = checkNumber n >> clean   >> exit
parse ["-h"]    = usage >> exit
parse otherwise = usage >> exitError


checkNumber n = do
    if checkIsNumber n ==  True && n /= "0" then return n else usage >> exitError

checkIsNumber :: String -> Bool
checkIsNumber n = case (reads n) :: [(Natural, String)] of
    [(_, "")] -> True
    _         -> False

checkAlpha  = putStrLn "checkAlpha"
check       = putStrLn "check"

uniqueAlpha = putStrLn "uniqueAlpha"
unique      = putStrLn "unique"

cleanAlpha  = putStrLn "cleanAlpha"
clean       = putStrLn "clean"

usage       = putStrLn "USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n\t--check\t\tcheck if a sequence is a de Bruijn sequence\n\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences\n\t--clean\t\tlist cleaning\n\tn\t\torder of the sequence\n\ta\t\talphabet [def: “01”]"

exitError   = exitWith (ExitFailure 84)
exit        = exitWith ExitSuccess