import System.Environment
import System.Exit

main = getArgs >>= parse >>= putStr

parse [int, string, "--check"]  = checkAlpha    >> exit
parse [int, string, "--unique"] = uniqueAlpha    >> exit
parse [int, string, "--clean"]  = cleanAlpha    >> exit
parse [int, "--check"]  = check    >> exit
parse [int, "--unique"] = unique    >> exit
parse [int, "--clean"]  = clean    >> exit
parse ["-h"]    = usage     >> exit
parse []        = usage     >> exitError
parse otherwise = usage     >> exitError


checkAlpha  = putStrLn "checkAlpha"
check       = putStrLn "check"

uniqueAlpha = putStrLn "uniqueAlpha"
unique      = putStrLn "unique"

cleanAlpha  = putStrLn "cleanAlpha"
clean       = putStrLn "clean"

usage       = putStrLn "USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n\t--check\t\tcheck if a sequence is a de Bruijn sequence\n\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences\n\t--clean\t\tlist cleaning\n\tn\t\torder of the sequence\n\ta\t\talphabet [def: “01”]"

exitError   = exitWith (ExitFailure 84)
exit        = exitWith ExitSuccess