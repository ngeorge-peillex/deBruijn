import System.Environment
import System.Exit
import Numeric.Natural
import Data.List
import Data.Char

main = do
    getArgs >>= parse

parse [n, alphabet, "--unique"] = do
    nbr <- checkNumber n
    let a = rotate alphabet
    unique nbr a

parse [n, alphabet, "--check"] = do
    nbr <- checkNumber n
    check nbr alphabet

parse [n, alphabet, "--clean"] = do
    nbr <- checkNumber n
    clean nbr alphabet

parse [n, "--unique"] = do
    nbr <- checkNumber n
    unique nbr "01"

parse [n, "--check"] = do
    nbr <- checkNumber n
    check nbr "01"

parse [n, "--clean"] = do
    nbr <- checkNumber n
    clean nbr "01"
parse ["-h"] = do
    usage

parse otherwise = do
    usage
    exitError

allUnique ::(Eq a) => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

rotate :: String -> String
rotate xs = take (length xs) (drop 1 (cycle xs))
                                                                                                        
expectLength :: String -> Natural -> Int                                                                    
expectLength alphabet n = (length alphabet) ^ n 

checkNumber :: String -> IO (Int)
checkNumber n = do
    if checkIsNumber n ==  True && n /= "0" then return (read n::Int) else usage >> exitError

checkIsNumber :: String -> Bool
checkIsNumber n = case (reads n) :: [(Natural, String)] of
    [(_, "")] -> True
    _         -> False

checkInput :: String -> String -> Bool
checkInput []  alphabet = True
checkInput (x:xs) alphabet
    | x `elem` alphabet && checkInput xs alphabet = True
    | otherwise = False

splitString :: Int -> String -> [String] -> [String]
splitString nbr input xs
    | xs == [] = splitString nbr tmp ((take nbr input) : xs)
    | length input < nbr = xs
    | otherwise = splitString nbr (drop 1 input) ((take nbr input) : xs)
    where
        tmp = input ++ (take (nbr - 1) input)

isDebruijn :: Int -> Int -> [String] -> Bool
isDebruijn sizeAlpha nbr array =
    if arraySize == exceptSize then True else False
    where
        exceptSize = sizeAlpha ^ nbr
        arraySize = length (nub array)

areDebruijn :: Int -> String -> String -> Bool
areDebruijn nbr alphabet xs =
    isDebruijn size nbr xs'
    where
        size = length alphabet
        xs' = (splitString nbr xs [])

printNewLine :: Show a => a -> IO ()
printNewLine x = putStrLn (show x)

printOKorKO :: Bool -> IO ()
printOKorKO result =
    if result == True then putStrLn "OK" else putStrLn "KO"

check :: Int -> String -> IO ()
check nbr alphabet = do
    input <- getLine
    printOKorKO $ isDebruijn (length alphabet) nbr (splitString nbr input [])

isUnique :: String -> String -> Int-> Bool
isUnique reference other index
    | reference == other = False
    | index == length reference = True
    | otherwise = isUnique reference rotateString (index + 1)
    where
        rotateString = rotate other

unique :: Int -> String -> IO ()
unique nbr alphabet = do
    reference <- getLine
    other <- getLine
    printOKorKO $ (areDebruijn nbr alphabet reference && areDebruijn nbr alphabet other && isUnique reference other 0)

getLineUntil :: [String] -> IO [String]
getLineUntil array = do
    line <- getLine
    case line of
        "END" -> return $ reverse array
        _ -> getLineUntil (line : array)

removeNonDebruin :: [String] -> IO ()
removeNonDebruin array = do
    putStr ( unlines (nubBy (\x y -> isUnique x y 0 == False) array) )

clean :: Int -> String -> IO ()
clean nbr alphabet = do
    lines <- getLineUntil []
    removeNonDebruin (filter (\x -> areDebruijn nbr alphabet x) lines)

usage :: IO ()
usage   = putStrLn "USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n\t--check\t\tcheck if a sequence is a de Bruijn sequence\n\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences\n\t--clean\t\tlist cleaning\n\tn\t\torder of the sequence\n\ta\t\talphabet [def: “01”]"

exitError   = exitWith (ExitFailure 84)
exit        = exitWith ExitSuccess