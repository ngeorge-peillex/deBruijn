import System.Environment
import System.Exit
import Numeric.Natural
import Data.List
import Data.Char

main = do
    getArgs >>= parse

-- PARSING ARGS ------------------------------------------

parse [n, alphabet, "--unique"] = do
    nbr <- checkNumber n
    if allUnique alphabet
        then unique nbr alphabet
        else usage >> exitError

parse [n, alphabet, "--check"] = do
    nbr <- checkNumber n
    if allUnique alphabet
        then check nbr alphabet
        else usage >> exitError

parse [n, alphabet, "--clean"] = do
    nbr <- checkNumber n
    if allUnique alphabet
        then clean nbr alphabet
        else usage >> exitError

parse [n, "--unique"] = do
    nbr <- checkNumber n
    unique nbr "01"

parse [n, "--check"] = do
    nbr <- checkNumber n
    check nbr "01"

parse [n, "--clean"] = do
    nbr <- checkNumber n
    clean nbr "01"

parse [n, alphabet] = do
    nbr <- checkNumber n
    if allUnique alphabet
        then generation nbr alphabet
        else usage >> exitError
    
parse ["-h"] = do
        usage

parse [n] = do
    nbr <- checkNumber n
    generation nbr "01"

parse otherwise = do
    usage
    exitError

-- TOOLS ---------------------------------------------------

allUnique ::(Eq a) => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

rotate :: String -> String
rotate xs = take (length xs) (drop 1 (cycle xs))

checkNumber :: String -> IO (Int)
checkNumber n = do
    if checkIsNumber n ==  True && n /= "0" then return (read n::Int) else usage >> exitError

checkIsNumber :: String -> Bool
checkIsNumber n = case (reads n) :: [(Natural, String)] of
    [(_, "")] -> True
    _         -> False

splitString :: Int -> String -> [String] -> [String]
splitString nbr input xs
    | xs == [] = splitString nbr tmp ((take nbr input) : xs)
    | length input < nbr = xs
    | otherwise = splitString nbr (drop 1 input) ((take nbr input) : xs)
    where
        tmp = input ++ (take (nbr - 1) input)

printOKorKO :: Bool -> IO ()
printOKorKO result =
    if result == True then putStrLn "OK" else putStrLn "KO"


usage :: IO ()
usage   = putStrLn "USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n\t--check\t\tcheck if a sequence is a de Bruijn sequence\n\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences\n\t--clean\t\tlist cleaning\n\tn\t\torder of the sequence\n\ta\t\talphabet [def: “01”]"

exitError   = exitWith (ExitFailure 84)
exit        = exitWith ExitSuccess

-- CHECK ----------------------------------------------------

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

check :: Int -> String -> IO ()
check nbr alphabet = do
    input <- getLine
    printOKorKO $ isDebruijn (length alphabet) nbr (splitString nbr input [])

-- UNIQUE -------------------------------------------------

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

-- CLEAN --------------------------------------------------

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

-- GENERATION ---------------------------------------------

addAlpha :: Int -> String -> String -> String
addAlpha nbr alphabet xs
    | isInfixOf ((drop ((length xs) - (nbr - 1)) xs) ++ [last alphabet]) xs = [head alphabet]
    | otherwise = [last alphabet]  

preferOne :: Int -> String -> String -> String
preferOne nbr alphabet xs
    | length xs < nbr = preferOne nbr alphabet (xs ++ [head alphabet])
    | length xs < fullLength = preferOne nbr alphabet (xs ++ (addAlpha nbr alphabet xs))
    | otherwise = xs
    where
        fullLength = length alphabet ^ nbr

generation :: Int -> String -> IO ()
generation nbr alphabet
    | length alphabet /= 2 = print (alphabet)
    | otherwise = putStrLn $ preferOne nbr alphabet []