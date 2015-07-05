import System.Environment
import Text.Read

-- Solution
superdigit :: Integer -> Integer
superdigit x
    | x < 0     = - superdigit (-x)
    | x < 10    = x
    | otherwise = superdigit $ sumDigits x

sumDigits :: Integer -> Integer
sumDigits = sum . map readDigit . show

readDigit :: Char -> Integer
readDigit x = read [x]

-- Input and error handling
parseSingleArgument :: String -> Either Integer String
parseSingleArgument input = case readMaybe input of
    Just number -> Left number
    _           -> Right $ input ++ " is not a number"

parseArgs :: [String] -> IO (Either Integer String)
parseArgs [] = return $ Right "Need one number as argument"
parseArgs [x] = return $ parseSingleArgument x
parseArgs _ = return $ Right "Too much arguments"

-- Plumbing
calcAndShowSuperdigit :: Integer -> IO ()
calcAndShowSuperdigit = putStrLn . show . superdigit

main :: IO ()
main = do
    input <- getArgs >>= parseArgs
    case input of
            Left number -> calcAndShowSuperdigit number
            Right msg -> putStrLn msg

