module Assembler where

import Data.Either
import Data.Bits
import Data.Char
import Control.Monad
import Control.Applicative
import Numeric

newtype MachineCode = MachineCode {mc :: String}
newtype BFCode = BFCode {bf :: String}
newtype Error = Error {getError :: String}

type Stack = [Int]

assembleToMif :: BFCode -> Either Error [String]
assembleToMif asm = do
    codeLines <- assemble asm
    return $ formatMifWidth 16 codeLines

formatMifWidth :: Int -> [MachineCode] -> [String]
formatMifWidth width codes =
    ["Width=" ++ show width ++ ";"] ++
    ["Depth=" ++ (show $ (length codes + 1)) ++ ";\n"] ++
    ["ADDRESS_RADIX=DEC;"] ++
    ["DATA_RADIX=HEX;\n"] ++
    ["CONTENT BEGIN"] ++
        zipWith (formatCodeLine . mc) codes [0..] ++
        [defaultLine] ++
    ["END;"]
    where formatCodeLine code pc = "\t" ++ (show pc) ++ ": " ++ code ++ ";"
          defaultLine
            | width == 16 = "\t" ++ (show $ length codes)  ++ ": " ++ "ffff;"
            | width == 32 = "\t" ++ (show $ length codes)  ++ ": " ++ "ffffffff;"

assemble :: BFCode -> Either Error [MachineCode]
assemble asm = filter ((/= "").mc) <$> concat <$> zipWithM (assembleLine linepcs) [0..] bfs
    where bfs = filter ((/="").bf) $ map BFCode $ lines $ bf asm
          linepcs = getLinePCs bfs

assembleLine :: [Int] -> Int -> BFCode -> Either Error [MachineCode]
assembleLine startpcs line code = 
    (reverse <$> ((MachineCode "a000":) <$> assembleLine' startpcs ((),[]) ((startpcs !! line) + (length (bf code)) -1) line ((BFCode . reverse . bf) code)))
    >>= (getEnds ((),[]) (startpcs !! line))


getEnds :: ((), Stack) -> Int -> [MachineCode] -> Either Error [MachineCode]
getEnds ((), stack) pc (x:xs)
    | (head.mc) x == '5' = (x:) <$> getEnds (pushJump pc stack) (pc+1) xs
    | (head.mc) x == '6' = case hex startJump of
                            Right start -> (MachineCode ("6" ++ start):) <$> getEnds ((),nextstack) (pc+1) xs
                            Left err -> Left err
    | otherwise          = (x:) <$> getEnds ((),stack) (pc+1) xs
    where (startJump, nextstack) = getJump stack 
getEnds _ _ [] = Right []


assembleLine' :: [Int] -> ((), Stack) -> Int -> Int -> BFCode -> Either Error [MachineCode]
assembleLine' startpcs ((), stack) pc line (BFCode (x:xs)) = case x of
    '+' -> (MachineCode "1000":) <$> rest
    '-' -> (MachineCode "2000":) <$> rest
    '>' -> (MachineCode "3000":) <$> rest
    '<' -> (MachineCode "4000":) <$> rest
    '[' -> case hex endJump of 
            Right end -> (MachineCode ("5" ++ end):) <$> restnext
            Left err -> Left err
    ']' -> (MachineCode "6000":) <$> (assembleLine' startpcs (pushJump pc stack) (pc-1) line $ BFCode xs)
    '*' -> case hex line of
            Right start -> (MachineCode ("7" ++ start):) <$> rest
            Left err -> Left err
    '^' -> case hex (startpcs !! (line - 1)) of
            Right start -> (MachineCode ("7" ++ start):) <$> rest
            Left err -> Left err
    'v' -> if length startpcs == (line + 2) then Left (Error "v at last line")
                else case hex (startpcs !! (line + 1)) of
                    Right start -> (MachineCode ("7" ++ start):) <$> rest
                    Left err -> Left err 
    '|' -> (MachineCode "8000":) <$> rest
    ' ' -> assembleLine' startpcs ((), stack) pc line $ BFCode xs
    '.' -> (MachineCode "9000":) <$> rest
    where rest = assembleLine' startpcs ((), stack) (pc-1) line $ BFCode xs
          (endJump, nextstack) = getJump stack 
          restnext = assembleLine' startpcs ((), nextstack) (pc-1) line $ BFCode xs
assembleLine' _ _ _ _ (BFCode []) = Right []

toHex [] = []
toHex bits = (hexDigit $ take 4 bits) : (toHex $ drop 4 bits)
hexDigit = (intToDigit . readImmBase 2)
hex str = toHex <$> getNLowestBits 12 False str


getNLowestBits :: Int -> Bool -> Int -> Either Error String
getNLowestBits n isSigned val
    | tooManyBits = Left $ Error errMsg
    | otherwise = Right $ map (boolToBit . testBit val) [n - 1, n - 2 .. 0]
    where boolToBit bool = if bool then '1' else '0'
          tooManyBits 
            | isSigned = not (val `elem` [(negate (2^(n-1)))..(2^(n-1))])
            | otherwise = not (val `elem` [0..2^n])
          errMsg = "More than " ++ (show n) ++ " bits encode: " ++ (show val)

getLinePCs :: [BFCode] -> [Int]
getLinePCs bfs = scanl (\acc x -> acc + (length. bf) x) 0 bfs

pushJump :: Int -> Stack -> ((),Stack)
pushJump a xs = ((), a:xs)

getJump :: Stack -> (Int,Stack)
getJump (x:xs) = (x,xs)

readImmBase :: Num a => a -> String -> a
readImmBase base imm = foldl (\l r -> base*l + r) 0 nums
    where nums = map (fromIntegral . digitToInt) imm

