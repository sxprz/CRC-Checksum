module CRC(toChar, toString, toPoly, fromChar, fromString, fromPoly, xor,
            xorPoly, deg, convert, appendPoly, performPLD, computeChecksum,
            checkChecksum, transmittedBitString, Polynomial, PolyLongDiv) where
{-
    The polynomial is stored as a little-endian binary number,
    where the reference point is our index.
    Essentially, the first Bool value is the highest bit.
-}
newtype Polynomial = Poly [Bool]

data PolyLongDiv = PLD Polynomial Polynomial Polynomial Polynomial Steps
data Step = S Polynomial Polynomial Polynomial
type Steps = [Step]

instance Show Polynomial where
    show (Poly bs) = show $ toString bs

instance Show Step where
    show (S p p' p'') = show p ++ "\n" ++ show p' ++ "\tXOR\n" ++ replicate (length (show p') + 8) '-' ++ "\n" ++ show p'' ++ "\n\n"

instance Show PolyLongDiv where
    show (PLD a b c d ss) = "Polynomial long division: \n" ++ show a ++ " : " ++ show b ++ " = " ++ show c ++ " with remainder " ++ show d ++ "\n" ++
                            "Steps as follows: \n" ++ concatMap show ss

instance Eq Polynomial where
    (==) (Poly bs) (Poly bs') = bs == bs'
    (/=) p p' = not $ p == p'
    
instance Ord Polynomial where
    (>) (Poly bs) (Poly bs') = convert bs > convert bs'
    (<) (Poly bs) (Poly bs') = convert bs < convert bs'
    (<=) p p' = not (p > p')
    (>=) p p' = not (p < p')

toChar :: Bool -> Char
toChar True  = '1'
toChar _     = '0'

toString :: [Bool] -> String
toString [] = ""
toString bs = [toChar b | b <- bs]

toPoly :: String -> Polynomial
toPoly s = Poly (fromString s)

fromChar :: Char -> Bool
fromChar '1' = True
fromChar _   = False

fromString :: String -> [Bool]
fromString ""  = []
fromString str = [fromChar c | c <- str]

fromPoly :: Polynomial -> String
fromPoly (Poly bs) = toString bs

-- Basic xor function
xor :: Bool -> Bool -> Bool
xor False False = False
xor False True  = True
xor True False  = True
xor True True   = False

-- Returns the degree of a given polynom
deg :: Polynomial -> Int
deg (Poly bs) = length bs - 1

-- Performs an xor-operation on two binary polynomials
xorPoly :: Polynomial -> Polynomial -> Polynomial
xorPoly (Poly bs) (Poly bs') = Poly $ xorPoly' bs bs'

xorPoly' :: [Bool] -> [Bool] -> [Bool]
xorPoly' bs [] = bs
xorPoly' [] bs = bs
xorPoly' (b:bs) (b':bs') = xor b b' : xorPoly' bs bs'

-- Counts till '1' occurs in the bitstring
countTillOne :: [Bool] -> Int
countTillOne [] = 0
countTillOne (True:bs) = 0
countTillOne (False:bs) = 1+countTillOne bs

-- Converts a bitstring into an integer
convert :: [Bool] -> Int
convert bs = sum [if bs !! i then 2^i else 0 | i <- [0..length bs-1]]
-- add this: let revBs = reverse bs -> then fix performPLD bugfix

-- Compares two bit lists and returns True if bs > bs' as Int else False
cmp :: [Bool] -> [Bool] -> Bool
cmp bs bs' = convert bs > convert bs'

-- Appends the second bitstring to the first one
appendPoly :: Polynomial -> Polynomial -> Polynomial
appendPoly (Poly bs) (Poly bs') = Poly (bs++bs')

-- This performs an algorithm called polynomial long division
performPLD :: Polynomial -> Polynomial -> PolyLongDiv
performPLD m@(Poly []) r = PLD m r m m []
performPLD m@(Poly bs) r@(Poly bs') = PLD m r (Poly res) (Poly rem) ss
    where (res, rem, ss) = pld (drop (countTillOne bs) bs) bs'

-- return type : (result, remainder, steps)
pld :: [Bool] -> [Bool] -> ([Bool], [Bool], Steps)
pld bs bs'
    | length bs-1 == length bs'-1 && not (cmp newBs bs') = ([CRC.cmp (take (length bs') bs) bs'], drop (length xor'd-(length bs'-1)) xor'd, [S (Poly bs) (Poly bs') (Poly xor'd)]) 
    | otherwise = let (res,rem,ss) = pld newBs bs' in (CRC.cmp (take (length bs') bs) bs' : res, rem, S (Poly bs) (Poly bs') (Poly xor'd):ss)
    where xor'd = xorPoly' (take (length bs') bs) bs'
          newBs = drop (countTillOne xor'd) xor'd ++ drop (length bs') bs

-- Computes checksum of first polynomial by executing PLD with second polynomial on the first one
computeChecksum :: Polynomial -> Polynomial -> Polynomial
computeChecksum (Poly bs) p = rem
    where (PLD _ _ _ rem _) = performPLD (Poly (bs ++ replicate (deg p) False)) p

-- Checks if bit-flips occured in the sent message (as bitstring) 
checkChecksum :: Polynomial -> Polynomial -> Bool
checkChecksum p p' = computeChecksum (transmittedBitString p p') p' == toPoly (replicate (deg p) '0')

-- Returns the complete bitstring after PLD
transmittedBitString :: Polynomial -> Polynomial -> Polynomial
transmittedBitString p p' = appendPoly p (computeChecksum p p')
