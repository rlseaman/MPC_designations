{-|
Module      : MPCDesignation
Description : Convert between packed and unpacked MPC designations
Copyright   : Public Domain (CC0)
License     : CC0-1.0
Maintainer  : rlseaman@arizona.edu

Convert between packed and unpacked Minor Planet Center (MPC) designations
for asteroids, comets, and natural satellites.

Based on MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html
-}

module MPCDesignation
    ( -- * Core conversion functions
      convertSimple
    , convert
    , pack
    , unpack
      -- * Format detection
    , detectFormat
    , FormatInfo(..)
    , DesignationType(..)
    , Format(..)
      -- * Error type
    , MPCError(..)
      -- * Low-level functions
    , packPermanent
    , unpackPermanent
    , packProvisional
    , unpackProvisional
    , packCometNumbered
    , unpackCometNumbered
    , packCometFull
    , unpackCometFull
    , packSatellite
    , unpackSatellite
      -- * Helper functions
    , toReportFormat
    , fromReportFormat
    , hasFragment
    , getFragment
    , getParent
    , designationsEqual
    ) where

import Data.Char (ord, chr, isDigit, isUpper, isLower, isAlpha, toUpper, toLower)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad (guard)

-- | Base-62 character set
base62Chars :: String
base62Chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

-- | Century codes for provisional designations
centuryCodes :: [(Char, Int)]
centuryCodes = [('A',10),('B',11),('C',12),('D',13),('E',14),('F',15),
                ('G',16),('H',17),('I',18),('J',19),('K',20),('L',21)]

-- | Valid comet type characters
cometTypes :: String
cometTypes = "PCDXAI"

-- | Valid planet codes for satellites
satellitePlanets :: String
satellitePlanets = "JSUN"

-- | Survey codes (packed -> unpacked)
surveyPackedToUnpacked :: [(String, String)]
surveyPackedToUnpacked = [("PLS","P-L"),("T1S","T-1"),("T2S","T-2"),("T3S","T-3")]

-- | Survey codes (unpacked -> packed)
surveyUnpackedToPacked :: [(String, String)]
surveyUnpackedToPacked = map (\(a,b) -> (b,a)) surveyPackedToUnpacked

-- | Maximum asteroid number (620000 + 62^4 - 1)
maxAsteroidNumber :: Int
maxAsteroidNumber = 15396335

-- | Error type for invalid designations
data MPCError = MPCError String
    deriving (Eq)

instance Show MPCError where
    show (MPCError msg) = "MPCDesignationError: " ++ msg

-- | Format of a designation
data Format = Packed | Unpacked
    deriving (Eq, Show)

-- | Type of designation
data DesignationType
    = Permanent
    | Provisional
    | ProvisionalExtended
    | Survey
    | CometNumbered
    | CometProvisional
    | CometFull
    | CometAncient
    | CometBCE
    | Satellite
    deriving (Eq, Show)

-- | Information about a detected format
data FormatInfo = FormatInfo
    { formatType   :: Format
    , designation  :: DesignationType
    , subtype      :: String
    } deriving (Eq, Show)

-- | Convert a base-62 character to its numeric value
base62ToNum :: Char -> Either MPCError Int
base62ToNum c = case elemIndex c base62Chars of
    Just n  -> Right n
    Nothing -> Left $ MPCError $ "Invalid base-62 character: " ++ [c]

-- | Convert a number (0-61) to its base-62 character
numToBase62 :: Int -> Either MPCError Char
numToBase62 n
    | n >= 0 && n < 62 = Right $ base62Chars !! n
    | otherwise = Left $ MPCError $ "Number out of base-62 range: " ++ show n

-- | Convert a base-62 string to a number
base62StringToNum :: String -> Either MPCError Int
base62StringToNum = foldl accumulate (Right 0)
  where
    accumulate (Left e) _ = Left e
    accumulate (Right acc) c = do
        v <- base62ToNum c
        return $ acc * 62 + v

-- | Convert a number to a base-62 string of specified width
numToBase62String :: Int -> Int -> Either MPCError String
numToBase62String width num = go width num []
  where
    go 0 _ acc = Right acc
    go w n acc = do
        c <- numToBase62 (n `mod` 62)
        go (w - 1) (n `div` 62) (c : acc)

-- | Get century code for a century number
getCenturyCode :: Int -> Either MPCError Char
getCenturyCode cent = case lookup cent (map (\(c,n) -> (n,c)) centuryCodes) of
    Just c  -> Right c
    Nothing -> Left $ MPCError $ "Invalid century: " ++ show cent

-- | Get century number from century code
getCenturyNum :: Char -> Either MPCError Int
getCenturyNum c = case lookup c centuryCodes of
    Just n  -> Right n
    Nothing -> Left $ MPCError $ "Invalid century code: " ++ [c]

-- | Decode cycle count from 2-char encoded string
decodeCycleCount :: String -> Either MPCError Int
decodeCycleCount [c1, c2]
    | isDigit c2 = do
        tens <- getTens c1
        return $ tens * 10 + (ord c2 - ord '0')
    | otherwise = Left $ MPCError $ "Invalid cycle count encoding: " ++ [c1, c2]
  where
    getTens c
        | isDigit c = Right $ ord c - ord '0'
        | isUpper c = Right $ ord c - 55  -- A=10, B=11, ..., Z=35
        | isLower c = Right $ ord c - 61  -- a=36, b=37, ..., z=61
        | otherwise = Left $ MPCError $ "Invalid cycle count first char: " ++ [c]
decodeCycleCount s = Left $ MPCError $ "Invalid cycle count encoding: " ++ s

-- | Encode cycle count to 2-char string
encodeCycleCount :: Int -> Either MPCError String
encodeCycleCount count
    | count < 0 || count >= 620 = Left $ MPCError $ "Cycle count out of range: " ++ show count
    | otherwise = Right [first, chr (ord '0' + ones)]
  where
    tens = count `div` 10
    ones = count `mod` 10
    first
        | tens < 10 = chr (ord '0' + tens)
        | tens < 36 = chr (tens + 55)  -- A-Z
        | otherwise = chr (tens + 61)  -- a-z

-- | Convert half-month letter to position (A=1, B=2, ..., skipping I)
letterToPosition :: Char -> Either MPCError Int
letterToPosition c
    | not (isAlpha c) = Left $ MPCError $ "Invalid half-month letter: " ++ [c]
    | otherwise = Right $ pos - (if c' > 'I' then 1 else 0)
  where
    c' = toUpper c
    pos = ord c' - ord 'A' + 1

-- | Convert position to half-month letter (1=A, 2=B, ..., skipping I)
positionToLetter :: Int -> Either MPCError Char
positionToLetter pos
    | pos < 1 || pos > 25 = Left $ MPCError $ "Invalid letter position: " ++ show pos
    | otherwise = Right $ chr (ord 'A' + adjusted - 1)
  where
    adjusted = if pos >= 9 then pos + 1 else pos

-- | Check if half-month letter is valid (A-Y excluding I)
isValidHalfMonth :: Char -> Bool
isValidHalfMonth c = isAlpha c && c' >= 'A' && c' <= 'Y' && c' /= 'I'
  where c' = toUpper c

-- | Unpack a permanent (numbered) asteroid designation
unpackPermanent :: String -> Either MPCError Int
unpackPermanent packed
    -- Tilde format (>= 620,000)
    | length packed == 5 && head packed == '~' = do
        base62Val <- base62StringToNum (tail packed)
        return $ 620000 + base62Val
    -- Simple numeric format (< 100,000)
    | length packed == 5 && all isDigit packed =
        case readMaybe packed of
            Just n  -> Right n
            Nothing -> Left $ MPCError $ "Invalid packed permanent: " ++ packed
    -- Letter prefix format (100,000 - 619,999)
    | length packed == 5 && isAlpha (head packed) && all isDigit (tail packed) = do
        let first = head packed
            rest = tail packed
        val <- if isUpper first
               then Right $ ord first - 55  -- A=10, B=11, etc.
               else Right $ ord first - 61  -- a=36, b=37, etc.
        case readMaybe rest of
            Just n  -> Right $ val * 10000 + n
            Nothing -> Left $ MPCError $ "Invalid packed permanent: " ++ packed
    | otherwise = Left $ MPCError $ "Invalid packed permanent designation: " ++ packed

-- | Pack a permanent (numbered) asteroid designation
packPermanent :: Int -> Either MPCError String
packPermanent number
    | number < 1 || number > maxAsteroidNumber =
        Left $ MPCError $ "Invalid asteroid number: " ++ show number
    | number < 100000 =
        Right $ replicate (5 - length s) '0' ++ s
      where s = show number
packPermanent number
    | number < 620000 = Right $ letter : rest
      where
        divN = number `div` 10000
        modN = number `mod` 10000
        letter = if divN < 36
                 then chr (divN + 55)  -- A-Z
                 else chr (divN + 61)  -- a-z
        rest = replicate (4 - length s) '0' ++ s
        s = show modN
packPermanent number = do
    encoded <- numToBase62String 4 (number - 620000)
    return $ '~' : encoded

-- | Unpack a provisional asteroid designation
unpackProvisional :: String -> Either MPCError String
unpackProvisional packed
    -- Survey designations
    | length packed == 7 && take 3 packed `elem` map fst surveyPackedToUnpacked =
        case lookup (take 3 packed) surveyPackedToUnpacked of
            Just survey -> Right $ show (read (drop 3 packed) :: Int) ++ " " ++ survey
            Nothing -> Left $ MPCError $ "Invalid survey: " ++ packed
    -- Standard provisional
    | length packed == 7 = do
        let centCode = packed !! 0
        -- Validate century code for asteroids: only I-L (1800-2199) are valid
        when (centCode `notElem` "IJKL") $
            Left $ MPCError $ "Invalid century code for asteroid: " ++ [centCode] ++ " (must be I-L)"
        centNum <- getCenturyNum centCode
        orderNum <- decodeCycleCount (take 2 $ drop 4 packed)
        let yearStr = show centNum ++ take 2 (drop 1 packed)
            yearInt = read yearStr :: Int
            halfMonth = packed !! 3
            secondLetter = packed !! 6
        -- Pre-1925 designations use A-prefix format
        if yearInt < 1925
            then let firstDigit = head yearStr
                     yearSuffix = tail yearStr
                     prefix = if firstDigit == '1' then 'A' else 'B'
                 in if orderNum == 0
                    then Right $ prefix : yearSuffix ++ " " ++ [halfMonth, secondLetter]
                    else Right $ prefix : yearSuffix ++ " " ++ [halfMonth, secondLetter] ++ show orderNum
            else if orderNum == 0
                 then Right $ yearStr ++ " " ++ [halfMonth, secondLetter]
                 else Right $ yearStr ++ " " ++ [halfMonth, secondLetter] ++ show orderNum
    | otherwise = Left $ MPCError $ "Invalid packed provisional: " ++ packed
  where
    when False _ = Right ()
    when True e = e

-- | Pack a provisional asteroid designation
packProvisional :: String -> Either MPCError String
packProvisional unpacked
    -- Survey designations: "2040 P-L" or "3138 T-1"
    | matchSurvey = case lookup survey surveyUnpackedToPacked of
        Just code -> Right $ code ++ replicate (4 - length numStr) '0' ++ numStr
        Nothing -> Left $ MPCError $ "Invalid survey: " ++ unpacked
    -- Old-style: "A908 CJ"
    | matchOldStyle = do
        centCode <- case oldCentDigit of
            '8' -> Right 'I'
            '9' -> Right 'J'
            '0' -> Right 'K'
            _   -> Left $ MPCError $ "Invalid old-style century: " ++ [oldCentDigit]
        return $ centCode : oldYearShort ++ [oldHalfMonth] ++ "00" ++ [oldSecondLetter]
    -- Standard provisional: "1995 XA" or "1995 XA12"
    | matchStandard = do
        let century = read (take 2 stdYear) :: Int
        centCode <- getCenturyCode century
        let orderNum = if null stdOrderStr then 0 else read stdOrderStr :: Int
        if orderNum >= 620
            then packExtendedProvisional (read stdYear) stdHalfMonth stdSecondLetter orderNum
            else do
                orderEnc <- encodeCycleCount orderNum
                return $ centCode : drop 2 stdYear ++ [stdHalfMonth] ++ orderEnc ++ [stdSecondLetter]
    | otherwise = Left $ MPCError $ "Invalid unpacked provisional: " ++ unpacked
  where
    trimmed = dropWhile (== ' ') $ reverse $ dropWhile (== ' ') $ reverse unpacked

    -- Survey pattern
    (matchSurvey, numStr, survey) = case words trimmed of
        [n, s] | s `elem` map snd surveyPackedToUnpacked -> (True, n, s)
        _ -> (False, "", "")

    -- Old-style pattern: "A908 CJ"
    matchOldStyle = length trimmed == 7 &&
                    head trimmed `elem` "AB" &&
                    all isDigit (take 3 $ drop 1 trimmed) &&
                    trimmed !! 4 == ' ' &&
                    isUpper (trimmed !! 5) &&
                    isUpper (trimmed !! 6)
    oldCentDigit = trimmed !! 1
    oldYearShort = take 2 $ drop 2 trimmed
    oldHalfMonth = trimmed !! 5
    oldSecondLetter = trimmed !! 6

    -- Standard pattern
    (matchStandard, stdYear, stdHalfMonth, stdSecondLetter, stdOrderStr) =
        case words trimmed of
            [y, rest] | length y == 4 && all isDigit y && length rest >= 2 &&
                        isUpper (head rest) && isUpper (rest !! 1) ->
                (True, y, head rest, rest !! 1, drop 2 rest)
            _ -> (False, "", ' ', ' ', "")

-- | Pack extended provisional (cycle >= 620)
packExtendedProvisional :: Int -> Char -> Char -> Int -> Either MPCError String
packExtendedProvisional year halfMonth secondLetter cycle = do
    yearChar <- numToBase62 (year `mod` 100)
    letterPos <- letterToPosition secondLetter
    let baseSeq = (cycle - 620) * 25 + letterPos - 1
    seqEncoded <- numToBase62String 4 baseSeq
    return $ '_' : yearChar : halfMonth : seqEncoded

-- | Unpack extended provisional
unpackExtendedProvisional :: String -> Either MPCError String
unpackExtendedProvisional packed
    | length packed /= 7 || head packed /= '_' =
        Left $ MPCError $ "Invalid extended packed provisional: " ++ packed
    | otherwise = do
        yearDigit <- base62ToNum (packed !! 1)
        baseSeq <- base62StringToNum (drop 3 packed)
        let halfMonth = packed !! 2
            cycle = 620 + baseSeq `div` 25
            letterPos = (baseSeq `mod` 25) + 1
        secondLetter <- positionToLetter letterPos
        let year = 2000 + yearDigit
        return $ show year ++ " " ++ [halfMonth, secondLetter] ++ show cycle

-- | Unpack comet provisional designation
unpackCometProvisional :: String -> Either MPCError String
unpackCometProvisional packed
    | length packed `notElem` [7, 8] =
        Left $ MPCError $ "Invalid packed comet provisional: " ++ packed
    | otherwise = do
        centNum <- getCenturyNum (packed !! 0)
        orderNum <- decodeCycleCount (take 2 $ drop 4 packed)
        let year = show centNum ++ take 2 (drop 1 packed)
            halfMonth = packed !! 3
            fragment = if length packed == 7
                       then [packed !! 6]
                       else take 2 $ drop 6 packed
            result = year ++ " " ++ [halfMonth] ++ show orderNum
        if fragment == "0"
            then Right result
            else Right $ result ++ "-" ++ map toUpper fragment

-- | Pack comet provisional designation
packCometProvisional :: String -> Either MPCError String
packCometProvisional unpacked = do
    (year, halfMonth, orderNum, fragment) <- parseComet unpacked
    let century = read (take 2 year) :: Int
    centCode <- getCenturyCode century
    orderEnc <- encodeCycleCount orderNum
    let fragCode = if null fragment then "0" else map toLower fragment
    return $ centCode : drop 2 year ++ [halfMonth] ++ orderEnc ++ fragCode
  where
    parseComet s = case words (trim s) of
        [y, rest] | length y == 4 && all isDigit y -> do
            let (hm, rest') = splitAt 1 rest
                (numPart, fragPart) = span isDigit rest'
            when (null hm || null numPart) $
                Left $ MPCError $ "Invalid comet provisional: " ++ s
            let frag = if "-" `isPrefixOf` fragPart
                       then drop 1 fragPart
                       else ""
            return (y, head hm, read numPart, frag)
        _ -> Left $ MPCError $ "Invalid comet provisional: " ++ s
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    isPrefixOf p xs = take (length p) xs == p
    when False _ = Right ()
    when True e = e

-- | Unpack numbered comet designation
unpackCometNumbered :: String -> Either MPCError String
unpackCometNumbered packed
    -- Match with optional fragment: "0073P", "0073Pa", "0073Paa"
    | len >= 5 && len <= 7 && all isDigit (take 4 packed) && packed !! 4 `elem` "PD" =
        let numStr = take 4 packed
            ctype = packed !! 4
            fragment = drop 5 packed
            num = read numStr :: Int
        in if null fragment
           then Right $ show num ++ [ctype]
           else Right $ show num ++ [ctype] ++ "-" ++ map toUpper fragment
    | otherwise = Left $ MPCError $ "Invalid packed numbered comet: " ++ packed
  where
    len = length packed

-- | Pack numbered comet designation
packCometNumbered :: String -> Either MPCError String
packCometNumbered unpacked
    | matchNumbered =
        if num < 1 || num > 9999
        then Left $ MPCError $ "Comet number out of range: " ++ show num
        else Right $ replicate (4 - length numStr) '0' ++ numStr ++ [ctype] ++ map toLower fragment
    | otherwise = Left $ MPCError $ "Invalid unpacked numbered comet: " ++ unpacked
  where
    trimmed = trim unpacked
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    -- Match "1P", "73P-A", "73P-AA", or "1P/Halley"
    (matchNumbered, numStr, num, ctype, fragment) =
        let (ns, rest) = span isDigit trimmed
        in if not (null ns) && length rest >= 1 && head rest `elem` "PD"
           then let afterType = drop 1 rest
                    frag = if "-" `isPrefixOf` afterType
                           then takeWhile isAlpha (drop 1 afterType)
                           else ""
                in (True, ns, read ns :: Int, head rest, frag)
           else (False, "", 0, ' ', "")
    isPrefixOf p xs = take (length p) xs == p

-- | Unpack full comet designation
unpackCometFull :: String -> Either MPCError String
unpackCometFull packed
    -- 8-char format: type + 7-char provisional
    | length packed == 8 && head packed `elem` cometTypes = do
        let ctype = head packed
            provPart = tail packed
        prov <- if isAsteroidStylePacked provPart
                then unpackProvisional provPart
                else unpackCometProvisional provPart
        return $ ctype : '/' : prov
    -- 9-char format: type + 8-char provisional with 2-letter fragment
    | length packed == 9 && head packed `elem` cometTypes = do
        let ctype = head packed
            provPart = tail packed
        prov <- unpackCometProvisional provPart
        return $ ctype : '/' : prov
    -- 12-char format
    | length packed == 12 || (length packed < 12 && head packed == ' ') = do
        let padded = replicate (12 - length packed) ' ' ++ packed
            numPart = trim $ take 4 padded
            ctype = padded !! 4
            provPart = drop 5 padded
        prov <- if isAsteroidStylePacked provPart
                then unpackProvisional provPart
                else unpackCometProvisional provPart
        if null numPart
            then return $ ctype : '/' : prov
            else return $ numPart ++ ctype : '/' : prov
    | otherwise = Left $ MPCError $ "Invalid packed full comet: " ++ packed
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    isAsteroidStylePacked s = length s == 7 && isUpper (last s)

-- | Pack full comet designation
packCometFull :: String -> Either MPCError String
packCometFull unpacked = do
    (numStr, ctype, year, provPart) <- parseFullComet unpacked

    -- Check for ancient/BCE year
    if year < 1000
        then packAncientCometProvisional ctype year provPart
        else do
            let provisional = show year ++ " " ++ provPart
            provPacked <- if isAsteroidStyleUnpacked provisional
                          then packProvisional provisional
                          else packCometProvisional provisional
            if null numStr
                then return $ ctype : provPacked
                else do
                    let num = read numStr :: Int
                    if num < 1 || num > 9999
                        then Left $ MPCError $ "Comet number out of range: " ++ show num
                        else return $ replicate (4 - length numStr) '0' ++ numStr ++ ctype : provPacked
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    parseFullComet s =
        let trimmed = trim s
            (numPart, rest) = span isDigit trimmed
            (ctypePart, rest') = splitAt 1 $ dropWhile (== ' ') rest
        in if null ctypePart || head ctypePart `notElem` cometTypes ||
              (not (null rest') && head rest' /= '/')
           then Left $ MPCError $ "Invalid comet designation: " ++ s
           else let afterSlash = drop 1 rest'
                    (yearStr, provRest) = span (\c -> isDigit c || c == '-') afterSlash
                    prov = trim $ drop 1 $ dropWhile (/= ' ') afterSlash
                in if null yearStr
                   then Left $ MPCError $ "Invalid comet designation: " ++ s
                   else Right (numPart, head ctypePart, read yearStr :: Int, prov)
    isAsteroidStyleUnpacked s = case words s of
        [_, rest] -> length rest >= 2 && isAlpha (rest !! 1)
        _ -> False

-- | Pack ancient comet provisional
packAncientCometProvisional :: Char -> Int -> String -> Either MPCError String
packAncientCometProvisional ctype year provPart = do
    (halfMonth, orderNum, fragment) <- parseProv provPart
    orderEnc <- encodeCycleCount orderNum
    let fragCode = if null fragment then "0" else map toLower fragment
    if year < 0
        then do
            let (prefix, code) = encodeBCEYear year
            return $ ctype : prefix : code ++ [halfMonth] ++ orderEnc ++ fragCode
        else do
            let yearStr = replicate (3 - length (show year)) '0' ++ show year
            return $ ctype : yearStr ++ [halfMonth] ++ orderEnc ++ fragCode
  where
    parseProv s = case span isAlpha s of
        ([hm], rest) ->
            let (numPart, fragPart) = span isDigit rest
                frag = if "-" `isPrefixOf` fragPart then drop 1 fragPart else ""
            in if null numPart
               then Left $ MPCError $ "Invalid ancient comet provisional: " ++ s
               else Right (hm, read numPart :: Int, frag)
        _ -> Left $ MPCError $ "Invalid ancient comet provisional: " ++ s
    isPrefixOf p xs = take (length p) xs == p
    encodeBCEYear y =
        let absYear = abs y
            code = 99 - (absYear `mod` 100)
            codeStr = replicate (2 - length (show code)) '0' ++ show code
            prefix = if absYear < 100 then '/'
                     else if absYear < 200 then '.'
                     else '-'
        in (prefix, codeStr)

-- | Unpack ancient comet provisional
unpackAncientCometProvisional :: String -> Either MPCError String
unpackAncientCometProvisional packed
    | length packed /= 8 = Left $ MPCError $ "Invalid ancient comet: " ++ packed
    | head packed `notElem` cometTypes = Left $ MPCError $ "Invalid comet type: " ++ [head packed]
    | packed !! 1 `elem` "/.-" = do  -- BCE
        let prefix = packed !! 1
            yearCode = take 2 $ drop 2 packed
            year = decodeBCEYear prefix yearCode
            halfMonth = packed !! 4
        orderNum <- decodeCycleCount (take 2 $ drop 5 packed)
        let fragment = [packed !! 7]
            result = head packed : '/' : show year ++ " " ++ [halfMonth] ++ show orderNum
        if fragment == "0"
            then Right result
            else Right $ result ++ "-" ++ map toUpper fragment
    | otherwise = do  -- Ancient (year 1-999)
        let year = read (take 3 $ drop 1 packed) :: Int
            halfMonth = packed !! 4
        orderNum <- decodeCycleCount (take 2 $ drop 5 packed)
        let fragment = [packed !! 7]
            result = head packed : '/' : show year ++ " " ++ [halfMonth] ++ show orderNum
        if fragment == "0"
            then Right result
            else Right $ result ++ "-" ++ map toUpper fragment
  where
    decodeBCEYear prefix code =
        let codeNum = read code :: Int
            yearPart = 99 - codeNum
        in case prefix of
            '/' -> -yearPart
            '.' -> -(yearPart + 100)
            '-' -> -(yearPart + 200)
            _   -> 0

-- | Unpack satellite designation
unpackSatellite :: String -> Either MPCError String
unpackSatellite packed
    | length packed /= 8 || head packed /= 'S' =
        Left $ MPCError $ "Invalid packed satellite: " ++ packed
    | otherwise = do
        centNum <- getCenturyNum (packed !! 1)
        let year = show centNum ++ take 2 (drop 2 packed)
            planet = packed !! 4
        when (planet `notElem` satellitePlanets) $
            Left $ MPCError $ "Invalid planet code: " ++ [planet]
        number <- decodeCycleCount (take 2 $ drop 5 packed)
        return $ "S/" ++ year ++ " " ++ [planet] ++ " " ++ show number
  where
    when False _ = Right ()
    when True e = e

-- | Pack satellite designation
packSatellite :: String -> Either MPCError String
packSatellite unpacked = do
    (year, planet, number) <- parseSatellite unpacked
    let century = read (take 2 year) :: Int
    centCode <- getCenturyCode century
    when (planet `notElem` satellitePlanets) $
        Left $ MPCError $ "Invalid planet: " ++ [planet]
    when (number < 1) $
        Left $ MPCError $ "Satellite number must be positive"
    numberEnc <- encodeCycleCount number
    return $ 'S' : centCode : drop 2 year ++ [planet] ++ numberEnc ++ "0"
  where
    when False _ = Right ()
    when True e = e
    parseSatellite s =
        let trimmed = trim s
        -- Format: S/YYYY P N where N can be 1+ digits (minimum length 10)
        in if take 2 trimmed == "S/" && length trimmed >= 10
           then let year = take 4 $ drop 2 trimmed
                    planet = trimmed !! 7
                    numPart = trim $ drop 9 trimmed
                in if all isDigit year && not (null numPart) && all isDigit numPart
                   then Right (year, planet, read numPart :: Int)
                   else Left $ MPCError $ "Invalid satellite designation: " ++ s
           else Left $ MPCError $ "Invalid satellite designation: " ++ s
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Detect the format of a designation
detectFormat :: String -> Either MPCError FormatInfo
detectFormat des
    -- Packed satellite (8 chars starting with S)
    | len == 8 && head trimmed == 'S' && trimmed !! 1 `elem` map fst centuryCodes &&
      all isDigit (take 2 $ drop 2 trimmed) && trimmed !! 4 `elem` satellitePlanets =
        Right $ FormatInfo Packed Satellite "natural satellite"

    -- Packed permanent tilde format
    | len == 5 && head trimmed == '~' =
        Right $ FormatInfo Packed Permanent "permanent numbered (tilde/base-62)"

    -- Packed permanent numeric
    | len == 5 && all isDigit trimmed =
        Right $ FormatInfo Packed Permanent "permanent numbered (5-digit)"

    -- Packed permanent letter-prefix
    | len == 5 && isAlpha (head trimmed) && all isDigit (tail trimmed) =
        Right $ FormatInfo Packed Permanent "permanent numbered (letter-prefix)"

    -- Packed provisional extended (no spaces)
    | len == 7 && head trimmed == '_' && ' ' `notElem` trimmed =
        Right $ FormatInfo Packed ProvisionalExtended "provisional extended"

    -- Packed provisional standard (no spaces allowed)
    | len == 7 && head trimmed `elem` map fst centuryCodes && isUpper (last trimmed) && ' ' `notElem` trimmed =
        Right $ FormatInfo Packed Provisional "provisional"

    -- Packed survey
    | len == 7 && take 3 trimmed `elem` map fst surveyPackedToUnpacked =
        Right $ FormatInfo Packed Survey "survey"

    -- Packed numbered comet (5 chars without fragment)
    | len == 5 && all isDigit (take 4 trimmed) && last trimmed `elem` "PD" =
        Right $ FormatInfo Packed CometNumbered "comet numbered"

    -- Packed numbered comet with fragment (6-7 chars: nnnnPf or nnnnPff)
    | (len == 6 || len == 7) && all isDigit (take 4 trimmed) &&
      trimmed !! 4 `elem` "PD" && all isLower (drop 5 trimmed) =
        Right $ FormatInfo Packed CometNumbered "comet numbered with fragment"

    -- Packed comet provisional (7 chars, no spaces)
    | len == 7 && head trimmed `elem` map fst centuryCodes && isDigit (last trimmed) && ' ' `notElem` trimmed =
        Right $ FormatInfo Packed CometProvisional "comet provisional"

    -- Packed ancient comet (8 chars: type + 3-digit year + provisional)
    | len == 8 && head trimmed `elem` cometTypes && ' ' `notElem` trimmed &&
      all isDigit (take 3 $ drop 1 trimmed) =
        Right $ FormatInfo Packed CometAncient "comet ancient"

    -- Packed BCE comet (8 chars: type + BCE prefix + 2-digit code + provisional)
    | len == 8 && head trimmed `elem` cometTypes && ' ' `notElem` trimmed &&
      trimmed !! 1 `elem` "./\\-" =
        Right $ FormatInfo Packed CometBCE "comet BCE"

    -- Packed comet full (8 chars, no spaces, second char is century code not slash)
    | len == 8 && head trimmed `elem` cometTypes && ' ' `notElem` trimmed &&
      trimmed !! 1 `elem` map fst centuryCodes =
        Right $ FormatInfo Packed CometFull "comet with provisional"

    -- Packed comet full with 2-letter fragment (9 chars)
    | len == 9 && head trimmed `elem` cometTypes && isLower (trimmed !! 7) && isLower (trimmed !! 8) =
        Right $ FormatInfo Packed CometFull "comet with 2-letter fragment"

    -- Packed comet full (12 chars)
    | length des == 12 && des !! 4 `elem` cometTypes =
        Right $ FormatInfo Packed CometFull "comet full format"

    -- Unpacked satellite
    | take 2 trimmed == "S/" && ' ' `elem` drop 7 trimmed =
        Right $ FormatInfo Unpacked Satellite "natural satellite"

    -- Unpacked permanent (all digits)
    | all isDigit trimmed && not (null trimmed) =
        Right $ FormatInfo Unpacked Permanent "permanent numbered"

    -- Unpacked survey
    | matchesSurvey trimmed =
        Right $ FormatInfo Unpacked Survey "survey"

    -- Unpacked provisional (standard or old-style)
    | matchesProvisional trimmed =
        Right $ FormatInfo Unpacked Provisional "provisional"

    -- Unpacked comet with type prefix
    | matchesCometFull trimmed =
        Right $ FormatInfo Unpacked CometFull "comet with type prefix"

    -- Unpacked numbered comet
    | matchesCometNumbered trimmed =
        Right $ FormatInfo Unpacked CometNumbered "comet numbered"

    | otherwise = Left $ MPCError $ "Unable to detect designation format: " ++ des
  where
    trimmed = trim des
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    len = length trimmed

    matchesSurvey s = case words s of
        [n, survey] -> all isDigit n && survey `elem` ["P-L", "T-1", "T-2", "T-3"]
        _ -> False

    matchesProvisional s =
        -- Standard: "1995 XA" or "1995 XA12"
        case words s of
            [y, rest] | length y == 4, all isDigit y,
                        length rest >= 2, isUpper (head rest), isUpper (rest !! 1) -> True
            _ -> -- Old-style: "A908 CJ"
                 length s == 7 && head s `elem` "AB" && all isDigit (take 3 $ drop 1 s) &&
                 s !! 4 == ' ' && isUpper (s !! 5) && isUpper (s !! 6)

    matchesCometFull s =
        let (numPart, rest) = span isDigit s
            afterNum = dropWhile (== ' ') rest
        in not (null afterNum) && head afterNum `elem` cometTypes &&
           length afterNum > 1 && afterNum !! 1 == '/'

    matchesCometNumbered s =
        let (numPart, rest) = span isDigit s
        in not (null numPart) && not (null rest) && head rest `elem` "PD" &&
           (length rest == 1 || rest !! 1 == '/' || rest !! 1 == '-')

-- | Convert a designation (returns info dict)
convert :: String -> Either MPCError (String, FormatInfo)
convert des = do
    info <- detectFormat des
    result <- case (formatType info, designation info) of
        (Packed, Permanent) -> show <$> unpackPermanent des
        (Packed, Provisional) -> unpackProvisional des
        (Packed, ProvisionalExtended) -> unpackExtendedProvisional des
        (Packed, Survey) -> unpackProvisional des
        (Packed, CometNumbered) -> unpackCometNumbered des
        (Packed, CometProvisional) -> unpackCometProvisional des
        (Packed, CometFull) -> unpackCometFull des
        (Packed, CometAncient) -> unpackAncientCometProvisional des
        (Packed, CometBCE) -> unpackAncientCometProvisional des
        (Packed, Satellite) -> unpackSatellite des
        (Unpacked, Permanent) -> packPermanent (read (trim des))
        (Unpacked, Provisional) -> packProvisional des
        (Unpacked, Survey) -> packProvisional des
        (Unpacked, CometNumbered) -> packCometNumbered des
        (Unpacked, CometFull) -> packCometFull des
        (Unpacked, Satellite) -> packSatellite des
        _ -> Left $ MPCError $ "Unhandled conversion: " ++ show info
    return (result, info)
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Simple conversion (just returns the converted string)
convertSimple :: String -> Either MPCError String
convertSimple des = fst <$> convert des

-- | Ensure designation is packed
pack :: String -> Either MPCError String
pack des = do
    info <- detectFormat des
    case formatType info of
        Packed -> Right $ trim des
        Unpacked -> convertSimple des
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Ensure designation is unpacked
unpack :: String -> Either MPCError String
unpack des = do
    info <- detectFormat des
    case formatType info of
        Unpacked -> Right $ trim des
        Packed -> convertSimple des
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- ===========================================================================
-- Helper functions for format conversion and fragment handling
-- ===========================================================================

-- | Convert minimal packed format to 12-character MPC observation report format.
-- The 12-char format is used in MPC observation records (columns 1-12).
toReportFormat :: String -> String
toReportFormat minimal
    -- Numbered comet with possible fragment: "0073P", "0073Pa", "0073Paa"
    | len >= 5 && len <= 7 && all isDigit (take 4 m) && m !! 4 `elem` "PD" =
        let basePart = take 5 m  -- "0073P"
            fragment = drop 5 m
        in if null fragment
           then basePart ++ replicate (12 - 5) ' '  -- left-align with trailing spaces
           else let padding = 12 - 5 - length fragment
                in basePart ++ replicate padding ' ' ++ fragment
    -- All other formats: right-align in 12 characters
    | otherwise = replicate (12 - len) ' ' ++ m
  where
    m = trim minimal
    len = length m
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Convert 12-character MPC report format to minimal packed format.
fromReportFormat :: String -> Either MPCError String
fromReportFormat report
    | length report /= 12 = Left $ MPCError "Report format must be exactly 12 characters"
    -- Check for numbered comet format: "0073P       " or "0073P      a" or "0073P     aa"
    | len5 >= 5 && all isDigit (take 4 trimLeft) && trimLeft !! 4 `elem` "PD" =
        let basePart = take 5 trimLeft
            afterBase = drop 5 report
            fragment = trim afterBase
        in if null fragment
           then Right basePart
           else Right $ basePart ++ fragment
    -- All other formats: just trim whitespace
    | otherwise = Right $ trim report
  where
    trimLeft = dropWhile (== ' ') report
    len5 = length (take 5 trimLeft)
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Check if designation has a comet fragment suffix.
-- Works with both packed and unpacked formats.
hasFragment :: String -> Bool
hasFragment des
    -- Unpacked numbered comet with fragment: "73P-A", "73P-AA"
    | matchUnpackedNumberedFragment = True
    -- Unpacked provisional comet with fragment: "D/1993 F2-A", "P/1930 J1-AA"
    | matchUnpackedProvFragment = True
    -- Packed numbered comet with fragment: "0073Pa", "0073Paa"
    | matchPackedNumberedFragment = True
    -- Packed provisional comet with fragment (full format): "DJ93F02a", "PJ30J01aa" (8-9 chars)
    | matchPackedProvFragment = True
    | otherwise = False
  where
    d = trim des
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    len = length d

    matchUnpackedNumberedFragment =
        let (numPart, rest) = span isDigit d
        in not (null numPart) && length rest >= 3 && head rest `elem` "PD" &&
           rest !! 1 == '-' && all isAlpha (drop 2 rest) && length (drop 2 rest) <= 2

    matchUnpackedProvFragment =
        len >= 11 && head d `elem` cometTypes && d !! 1 == '/' &&
        '-' `elem` drop 7 d

    matchPackedNumberedFragment =
        (len == 6 || len == 7) && all isDigit (take 4 d) && d !! 4 `elem` "PD" &&
        all isLower (drop 5 d)

    matchPackedProvFragment =
        (len == 8 || len == 9) && head d `elem` cometTypes &&
        d !! 1 `elem` map fst centuryCodes && isLower (last d)

-- | Extract fragment suffix from comet designation.
-- Returns uppercase fragment (e.g., "A", "AA"), empty string if no fragment.
-- Works with both packed and unpacked formats.
getFragment :: String -> String
getFragment des
    -- Unpacked numbered comet with fragment: "73P-A", "73P-AA"
    | matchUnpackedNumberedFragment = map toUpper fragUnpackedNum
    -- Unpacked provisional comet with fragment: "D/1993 F2-A", "P/1930 J1-AA"
    | matchUnpackedProvFragment = map toUpper fragUnpackedProv
    -- Packed numbered comet with fragment: "0073Pa", "0073Paa"
    | matchPackedNumberedFragment = map toUpper (drop 5 d)
    -- Packed provisional comet with fragment (full format): "DJ93F02a", "PJ30J01aa"
    | matchPackedProvFragment = map toUpper fragPackedProv
    | otherwise = ""
  where
    d = trim des
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    len = length d

    (matchUnpackedNumberedFragment, fragUnpackedNum) =
        let (numPart, rest) = span isDigit d
        in if not (null numPart) && length rest >= 3 && head rest `elem` "PD" &&
              rest !! 1 == '-' && all isAlpha (drop 2 rest) && length (drop 2 rest) <= 2
           then (True, drop 2 rest)
           else (False, "")

    (matchUnpackedProvFragment, fragUnpackedProv) =
        if len >= 11 && head d `elem` cometTypes && d !! 1 == '/'
        then case break (== '-') (drop 7 d) of
            (_, '-':frag) | not (null frag) && all isAlpha frag && length frag <= 2 -> (True, frag)
            _ -> (False, "")
        else (False, "")

    matchPackedNumberedFragment =
        (len == 6 || len == 7) && all isDigit (take 4 d) && d !! 4 `elem` "PD" &&
        all isLower (drop 5 d)

    (matchPackedProvFragment, fragPackedProv) =
        if (len == 8 || len == 9) && head d `elem` cometTypes &&
           d !! 1 `elem` map fst centuryCodes && isLower (last d)
        then let frag = if len == 8 then [last d] else drop 7 d
             in if all isLower frag then (True, frag) else (False, "")
        else (False, "")

-- | Get parent comet designation without fragment suffix.
-- Returns in same format (packed or unpacked) as input.
getParent :: String -> String
getParent des
    -- Unpacked numbered comet with fragment: "73P-A" -> "73P"
    | matchUnpackedNumberedFragment = parentUnpackedNum
    -- Unpacked provisional comet with fragment: "D/1993 F2-A" -> "D/1993 F2"
    | matchUnpackedProvFragment = parentUnpackedProv
    -- Packed numbered comet with fragment: "0073Pa" -> "0073P"
    | matchPackedNumberedFragment = take 5 d
    -- Packed provisional comet with fragment: "DJ93F02a" -> "DJ93F020"
    | matchPackedProvFragment = take 7 d ++ "0"
    -- No fragment found, return as-is
    | otherwise = d
  where
    d = trim des
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    len = length d

    (matchUnpackedNumberedFragment, parentUnpackedNum) =
        let (numPart, rest) = span isDigit d
        in if not (null numPart) && length rest >= 3 && head rest `elem` "PD" &&
              rest !! 1 == '-' && all isAlpha (drop 2 rest) && length (drop 2 rest) <= 2
           then (True, numPart ++ [head rest])
           else (False, "")

    (matchUnpackedProvFragment, parentUnpackedProv) =
        if len >= 11 && head d `elem` cometTypes && d !! 1 == '/'
        then case break (== '-') (drop 7 d) of
            (before, '-':frag) | not (null frag) && all isAlpha frag && length frag <= 2 ->
                (True, take 7 d ++ before)
            _ -> (False, "")
        else (False, "")

    matchPackedNumberedFragment =
        (len == 6 || len == 7) && all isDigit (take 4 d) && d !! 4 `elem` "PD" &&
        all isLower (drop 5 d)

    matchPackedProvFragment =
        (len == 8 || len == 9) && head d `elem` cometTypes &&
        d !! 1 `elem` map fst centuryCodes && isLower (last d)

-- | Check if two designations refer to the same object.
-- Normalizes both to packed format before comparing.
designationsEqual :: String -> String -> Bool
designationsEqual d1 d2 =
    case (pack d1, pack d2) of
        (Right p1, Right p2) -> p1 == p2
        _ -> False
