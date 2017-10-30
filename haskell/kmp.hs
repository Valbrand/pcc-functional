import Data.Array

initTable :: String -> Array Int Int
initTable [ch] = listArray (0, 1) [-1, 0]
initTable [] = listArray (0, 0) [-1]
initTable pattern@(ch0:ch1:str) =
  tailInitTable (listArray (0, lenPattern - 1) pattern) 1 0 lenPattern $ listArray (0, lenPattern) initArray
  where
    lenPattern = length pattern
    initArray =
      -- deciding whether a strict border exists for pattern[..1]
      if ch0 /= ch1 then
        -1:0:(take (lenPattern - 1) $ repeat (-1))
      else
        take (lenPattern + 1) $ repeat (-1)

checkPattern :: String -> String -> Int -> Int
checkPattern (chPattern:restPattern) (chText:restText) matches =
  if chText == chPattern then
    checkPattern restPattern restText (matches + 1)
  else
    matches
checkPattern _ _ matches = matches

tailInitTable :: Array Int Char -> Int -> Int -> Int -> Array Int Int -> Array Int Int
tailInitTable patternArray cursor matches lenPattern table
  | cursor >= lenPattern = table
  | (cursor + matches < lenPattern) && patternArray ! matches == patternArray ! (cursor + matches) =
    -- main loop
    -- we'll only care to try to refresh a certain entry in the table if it hasn't been set yet
    if table ! lookAheadIndex == (-1) then
      -- lookahead for border strictness check
      if lookAheadIndex == lenPattern || patternArray ! incrementedMatches /=  patternArray ! lookAheadIndex then
        -- success: set table entry, continue traversing pattern
        tailInitTable patternArray cursor incrementedMatches lenPattern $ table // [(lookAheadIndex, incrementedMatches)]
      else
        -- if the border that was found isn't strict, the strict border for the current substring length
        -- is the same as the strict border for a substring with length 
        -- equal to the amount of matches made so far
        tailInitTable patternArray cursor incrementedMatches lenPattern $ table // [(lookAheadIndex, table ! incrementedMatches)]
    else
      tailInitTable patternArray cursor incrementedMatches lenPattern table
  | otherwise =
    -- first check for empty strict border case, then adapt cursor value accordingly
    if matches == 0 && (cursor + 1 == lenPattern || patternArray ! 0 /= patternArray ! (cursor + 1)) then
      tailInitTable patternArray (cursor + cursorSkip) newMatchesValue lenPattern $ table // [(cursor + 1, 0)]
    else
      tailInitTable patternArray (cursor + cursorSkip) newMatchesValue lenPattern table
  where
    lookAheadIndex = cursor + matches + 1
    incrementedMatches = matches + 1
    matchesTableLookup = table ! matches
    cursorSkip = matches - matchesTableLookup
    newMatchesValue = max 0 matchesTableLookup

kmp :: String -> String -> [Int]
kmp pattern@(patCh:restPat) text@(txtCh:restTxt) = 
  reverse $ tailKMP lookupTable 0 (length pattern) (length text) pattern text []
  where
    lookupTable = initTable pattern
kmp _ _ = []

tailKMP :: Array Int Int -> Int -> Int -> Int -> String -> String -> [Int] -> [Int]
tailKMP lookupTable index lenPattern remainingLength pattern subText occurrences
  | lenPattern > remainingLength = occurrences
  | patternCheckResult == lenPattern =
      tailKMP lookupTable nextIndex lenPattern nextRemainingLength pattern nextSubText (index:occurrences)
  | otherwise = 
      tailKMP lookupTable nextIndex lenPattern nextRemainingLength pattern nextSubText occurrences
  where
    patternCheckResult = checkPattern pattern subText 0
    cursorSkip = patternCheckResult - lookupTable ! patternCheckResult
    nextIndex = index + cursorSkip
    nextRemainingLength = remainingLength - cursorSkip
    nextSubText = drop cursorSkip subText


main = do
  ptn <- return "abad"
  txt <- return "abdbabadabdababadbabd"
  abra <- return $ elems $ initTable ptn
  print $ ptn ++ ":"
  print abra
  result <- return $ kmp ptn txt
  print $ "looking for '" ++ ptn ++ "' in '" ++ txt
  print result
