bruteForce :: String -> String -> [Int]
bruteForce pattern text = reverse $ tailBruteForce 0 (length pattern) (length text) [] pattern text

tailBruteForce :: Int -> Int -> Int -> [Int] -> String -> String -> [Int]   
tailBruteForce _ _ _ occurrences _ [] = occurrences
tailBruteForce index lenPattern remainingLength occurrences pattern subText =
  | lenPattern > remainingLength = occurrences
  | checkPattern subText pattern 0 == lenPattern =
    tailBruteForce (index + 1) lenPattern (remainingLength - 1) (index:occurrences) pattern $ tail subText
  | otherwise =
    tailBruteForce (index + 1) lenPattern (remainingLength - 1) occurrences pattern $ tail subText

checkPattern :: String -> String -> Int -> Int
checkPattern (chText:restText) (chPattern:restPattern) matches =
  if chText == chPattern then
    checkPattern restText restPattern (matches + 1)
  else
    matches
checkPattern _ _ matches = matches