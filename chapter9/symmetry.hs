myWords :: String -> [String]
myWords xs = go xs []
  where
    go "" acc = acc
    go xs' acc =
        let x = takeWhile (/= ' ') xs'
            rest =
                case dropWhile (/= ' ') xs' of
                    "" -> ""
                    (_:rest') -> rest'
         in go rest (acc ++ [x])
