module MLF.Util.Names (
    parseNameId
) where

parseNameId :: String -> Maybe Int
parseNameId name =
    case name of
        ('t':rest) ->
            case reads rest of
                [(n, "")] -> Just n
                _ -> Nothing
        _ -> Nothing
