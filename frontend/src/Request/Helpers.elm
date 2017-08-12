module Request.Helpers exposing (apiUrl)


apiUrl : String -> String
apiUrl str =
    "localhost:3000" ++ str
