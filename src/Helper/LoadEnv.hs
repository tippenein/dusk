module Helper.LoadEnv
    ( loadEnv
    , loadEnvFrom
) where

import ClassyPrelude.Yesod hiding ((<|>), try, many, optional)
import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Environment (setEnv)
import Text.Parsec
import Text.Parsec.String

loadEnv :: IO ()
loadEnv = loadEnvFrom ".env"

loadEnvFrom :: FilePath -> IO ()
loadEnvFrom fp = do
    e <- doesFileExist fp

    when e $ parseFromFile parseEnvironment fp >>=
        either print (mapM_ $ uncurry setEnv)

type Environment = [Variable]
type Variable = (String, String)

parseEnvironment :: Parser Environment
parseEnvironment = catMaybes <$> many parseLine

parseLine :: Parser (Maybe Variable)
parseLine = possibly parseVariable

possibly :: Parser a -> Parser (Maybe a)
possibly p = try (fmap Just p) <|> ignored
  where
    ignored = do
        void $ manyTill anyToken newline
        return Nothing

parseVariable :: Parser Variable
parseVariable = do
    optional $ between spaces spaces $ string "export"

    i <- identifier
    void $ char '='

    v <- value
    void $ many $ oneOf " \t"
    void $ newline

    return (i, v)

identifier :: Parser String
identifier = do
    x <- upper <|> underscore
    ys <- many $ upper <|> digit <|> underscore

    return (x:ys)

  where
    underscore = char '_'

value :: Parser String
value = quotedValue <|> unquotedValue <|> return ""

quotedValue :: Parser String
quotedValue = do
    q <- oneOf "'\""

    manyTill (try (escaped q) <|> anyToken) (char q)

unquotedValue :: Parser String
unquotedValue = many1 $ try (escaped ' ') <|> noneOf "\"' \n"

escaped :: Char -> Parser Char
escaped c = string ("\\" ++ [c]) >> return c
