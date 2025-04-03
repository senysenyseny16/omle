module Main (main) where

import Omle.Parser (parseScalar)
import Options.Applicative
import Text.Megaparsec
import Data.Text.IO as DTIO

data Input = Input
  {inputFile :: FilePath}
  deriving (Show)

input :: Parser Input
input = Input <$> strArgument (metavar "INPUT_FILE" <> help "File to parse")

yamlParse :: Input -> IO ()
yamlParse Input {inputFile = inputFile'} = do
  content <- DTIO.readFile inputFile'
  case parse parseScalar "" content of
    Left err -> Prelude.putStrLn $ "error: " ++ show err
    Right ast -> print ast

main :: IO ()
main = yamlParse =<< execParser opts
  where
    opts = info (input <**> helper) (fullDesc <> progDesc "YAML formatter and linter")
