module Main (main) where

import qualified Data.Text.IO as DTIO
import Omle.Parser (parseSequence)
import Options.Applicative
import Text.Megaparsec

data Input = Input
  {inputFile :: FilePath}
  deriving (Show)

input :: Parser Input
input = Input <$> strArgument (metavar "INPUT_FILE" <> help "File to parse")

yamlParse :: Input -> IO ()
yamlParse Input {inputFile = inputFile'} = do
  content <- DTIO.readFile inputFile'
  case parse parseSequence "" content of
    Left err -> putStrLn $ "error: " ++ show err
    Right ast -> print ast

main :: IO ()
main = yamlParse =<< execParser opts
  where
    opts = info (input <**> helper) (fullDesc <> progDesc "YAML formatter and linter")
