import Data.Function ((&))
import System.IO
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)

mult :: Parser Integer
mult =
  try
    ( string "mul("
        *> pure (*)
        <*> number
        <* char ','
        <*> number
        <* char ')'
    )
    <|> (anyChar *> pure 0)
 where
  number = read <$> many1 digit

partOne = sum <$> many1 mult

partTwo =
  foldl (&) (1, 0)
    <$> ( many1 $
            (try (string "do()") *> pure (\x -> (1, snd x)))
              <|> (try (string "don't()") *> pure (\x -> (0, snd x)))
              <|> ((\y x -> (fst x, fst x * y + snd x)) <$> mult)
        )

main = do
  input <- readFile "input"
  print (Text.Parsec.runParser partOne () "" input)
  print (Text.Parsec.runParser partTwo () "" input)
