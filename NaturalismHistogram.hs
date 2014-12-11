import Data.Ord (comparing)
import Data.List (sortBy, permutations, intersperse)
import NaturalLanguageModule (naturalismDefault)
import BlindtextModule (blindtext1)
import SnModule (perm, Direction (..))

main :: IO()
main = putStrLn tableString

score :: [Int] -> Double
score x = naturalismDefault (perm Encrypt x blindtext1)

keyLength :: Int
keyLength = 4

perms :: [[Int]]
perms = permutations [1 .. keyLength]

pair :: (a -> b) -> a -> (a,b)
pair f x = (x, f x)

pairs :: [([Int], Double)]
pairs = map (pair score) perms

tableString :: String
tableString = concat . intersperse "\n". map (\(a,b) -> "\"" ++ show a ++ "\"; " ++ show b) . reverse . sortBy (comparing snd)$ pairs
