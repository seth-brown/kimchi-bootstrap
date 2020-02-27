{-  Seth Brown
    2017-09-05
    software AT seth-brown DOT NET
-}

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified System.Environment as SE
import qualified Text.Read as TR
import qualified Data.List as DL
import Text.Printf

main :: IO ()
main = do
    scale <- getScale
    mapM_ putStrLn $ output $ (scaleFn scale oneXkimchi)

getScale :: IO Float
getScale = fmap (TR.read . head) SE.getArgs
oneXkimchi = map (\(a,b,c) -> Ingredient a b c) reagents
scaleFn scale = map (\d -> Ingredient (item d) (unit d) (scale * amount d))
fmt i = [item i, printf "%.1f" (amount i:: Float) ++ " " ++ unit i]
underscore2spaces = map (\d -> if d=='_' then ' '; else d)
output = map (underscore2spaces . DL.intercalate "\t" . fmt)

reagents = [  
               ("cabbage", "", 1.0)
             , ("red_chili_flakes", "Cup", 0.5)
             , ("ginger", "Tbsp", 2.0)
             , ("garlic", "Tbsp", 3)
             , ("water", "Cup", 0.75)
             , ("gochujang", "Tbsp", 0.5)
             , ("radish", "Cup", 1.0)
             , ("carrot", "", 2.0)
             , ("green_onion", "", 5)
             , ("asian_chives", "", 4.0)
             , ("rice_powder", "Tbsp", 1)
           ]

data Ingredient = Ingredient
    { item                     :: String
    , unit                     :: String
    , amount                   :: Float
    } deriving (Show, Eq)
