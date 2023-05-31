--import Text.CSV
import Data.List

main :: IO ()
main = do
    let fileName = "release_dates.csv" -- title,year
    input <- readFile fileName
    let csv = parseCSV fileName input
    either handleError doWork csv
    
handleError csv = putStrLn "error parsing"

title :: Record -> String
title [a, b, c, d] = show a

year :: Record -> Int
year [a, b, c, d] = toInt b

toInt :: String -> Int
toInt = read

country :: Record -> String
country [a, b, c, d] = show c

date :: Record -> String
date [a, b, c, d] = show d

-- Recorremos todo el archivo y buscamos los que tengan el mismo nombre 
-- pero que sean iguales que una fecha introducida

doWork = print . filterPorFechas "2014-10-16"

filterPorFechas :: String -> [Record] -> [Record]
filterPorFechas s = map (\s -> s . date) (filterTitles "$71")

filterTitles :: String -> [Record]
filterTitles nombre = filter(isPrefixOf nombre . title)