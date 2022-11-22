-- ghc --make WordCount.hs
module Main where

import System.Environment
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.FilePath (takeExtension, takeFileName)

main :: IO ()
main = do
  args <- getArgs
  currentDir <- getCurrentDirectory
  files <- getDirectoryContents currentDir

  let pureFiles = reverse (filterFiles files args)

  fileContents <- mapM readFile pureFiles

  let wordCounts = map wcCount fileContents

  mapM_ putStr (formatOutput wordCounts pureFiles)

formatOutput :: [[Int]] -> [FilePath] -> [String]
formatOutput wcs files = formatOutput' wcs files [0, 0, 0]
  where
    formatOutput' :: [[Int]] -> [FilePath] -> [Int] -> [String]
    formatOutput' [] [] total = [fst (computeLine total "total" total)]
    formatOutput' [] files total = []
    formatOutput' wcs [] total = []

    formatOutput' (wc:wcs) (file:files) total = (fst result ++ "\n") : formatOutput' wcs files (snd result)
      where result = computeLine wc file total
    
    computeLine :: [Int] -> FilePath -> [Int] -> (String, [Int])
    computeLine wc file total = (show linesLength ++ "\t" ++ show wordsLength ++ "\t" ++ show totalLength ++ "\t" ++ takeFileName file, [totalLines, totalWords, totalTotal])
      where
        linesLength = head wc
        wordsLength = head $ tail wc
        totalLength = head $ tail $ tail wc

        totalLines = head total + linesLength
        totalWords = head (tail total) + wordsLength
        totalTotal = head (tail $ tail total) + totalLength


filterFiles :: [FilePath] -> [String] -> [FilePath]
filterFiles files filters = filter fileFilter files
  where
    fileFilter :: FilePath -> Bool
    fileFilter file = fileFilter' filters file

    fileFilter' :: [String] -> FilePath -> Bool
    fileFilter' [] path = False
    fileFilter' (x:xs) path
      | x == path = True
      | head x == '*' && tail x == takeExtension path = True
      | otherwise = fileFilter' xs path

wcCount :: String -> [Int]
wcCount str = [length (lines str), length (words str), length str]
