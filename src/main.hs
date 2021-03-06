-- file main.hs
module Main where

import System.IO
import System.Directory

data Todo = Todo { id' :: Int, content :: String, done :: Bool } deriving (Read, Show)

type Filename = String

main :: IO ()
main = do
    list' "data.txt"
    todo <- getLine
    add $ ("data.txt", Todo 1 todo False)
    update $ ("data.txt", Todo 1 todo False)
    main
    return ()

add :: (Filename, Todo) -> IO ()
add (file, todoItem) = do
    appendFile file (show todoItem ++ "\n")

update :: (Filename, Todo) -> IO ()
update (file, todo) = do
    contents <- readFile file
    let todoItemList =  todofy $ lines $ contents
    putStrLn $  show todoItemList
        where todofy = map (\todo -> read todo :: Todo)

-- Versão usando fmap
list' :: Filename -> IO ()
list' file = do
    todoItems <- (unlines . map todofy . lines) <$> (readFile file)
    putStr todoItems
        where todofy todo = printify $ (read todo :: Todo)
              printify todo = checkbox (done todo) ++ show (content todo)
              checkbox status
                  | status == True = "[ x ] - "
                  | otherwise      = "[   ] - "

list :: Filename -> IO ()
list file = do
    contents <- readFile file
    let todoItemList =  todofy $ lines $ contents
    putStr $ unlines todoItemList
        where todofy = map (\todo -> printify $ (read todo :: Todo))
              printify todo = checkbox (done todo) ++ show (content todo)
              checkbox status
                  | status == True = "[ x ] - "
                  | otherwise      = "[   ] - "