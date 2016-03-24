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
    todoItems <- fmap (unlines . map todofy . lines) (readFile file)
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

-- List
-- 1 - [   ] - Tarefa 1
-- 2 - [   ] - Tarefa 2
-- 3 - [   ] - Tarefa 3

-- Complete 3
-- 1 - [   ] - Tarefa 1
-- 2 - [   ] - Tarefa 2
-- 3 - [ x ] - Tarefa 3

-- Edit 2
-- "Task 2"
-- 1 - [   ] - Tarefa 1
-- 2 - [   ] - Task 2
-- 3 - [ x ] - Tarefa 3

-- Delete 1
-- 2 - [   ] - Task 2
-- 3 - [ x ] - Tarefa 3

-- Add
-- "Tarefa 4"
-- 2 - [   ] - Task 2
-- 3 - [ x ] - Tarefa 3
-- 4 - [ x ] - Tarefa 4