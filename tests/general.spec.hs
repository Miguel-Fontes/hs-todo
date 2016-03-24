-- file general.spec.hs
module GeneralSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.IO
import System.Directory -- Contem o doesFileExist

main :: IO()

-- Nome do arquivo de testes para simplificar
testFileName :: String
testFileName  = "data.txt"

-- Helper function para retornar a quantidade de linhas de um arquivo
numberOfLines = length . lines

main = hspec $ do
    describe "Testes Genericos com arquivos" $ do
        -- IO Modes -> ReadMode | WriteMode | AppendMode | ReadWriteMode
        it "deve criar um novo arquivo" $ do
            handle <- openFile testFileName WriteMode
            fileExists <- doesFileExist testFileName
            fileExists `shouldBe` True
            hClose handle

        it "deve deletar um arquivo" $ do
            removeFile testFileName
            fileExists <- doesFileExist testFileName
            fileExists `shouldBe` False

        it "deve criar um arquivo e adicionar conteudo" $ do
            writeFile testFileName ("AEEEEHOOOOOOOOOOOOO")
            contents <- readFile  testFileName
            contents `shouldBe` "AEEEEHOOOOOOOOOOOOO"

        it "deve appendar conteudo no arquivo" $ do
            writeFile  testFileName ("1 - Registro \n")
            appendFile testFileName ("2 - Registro \n")
            appendFile testFileName ("3 - Registro \n")
            appendFile testFileName ("4 - Registro \n")
            appendFile testFileName ("5 - Registro \n")

            fileContents <- readFile testFileName
            numberOfLines fileContents `shouldBe` 5

        it "deve atualizar o registro 3 no arquivo" $ do
            fileContents <- readFile testFileName

            let contentList = lines $ fileContents
                updatedContent = map (\x -> if x == "3 - Registro " then "3 - Treta " else x) contentList

            (tempFileName, tempHandle) <- openTempFile "." ".temp"
            hPutStrLn tempHandle $ unlines updatedContent
            hClose tempHandle

            removeFile testFileName
            renameFile tempFileName testFileName

            newfileContents <- readFile testFileName
            numberOfLines fileContents `shouldBe` 5




