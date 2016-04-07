{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Exts
import Parsing
import Data.Aeson
import Data.Text
import qualified Data.Char as C
import System.IO
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Encoding as TE

commandFile :: FilePath
commandFile = "commands.txt"

type Degree = Int

data Color = Rojo | Amarillo | Azul

data Command = Fw Int | Rot Degree | Chg Color | Rep Int Command

colorToString :: Color -> String
colorToString Rojo = "rojo"
colorToString Amarillo = "amarillo"
colorToString Azul = "azul"


-- Esta instancia sirve para poder tratar como un objeto JSON a los comandos que represento con mi AST Command

instance ToJSON Command where
  toJSON (Fw n) = object ["command" .= pack "fw", "value" .= n]
  toJSON (Rot d) = object ["command" .= pack "rot", "value" .= d]
  toJSON (Chg c) = object ["command" .= pack "chg", "value" .= colorToString c]


main :: IO String
main = do putStrLn "Ingrese una expresion"
          s <- getLine
          let (ast,resto):xs = (parse expr) s
          prettyPrint ast
          TIO.writeFile commandFile (TE.decodeUtf8 . encode $ ast)
          if resto == "" then main else putStrLn resto >> main



minusculas :: String -> String
minusculas [] = []
minusculas (x:xs) = (C.toLower x):(minusculas xs)


-- Ejercicio 1

expr :: Parser Command
expr = do symbol "fw"
          n <- integer
          return (Fw n)
       <|> do symbol "rot"
              n <- integer
              return (Rot n)
           <|> do symbol "chg"
                  c <- many letter
                  case minusculas c of
                    "rojo" -> return (Chg Rojo) 
                    "amarillo" -> return (Chg Amarillo)
                    "azul" -> return (Chg Azul)
              <|> do symbol "rep"
                     n <- integer
                     c <- expr1
                     return (Rep n c)

expr1 :: Parser Command
expr1 = do symbol "fw"
           n <- integer
           return (Fw n)
        <|> do symbol "rot"
               n <- integer
               return (Rot n)
            <|> do symbol "chg"
                   c <- many letter
                   case minusculas c of
                     "rojo" -> return (Chg Rojo) 
                     "amarillo" -> return (Chg Amarillo)
                     "azul" -> return (Chg Azul)


prettyPrint :: Command -> IO ()
prettyPrint (Fw n) = putStrLn ("fw" ++ " " ++ show n)
prettyPrint (Rot d) = putStrLn ("rot" ++ " " ++ show d)
prettyPrint (Chg c) = case c of
                      Rojo -> putStrLn ("chg" ++ " " ++ "rojo")
                      Amarillo -> putStrLn ("chg" ++ " " ++ "amarillo")
                      Azul -> putStrLn ("chg" ++ " " ++ "azul")
prettyPrint (Rep n c) = do putStr ("rep" ++ " " ++ show n ++ " ")
                           prettyPrint c

