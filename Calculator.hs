{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Extended for the L.EIC Functional and Logic Programming Project
-}
module Main where

import Parsing
import Data.Char

type Name = String
type Env = [(Name, Integer)]

data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr  
          | Div Expr Expr  
          | Mod Expr Expr  
          | Var Name      
          deriving Show

data Command = Assign Name Expr   
             | Eval Expr        
             deriving Show

eval :: Env -> Expr -> Integer
eval env (Num n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2  
eval env (Div e1 e2) = eval env e1 `div` eval env e2 
eval env (Mod e1 e2) = eval env e1 `mod` eval env e2 
eval env (Var n)   = case lookup n env of       
                      Just val -> val
                      Nothing  -> error ("undefined variable: " ++ n)

command :: Parser Command
command = do v <- variable
             char '='
             e <- expr
             return (Assign v e)
          <|> do e <- expr
                 return (Eval e)

expr :: Parser Expr
expr = do t <- term
          exprCont t


exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               <|> do char '-'
                      t <- term
                      exprCont (Sub acc t)
               <|> return acc

              

term :: Parser Expr
term = do f <- factor
          termCont f


termCont :: Expr -> Parser Expr
termCont acc = (do char '*'
                   f <- factor
                   termCont (Mul acc f))
               <|> (do char '/'            
                       f <- factor
                       termCont (Div acc f))
               <|> (do char '%'            
                       f <- factor
                       termCont (Mod acc f))
               <|> return acc

factor :: Parser Expr
factor = (do v <- variable          
             return (Var v))
         <|> (do n <- natural
                 return (Num n))
         <|> (do char '('
                 e <- expr
                 char ')'
                 return e)
             

variable :: Parser Name
variable = many1 (satisfy isLetter)

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator [] (lines txt) 


calculator :: Env -> [String] -> IO ()
calculator env []  = return ()
calculator env (l:ls) = do
    let (output, env') = execute env l
    putStrLn output
    calculator env' ls  

execute :: Env -> String -> (String, Env)
execute env txt =
  case parse command txt of
    [(cmd, "")] -> case cmd of
      Assign v e ->
        let val = eval env e
            env' = (v, val) : env 
        in (show val, env') 
      Eval e ->
        let val = eval env e
        in (show val, env) 
    _ -> ("parse error; try again", env)