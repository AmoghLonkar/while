-- Mainly followed: https://wiki.haskell.org/Parsing_a_simple_imperative_language
--                  https://www.youtube.com/watch?v=N9RUqGYuGfw
-- Also referenced https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf

module Main where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

--Defining Grammar as done during Lecture

data ArithExpr = Int Integer
               | Var String 
               | ArithEval ArithOp ArithExpr ArithExpr
                 deriving (Show)

data ArithOp  = Add 
	      | Sub 
              | Mul
                deriving (Show)

data BoolExpr = BoolConst Bool 
	      | Comparison CompOp BoolExpr BoolExpr 
	      | Neg BoolExpr 
	      | Logical LogicOp BoolExpr BoolExpr 
	        deriving (Show)

data CompOp = Equality 
	    | LessThan 
	      deriving (Show)

data LogicOp = And 
	     | Or 
	       deriving (Show)

data Command = Skip 
	     | Seq [Command]
	     | Assignment String ArithExpr 
	     | If BoolExpr Command Command 
	     | While BoolExpr Command 
	       deriving (Show)

--Specifying reserved keywords and operators in syntax

syntaxDef = emptyDef {  Token.reservedNames = ["if", "then", "else", "while", "do", "true", "false", "skip"],
			Token.reservedOpNames = [":=", "+", "-", "*", "∧", "∨", "¬", "<"]
}

--Lexer using Text.Parsec.Token to split input into predefined tokens

lexer = Token.makeTokenParser syntaxDef

varName = Token.identifier lexer
keyword = Token.reserved lexer
operator = Token.reservedOp lexer
operand = Token.integer lexer
spaces = Token.whiteSpace lexer
parentheses = Token.parens lexer
braces = Token.brackets lexer

--Extracting one command from a sequence

command :: Parser Command
command = parentheses command
       <|> multipleCommands

multipleCommands =
   do list <- (sepBy1 firstCommand split)
      return $ if length list == 1 then head list else Seq list

--Classifying the command

firstCommand' :: Parser Command
firstCommand' = ifCommand
	     <|> whileCommand
	     <|> skipCommand
	     <|> assignCommand


--Parsers for each command

ifCommand :: Parser Command 
ifCommand = do keyword "if"
	       condition <- boolExpr
	       keyword "then"
	       condTrue <- command
	       keyword "else"
	       condFalse <- command
	       return $ If condition condTrue condFalse	

whileCommand :: Parser Command
whileCommand = do keyword "while"
	          loopCond <- boolExpr
	   	  keyword "do"
	   	  loopBody <- command
	   	  return $ While loopCond loopBody

assignCommand :: Parser Command
assignCommand = do varName <- varToAssign
	           operator ":="
	   	   expression <- arithExpr
           	   return $ Assign varName expression

skipCommand :: Parser Command
skipCommand = do keyword "skip" 
	   	 return Skip

--Parsing Arithmetic and Boolean Expressions using 
--buildExpressionParser from Text.ParserCombinators.Parsec.Expr

arithExpr :: Parser ArithExpr
arithExpr = buildExpressionParser arithOp arithVar

boolExpr :: Parser BoolExpr
boolExpr = buildExpressionParser boolOp boolVar

--Defining operator precedence for arithmetic and boolean expressions

arithOp = [ 
	    [Infix, (operator "*" >> return (ArithEval Mul)) AssocLeft], 
	    [Infix, (operator "+" >> return (ArithEval Add)) AssocLeft],
            [Infix, (operator "-" >> return (ArithEval Sub)) AssocLeft]
	  ]

boolOp = [ [Prefix, (operator "¬" >> return (Not ))],
	   [Infix, (operator "∧" >> return (Logical And)) AssocLeft],
	   [Infix, (operator "∨" >> return (Logical Or)) AssocLeft]
	 ]
 
arithVar = parentheses arithExpr
	<|> liftM Var identifier
 	<|> liftM Int integer

boolVar = parentheses boolExpr
       <|> (keyword "true" >> return (BoolConst True))
       <|> (keyword "false" >> return (BoolConst False))
       <|> relationalExpr

relationalExpr = do lhs <- arithExpr
		    op <- relationalOp 
		    rhs <- arithExpr
		    return Comparison op lhs rhs

relationalOp = (keyword "=" return Equality)
	    <|> (keyword "<" return LessThan)	

