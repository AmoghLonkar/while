-- Mainly followed https://wiki.haskell.org/Parsing_a_simple_imperative_language
-- Also referenced https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf

module ParseWhile where

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
parantheses = Token.parens lexer
braces = Token.brackets lexer



