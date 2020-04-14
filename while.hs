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

data arithExpr = Int n 
	       | Var x 
	       | ABinary arithOp arithExpr arithExpr 
		 deriving (Show)

data arithOp = add 
	     | sub 
             | mul 
               deriving (Show)

data booleanExpression = BoolConst p 
			| Comparison compOp booleanExpression booleanExpression 
			| Neg booleanExpression 
			| Logical logicOp booleanExpression booleanExpression 
			  deriving (Show)

data compOp = equality 
	    | lessThan 
	      deriving (Show)

data logicOp = and 
	     | or 
	       deriving (Show)

data command = skip 
	     | Seq [command]
	     | assignment x arithmeticExpression 
	     | If booleanExpression command command 
	     | While booleanExpression command 
	       deriving (Show)
