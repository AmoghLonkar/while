# coding=utf-8
import sys

#Token Class
#Token has type and value
#Token types: integer, add, sub, mul
class Token:

    #Token "constructor"
    def __init__(self, type,value):
        self.type = type
        self.value = value

    #Display string for debugging
    def printToken(self):
          print(Token({type}, {value}).format(type=self.type, value=repr(self.value)))


#Lexer Class
#Lexer reads input string and splits into tokens
class Lexer:

    #Lexer "constructor"
    def __init__(self, expression):
        self.expression = expression
        self.index = 0
        self.current = self.expression[self.index]

    def __getitem__(self, index):
        return self.index

    #To help in iterating through expression
    def nextChar(self):
        self.index += 1
        if(self.index) > len(self.expression) - 1:
            self.current = None#'None'
        else:
            self.current = self.expression[self.index]

    #Accounting for multi-digit operands
    def intVal(self):
        operand = ''
        while self.current is not None and self.current.isdigit():
            operand += self.current
            self.nextChar()

        return int(operand)


    #Checking for negative numbers
    def negInt(self):
        if self.current == '-' and self.expression[self.index + 1].isdigit():
            self.nextChar()
            value = self.intVal() * -1
        return value
    
    #Getting multi-char objects
    def getWord(self):
        word = ""
        while self.current is not None and (self.current.isalpha() or self.current.isdigit()):
            word += self.current
            self.nextChar()
        return word
    
    def getArray(self):
        array = ""
        self.nextChar()
        while self.current is not None and self.current != ']':
            array += self.current
            self.nextChar()
        self.nextChar()
        return [int(num) for num in array.split(',')]
    
    def exprToToken(self):
        while self.current is not None:
            if self.current.isalpha():
                word = self.getWord()
                if word in ['if', 'then', 'else', 'while', 'do', 'skip', 'true', 'false']:
                    return Token('Keyword', word)
                else:
                    return Token('Variable', word)
            
            if self.current.isdigit():
                return Token('Integer', self.intVal())

            if self.current == '-' and self.expression[self.index + 1].isdigit():
                return Token('Integer', self.negInt())

            if self.current == '+':
                self.nextChar()
                return Token('Add', '+')

            if self.current == '-' and self.expression[self.index + 1].isspace():
                self.nextChar()
                return Token('Sub', '-')

            if self.current == '*':
                self.nextChar()
                return Token('Mul', '*')
            
            if self.current == ':':
                self.nextChar()
                if self.current == '=':
                    self.nextChar()
                    return Token('Assignment', ':=')

            if self.current in ['=', '<']:
                tokenVal = self.current
                self.nextChar()
                return Token('Relational', tokenVal)
            
            if self.current in ['∧', '∨', '¬']:
                tokenVal = self.current
                self.nextChar()
                return Token('Logical', tokenVal)
            
            if self.current == ';':
                self.nextChar()
                return Token('Semi', ';')

            if self.current in [ '(', ')']:
                tokenVal = self.current
                self.nextChar()
                return Token('Parens', tokenVal)
            
            if self.current in [ '{', '}']:
                tokenVal = self.current
                self.nextChar()
                return Token('Braces', tokenVal)

            if self.current == '[':
                array = self.getArray();
                return Token('Array', array)
            
            #Removing white spaces
            if self.current.isspace():
                self.nextChar()
                continue

        return Token('EOF', None)


class Num(object):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Variable(object):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class arithOp(object):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class boolVarP(object):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class relatOp(object):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Not(object):
    def __init__(self, node):
        self.op = '¬'
        self.value = node

class logicOp(object):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class Skip(object):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Assign(object):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class If(object):
    def __init__(self, condition, ifState, elseState):
        self.op = 'if'
        self.condition = condition
        self.ifState = ifState
        self.elseState = elseState
        
class While(object):
    def __init__(self, condition, condTrue, condFalse):
        self.op = 'while'
        self.condition = condition
        self.condTrue = condTrue
        self.condFalse = condFalse

class Array(object):
    def __init__(self, token):
        self.token = token
        self.value = token.value

#Parser Class
#Parser reads procssed input string and builds an AST
class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.currentToken = self.lexer.exprToToken()
    
    def factor(self):
        token = self.currentToken
        if token.type == 'Keyword':
            if token.value == 'if':
                self.currentToken = self.lexer.exprToToken()
                condition = self.boolExpr()
                if self.currentToken.value == 'then':
                    self.currentToken = self.lexer.exprToToken()
                    ifState = self.relationExpr()
                if self.current_token.value == 'else':
                    self.currentToken = self.lexer.exprToToken()
                    elseState = self.relationExpr()
                return If(condition, ifState, elseState)
            
            elif token.value == 'while':
                self.currentToken = self.lexer.exprToToken()
                condition = self.boolExpr()
                condFalse = Skip(Token('keyword','skip'))
                if self.current_token.value == 'do':
                    self.currentToken = self.lexer.exprToToken()
                    if self.currentToken.value == '{':
                        condTrue = self.relationExpr()
                else:
                    condTrue = self.relationVal()

                return While(condition, condTrue, condFalse)
            
            elif token.value == 'skip':
                return Skip(token)
            
            elif token.value in ['true', 'false']:
                return boolVarP(token)

        elif token.type == 'Variable':
            return Variable(token)
        
        elif token.type == 'Integer':
            return Num(token)
        
        if token.type == 'Logical':
            if token.value  == '¬':
                self.currentToken = self.lexer.exprToToken()
                if self.currentToken.value in ['true', 'false']:
                    return boolVarP(token)
                elif self.currentToken.value == '(':
                    return self.boolExpr()
        
        elif token.type == 'Parens':
            if token.value == '(':
                self.currentToken = self.lexer.exprToToken()
                return self.boolExpr()

            elif token.value == ')':
                self.currentToken = self.lexer.exprToToken()

        elif token.type == 'Braces':
            if token.value == '{':
                self.currentToken = self.lexer.tokenize()
                return self.relationExpr()
        
            elif token.value == '}':
                self.currentToken = self.lexer.exprToToken()
        
        elif token.type == 'Array':
            return self.Array(token)
"""
    def arithVar(self):
        
    def arithExpr(self):

    def boolVar(self):
    def boolExpr(self):
    def relationVar(self):
    def relationExpr(self):
    
"""
def main():
    while True:
        try:
            expression = input("")
        except EOFError:
            break
        tokens = Lexer(expression)
        parser = Parser(tokens)
        #interpreter = Interpreter(parser)
        #result = interpreter.interpret()
        #print(str(result))

if __name__ == "__main__":
    main()
