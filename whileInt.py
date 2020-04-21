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
        if word in ['TRUE', 'FALSE']:
            word = word.lower()
        return word
    
    def getArray(self):
        array = ""
        self.nextChar()
        while self.current is not None and self.current != ']':
            array += self.current
            self.nextChar()
        self.nextChar()
        return [int(num) for num in array.split(',')]
    
    def getLogicOp(self):
        logicOp = ""
        while self.current is not None and not self.current.isspace():
            logicOp += self.current
            self.nextChar()
        return logicOp    
    
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
            
            if self.current in ['\xe2', '\xc2']:
                tokenVal = self.getLogicOp()
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
        self.op = 'skip'
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

class Semi(object):
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right

#Parser Class
#Parser reads procssed input string and builds an AST
class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.currentToken = lexer.exprToToken()
    
    def factor(self):
        token = self.currentToken
        if token.type == 'Keyword':
            if token.value == 'if':
                self.currentToken = self.lexer.exprToToken()
                condition = self.boolExpr()
                if self.currentToken.value == 'then':
                    self.currentToken = self.lexer.exprToToken()
                    ifState = self.relationExpr()
                if self.currentToken.value == 'else':
                    self.currentToken = self.lexer.exprToToken()
                    elseState = self.relationExpr()
                return If(condition, ifState, elseState)
            
            elif token.value == 'while':
                self.currentToken = self.lexer.exprToToken()
                condition = self.boolExpr()
                condFalse = Skip(Token('keyword','skip'))
                if self.currentToken.value == 'do':
                    self.currentToken = self.lexer.exprToToken()
                    if self.currentToken.value == '{':
                        condTrue = self.relationExpr()
                    else:
                        condTrue = self.relationVar()

                return While(condition, condTrue, condFalse)
            
            elif token.value == 'skip':
                node =  Skip(token)
            
            elif token.value in ['true', 'false']:
                node =  boolVarP(token)

        elif token.type == 'Variable':
            node =  Variable(token)
        
        elif token.type == 'Integer':
            node =  Num(token)
        
        if token.type == 'Logical':
            if token.value  == '¬':
                self.currentToken = self.lexer.exprToToken()
                if self.currentToken.value in ['true', 'false']:
                    node =  boolVarP(token)
                elif self.currentToken.value == '(':
                    node =  self.boolExpr()
        
        elif token.type == 'Parens':
            if token.value == '(':
                self.currentToken = self.lexer.exprToToken()
                node =  self.boolExpr()

            elif token.value == ')':
                self.currentToken = self.lexer.exprToToken()

        elif token.type == 'Braces':
            if token.value == '{':
                self.currentToken = self.lexer.tokenize()
                node =  self.relationExpr()
        
            elif token.value == '}':
                self.currentToken = self.lexer.exprToToken()
        
        elif token.type == 'Array':
            node =  self.Array(token)
        
        elif token.type == 'Semi':
            node = self.Semi(token)

        else:
            return node

    def arithVar(self):
        node = self.factor()
        token = self.lexer.exprToToken()
        while self.currentToken.type == 'Mul':
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node =  arithOp(left = node, right = self.factor(), op = token.type)
        return node

    def arithExpr(self):
        node = self.arithVar()
        while self.currentToken.type in ('Add', 'Sub'):
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = arithOp(left = node, right = self.arithVar(), op = token.type)
        return node

    def parseArith(self):
        return self.arithExpr()
    
    def boolVar(self):
        node = self.arithExpr()
        if self.currentToken.type == 'Relational':
            #print(self.current_token)
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = logicOp(left = node, right = self.arithExpr(), op = token.type)
        return node

    def boolExpr(self):
        node = self.boolVar()
        if self.currentToken.value in  ['∧', '∨']:
            #print(self.current_token)
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = logicOp(left = node, right = self.boolVar(), op = token.type)
        return node
    
    def parseBool(self):
        return self.boolExpr()

    def relationVar(self):
        node = self.boolExpr()
        if self.currentToken.type == 'Assignment':
            #print(self.current_token)
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = logicOp(left = node, right = self.boolExpr(), op = token.type)
        return node

    def relationExpr(self):
        node = self.relationVar()
        if self.currentToken.type == 'Semi':
            #print(self.current_token)
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = logicOp(left = node, right = self.relationVar(), op = token.type)
        return node
    
    def parseRel(self):
        return self.relationExpr()

class storeTable(states):
    # __init__ function
    def __init__(self):
        self = dict()
    
    def access(self, name):
        return self[name]

    def insertVal(self, name, value):
        self[name] = value

def evaluate(treeNode, stateTable):
    currentState = stateTable
    node = treeNode
    if node.op == 'if':
        if(evaluate(node.condition, stateTable)):
            evaluate(node.ifState, stateTable)
        else:
            evaluate(node.elseState, stateTable)
    elif node.op == 'while':
        while(evaluate(node.condition, stateTable)):
            evaluate(node.ifState, stateTable)
    elif node.op == 'skip':
        stateTable = stateTable
    elif node.token.type == 'Variable':
        #Check if it exists in store and update value
        if node.token.value in stateTable:
            return stateTable.access(node.token.value)
        #Else initialize with value 0
        else:
            stateTable.insertVal(node.token.value, 0)
    elif node.token.type in ['Integer', 'boolVarP', 'Array']:
        return node.token.value
    elif node.op.type == 'Add':
        return evaluate(node.left, stateTable) + evaluate(node.right, stateTable)
    elif node.op.type == 'Sub':
        return evaluate(node.left, stateTable) - evaluate(node.right, stateTable)
    elif node.op.type  == 'Mul':
        return evaluate(node.left, stateTable) * evaluate(node.right, stateTable)
    elif node.op.type == '∧':
        return evaluate(node.left, stateTable) and evaluate(node.right, stateTable)
    elif node.op.type == '∨':
        return evaluate(node.left, stateTable) or evaluate(node.right, stateTable)
    elif node.op == '¬':
        return not evaluate(node, stateTable)
    elif node.op.type == '=':
        return evaluate(node.left, stateTable) == evaluate(node.right, stateTable)
    elif node.op.type == '<':
        return evaluate(node.left, stateTable) < evaluate(node.right, stateTable)
    elif node.op.type == 'Assignment':
            stateTable.insertVal(node.left.value, evaluate(node.right, stateTable))
    elif node.op.type == 'semi':
        evaluate(node.left, stateTable)
        evaluate(node.right, stateTable)
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
