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
        self.state = {}
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
        self.op = 'Integer'
        self.token = token
        self.value = token.value

class Variable(object):
    def __init__(self, token):
        self.op = 'Variable'
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
        self.op = 'boolVarP'
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
        self.op = 'Array'
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
        self.state = lexer.state
        self.currentToken = lexer.exprToToken()

    def factor(self):
        token = self.currentToken
        if token.value == 'if':
            self.currentToken = self.lexer.exprToToken()
            condition = self.boolExpr()
            if self.currentToken.value == 'then':
                self.currentToken = self.lexer.exprToToken()
                ifState = self.semiExpr()
            if self.currentToken == 'else':
                self.currentToken = self.lexer.exprToToken()
                elseState = self.semiExpr()
            return If(condition, ifState, elseState)

        elif token.value == 'while':
            self.currentToken = self.lexer.exprToToken()
            condition = self.boolExpr()
            condFalse = Skip(Token('Keyword', 'skip'))
            if self.currentToken.value == 'do':
                self.currentToken = self.lexer.exprToToken()
                condTrue = self.semiExpr()
            return While(condition, condTrue, condFalse)

        elif token.value in ['true', 'false']:
            self.currentToken = self.lexer.exprToToken
            node = boolVarP(token)

        elif token.value == 'skip':
            node = Skip(token)

        elif token.type == 'Integer':
            node = Num(token)

        elif token.type == 'Variable':
            node = Variable(token)

        elif token.value == '¬':
            self.currentToken = self.lexer.exprToToken()
            if self.currentToken.value == '(':
                self.currentToken = self.lexer.exprToToken()
                node = self.boolExpr()
            elif self.currentToken.value in ['true', 'false']:
                node = boolVarP(token)
            node = Not(node)
        
        elif token.value == '(':
            self.currentToken = self.lexer.exprToToken()
            node = self.boolExpr()

        elif token.value == ')':
            self.currentToken == self.lexer.exprToToken()

        elif token.value == '{':
            self.currentToken = self.lexer.exprToToken()
            node = self.semiExpr()

        elif token.value == '}':
            self.currentToken = self.lexer.exprToToken()

        elif token.type == 'Array':
            node = Array(token)

        self.currentToken = self.lexer.exprToToken()
        return node 

    def arithVar(self):
        node = self.factor()
        while self.currentToken.type == 'Mul':
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = arithOp(node, token.type, self.factor())
        return node

    def arithExpr(self):
        node = self.arithVar()
        while self.currentToken.type in ['Add', 'Sub']:
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = arithOp(node, token.type, self.arithVar())
        return node

    def relationExpr(self):
        node = self.arithExpr()
        if self.currentToken.value in ['=', '<']:
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = arithOp(node, token.value, self.arithExpr())
        return node

    def boolExpr(self):
        node = self.relationExpr()
        while self.currentToken.value in ['∧', '∨']:
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = arithOp(node, token.value, self.relationExpr())
        return node

    def assignExpr(self):
        node = self.boolExpr()
        if self.currentToken.type == 'Assignment':
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = Assign(node, token.type, self.boolExpr())
        return node
    
    def semiExpr(self):
        node = self.assignExpr()
        while self.currentToken.type == 'Semi':
            token = self.currentToken
            self.currentToken = self.lexer.exprToToken()
            node = Semi(node, token.type, self.assignExpr())
        return node

    def parseExpr(self):
        return self.semiExpr()
    
def evaluate(treeNode, stateTable, modifiedVar):
    node = treeNode
    stateTable = stateTable
    modifiedVar = modifiedVar

    if node.op == 'if':
        if(evaluate(node.condition, stateTable, modifiedVar)):
            evaluate(node.ifState, stateTable, modifiedVar)
        else:
            evaluate(node.elseState, stateTable, modifiedVar)
        
    elif node.op == 'while':
        while(evaluate(node.condition, stateTable, modifiedVar)):
            evaluate(node.condTrue, stateTable, modifiedVar)
        
    elif node.op == 'skip':
        stateTable = stateTable
        
    elif node.op == 'Variable':
        #Check if it exists in store and update value
        if node.value in stateTable:
            return stateTable[node.value]
        #Else initialize with value 0
        else:
            stateTable.update({node.value: 0})
            #stateTable[node.value] = 0
            return 0 
    elif node.op in ['Integer', 'boolVarP', 'Array']:
        return node.value
        
    elif node.op == 'Add':
        return evaluate(node.left, stateTable, modifiedVar) + evaluate(node.right, stateTable, modifiedVar)
    
    elif node.op == 'Sub':
        return evaluate(node.left, stateTable, modifiedVar) - evaluate(node.right, stateTable, modifiedVar)
        
    elif node.op  == 'Mul':
        return evaluate(node.left, stateTable, modifiedVar) * evaluate(node.right, stateTable, modifiedVar)
        
    elif node.op == '∧':
        return evaluate(node.left, stateTable, modifiedVar) and evaluate(node.right, stateTable, modifiedVar)
        
    elif node.op == '∨':
        return evaluate(node.left, stateTable, modifiedVar) or evaluate(node.right, stateTable, modifiedVar)
        
    elif node.op == '¬':
        return not evaluate(node.value, stateTable, modifiedVar)
        
    elif node.op == '=':
        return evaluate(node.left, stateTable, modifiedVar) == evaluate(node.right, stateTable, modifiedVar)
        
    elif node.op == '<':
        return evaluate(node.left, stateTable, modifiedVar) < evaluate(node.right, stateTable, modifiedVar)
    
    elif node.op == 'Assignment':
        modifiedVar.append(node.left.value)
        if node.left.value in stateTable:
            stateTable[node.left.value] = evaluate(node.right, stateTable, modifiedVar)
        else:
            stateTable = stateTable.update({node.left.value: evaluate(node.right, stateTable, modifiedVar)})
        
    elif node.op == 'semi':
        evaluate(node.left, stateTable, modifiedVar)
        evaluate(node.right, stateTable, modifiedVar)

class Interpreter(object):
    def __init__(self, parser):
        self.parser = parser
        self.state = parser.state
        self.tree = parser.parseExpr()
        self.modifiedVar = []

    def interpret(self):
        return evaluate(self.tree, self.state, self.modifiedVar)

def main():
    while True:
        try:
            expression = raw_input("")
        except EOFError:
            break
        
        tokens = Lexer(expression)
        parser = Parser(tokens)
        interpreter = Interpreter(parser)
        interpreter.interpret()
        state = interpreter.state
        modifiedVar = set(interpreter.modifiedVar)
        result = '{'
        for var in modifiedVar:
            result += (var + " → " + str(state[var]))
            if (len(modifiedVar) > 1):
                result += ', '
        result += '}'
        
        print(result)

if __name__ == "__main__":
    main()
