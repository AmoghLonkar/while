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
                self.nextChar()
                return Token('Relational', self.current)
            
            if self.current in ['∧', '∨', '¬']:
                self.nextChar()
                return Token('Logical', self.current)
            
            if self.current == ';':
                self.nextChar()
                return Token('Semi', ';')

            if self.current in [ '(', ')']:
                self.nextChar()
                return Token('Parens', self.current)
            
            if self.current in [ '{', '}']:
                self.nextChar()
                return Token('Braces', self.current)
            
            #Removing white spaces
            if self.current.isspace():
                self.nextChar()
                continue

        return Token('EOF', None)

def main():
    while True:
        try:
            expression = input("")
        except EOFError:
            break
        tokens = Lexer(expression)
        #parser = Parser(tokens)
        #interpreter = Interpreter(parser)
        #result = interpreter.interpret()
        #print(str(result))

if __name__ == "__main__":
    main()
