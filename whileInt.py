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
        firstIndex = self.index 
        while self.current is not None and self.expression[firstIndex].isalpha() is True:
            word += self.current
            self.nextChar()

            return word
    
    def isKeyword(self):
        word = self.getWord()
        keywordList = ['if', 'then', 'else', 'while', 'do', 'skip']
        
        if word in keywordList:
            return True
        else:
            return False
    
    def isVarName(self):
        word = self.getWord()
        if self.isKeyword() is False:
            return True
        else:
            return False

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
