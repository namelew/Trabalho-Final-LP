module Main where 

import Lexer
import Parser 
import TypeChecker
import Interpreter 

main = getContents >>= print . eval . typecheck . parser . lexer
--main = getContents >>= print . parser . lexer
