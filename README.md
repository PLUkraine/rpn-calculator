RPN Calculator
==============

Features
--------
* Console calculator
* Works with +, -, \*, /, ^
   * Works with unary +, - as well
* Works with functions *max, sin, cos*. Functions are called like this: f(a,b)
* Recognizes constants like *euler number (e)* and *pi*
* Arbitrary number of spaces can be added beween elements
* Prompts errors along with their potential cause
* You can put expressions as command line arguments and get their evaluasions immediately

Examples
--------
1+2\*3^4  
sin(pi/2) \* cos(0)  
max(2.2, max(4^2, 3^3))  
13^(3^(1/2)+(-2))\*3/2  

Build
-----
I recommend installing [Haskell Platform](https://www.haskell.org/platform/) to build this project. It will install all required instruments (GHC, Cabal).
Then download, configure and build project by typing in console

```bash
git clone https://github.com/PLUkraine/rpn-calculator.git
cd rpn-calculator
cabal install
rpnCalculator
```

What is under the hood
----------------------
Project consists of 3 big parts: *lexer*, *parser* and *evaluator*. Lexer tokenizes input, parser converts it into [reverse polish notation(RPN)](https://en.wikipedia.org/wiki/Reverse_Polish_notation) using [shunting-yard algorithm](https://en.wikipedia.org/wiki/Reverse_Polish_notation), then evaluator evaluates expression in RPN.
