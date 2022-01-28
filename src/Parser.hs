module Parser
    where
    import Text.Parsec (ParseError, parse)
    import Text.Parsec.String (Parser)
    import Text.Parsec.Char (oneOf, char, digit, satisfy, letter)
    import Text.Parsec.Combinator (many1, choice, chainl1)
    import qualified Text.Parsec.Token as Tok
    import qualified Text.Parsec.Expr as Ex

    import Control.Applicative ((<|>), many)
    import Control.Monad (void)
    import Data.Char (isLetter, isDigit)

    import Syntax
   
    -- using Parsec

    -- <|> combines two optional paths of parser logic

    -- tokens: characters immediately converted to tokens, reserved strings
    reservedNames = [":-)",
                     ":-(",
                     ";-)", 
                     ":-D",
                     ":-@",
                     "(("
                     ]

    reservedOpNames = []



    langDef = Tok.LanguageDef
        { Tok.commentStart    = "",
          Tok.commentEnd      = "",
          Tok.commentLine     = "...",
          Tok.nestedComments  = False,
          Tok.identStart      = oneOf "*",
          Tok.identLetter     = letter <|> (oneOf "*"), -- TODO how only special chars?
          Tok.opStart         = oneOf ":",                 -- all operations are
          Tok.opLetter        = digit <|> (oneOf ":-()*"),
          Tok.reservedNames   = reservedNames,
          Tok.reservedOpNames = reservedOpNames,
          Tok.caseSensitive   = True
          }

    prefixOp s f = Ex.Prefix (reservedOp s >> return f)

    opTable = [
              [ prefixOp "(l)" Roses ]
            , [ prefixOp "x-3" Skull ]
            ]

    -- lexer functions delegating to parsec lib
    lexer :: Tok.TokenParser ()
    lexer = Tok.makeTokenParser langDef

    -- parse every expression in parantheses
    parens :: Parser a -> Parser a
    parens = Tok.parens lexer

    reserved :: String -> Parser ()
    reserved = Tok.reserved lexer

    semiSep :: Parser a -> Parser [a]
    semiSep = Tok.semiSep lexer

    reservedOp :: String -> Parser ()
    reservedOp = Tok.reservedOp lexer

    



    -- nr parser
    num :: Parser Expr       -- parses length of smilie nose as integers
    num = do
        n <- many1 (oneOf "-") 
        return (Nose  (length n))



    -- parses variables
    var :: Parser Expr
    var = do
        fc <- satisfy (\a -> a == '*')
        rest <- many $ satisfy (\a -> isLetter a || a == '*')
        return $ AstAct (fc:rest)



    happy :: Parser Expr
    happy = do
        reserved ":-)"
        act <- expr -- evaluate condition statement
        reservedOp "(("
        com <- expr --many1 $ satisfy (\a -> a /= '_')
        return (Happy act com)
        

    angry :: Parser Expr
    angry = do
        reserved ":-@"
        return Angry

    angry :: Parser Expr
    teeth = do
        reserved ":-D"
        return Teeth

    --TODO parse comments


    parsers = [num, angry, teeth, happy]

    --TODO parse with all except calling
    allParse = choice parsers
    expr = Ex.buildExpressionParser opTable allParse



    -- top level entry to parser
   {- expr :: Parser Expr
    expr = Ex.buildExpressionParser table factor-}