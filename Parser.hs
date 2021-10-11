---------------------------------------------------------------------
-- Binary Expression Parser with Operator Precedence
--
-- input: string 
-- output: binary expression tree
--
-- example input: "1+2*3"
-- output:    +
--           / \
--          1   *
--             / \
--            2   3
--
-- This parser supports unsigned ints, and the following operators:
--
-- Operator     Precedence      Associativity
-- ------------------------------------------
-- * / %        5               Left to right
-- + -          4               Left to right
-- &            3               Left to right
-- ^            2               Left to right
-- |            1               Left to right
-- =            0               Right to left
---------------------------------------------------------------------

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, digitToInt)
import Test.QuickCheck  -- optional


-- binary expression tree
data Expr = BinExpr Op Expr Expr
          | IntConst Int
          deriving (Show, Eq)

-- binary operator
data Op = Op OpType Prec Fixity deriving (Show, Eq)

type Prec = Int
data Fixity = LFix | RFix deriving (Show, Eq)

data OpType = Mul
            | Div
            | Mod
            | Add
            | Sub
            | BitAnd
            | BitXor
            | BitOr
            | Assign
            deriving Eq

instance Show OpType where
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Add = "+"
    show Sub = "-"
    show BitAnd = "&"
    show BitXor = "^"
    show BitOr = "|"
    show Assign = "="


-- parser consumes some input stream, and returns some intermediate output and the 
-- remaining input stream
newtype Parser a = Parser { runParser :: String -> Either Error (a, String) }

type Error = String


instance Functor Parser where
    -- apply function to intermediate output of parser to modify its output
    fmap f p = Parser $ \s -> case runParser p s of
        Left err        -> Left err
        Right (out, s') -> Right (f out, s')


instance Applicative Parser where
    pure x = Parser $ \s -> Right (x, s)

    -- run two parsers in sequence
    -- first parser must return a function as its intermediate output; this function 
    -- will then be applied to the intermediate output of the second parser 
    pf <*> p = Parser $ \s -> case runParser pf s of
        Left err      -> Left err
        Right (f, s') -> runParser (fmap f p) s'


instance Alternative Parser where
    empty = Parser $ \s -> Left "empty"
    
    -- if first parser fails, try second parser
    p1 <|> p2 = Parser $ \s -> case runParser p1 s of 
        Left err  -> runParser p2 s
        Right out -> Right out


instance Monad Parser where
    return = pure

    -- pipe output of first parser into second parser
    -- second parser can do whatever it wants with first parser's output
    p >>= fp = Parser $ \s -> case runParser p s of
        Left err        -> Left err
        Right (out, s') -> runParser (fp out) s'

    p1 >> p2 = p1 >>= const p2


----------------------------------------
-- Basic Parsers
----------------------------------------

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \s -> case s of
    []     -> Left "empty input"
    (c:cs) -> if predicate c
        then Right (c, cs)
        else Left "unable to satisfy predicate"


char :: Char -> Parser Char
char c = satisfy (==c)


digit :: Parser Char
digit = satisfy isDigit


-- operator parsers
mulOp = Op Mul 5 LFix <$ char '*'
divOp = Op Div 5 LFix <$ char '/'
modOp = Op Mod 5 LFix <$ char '%'

addOp = Op Add 4 LFix <$ char '+'
subOp = Op Sub 4 LFix <$ char '-'

bitAndOp = Op BitAnd 3 LFix <$ char '&'
bitXorOp = Op BitXor 2 LFix <$ char '^'
bitOrOp  = Op BitOr 1 LFix  <$ char '|'
assignOp = Op Assign 0 RFix <$ char '='


-- overwrite error messages
(<?>) :: Parser a -> String -> Parser a
infix 1 <?>
p <?> err = Parser $ \s -> case runParser p s of
    Left _    -> Left err
    Right out -> Right out


op :: Parser Op
op =  mulOp
  <|> divOp
  <|> modOp
  <|> addOp
  <|> subOp
  <|> bitAndOp
  <|> bitXorOp
  <|> bitOrOp
  <|> assignOp
  <?> "invalid operator"


int :: Parser Int
int = read <$> some digit <?> "invalid int"


option :: a -> Parser a -> Parser a
option x p = p <|> return x


-- if the parser succeeds, return its output but don't consume the string
lookahead :: Parser a -> Parser a
lookahead p = Parser $ \s -> case runParser p s of
    Left err       -> Left err
    Right (out, _) -> Right (out, s)


----------------------------------------
-- Parse Expressions
----------------------------------------

intConst :: Parser Expr
intConst = IntConst <$> int


expr :: Parser Expr
expr = intConst >>= opExpr (-1)


opExpr :: Prec -> Expr -> Parser Expr
opExpr minPrec left = option left $ do
    opType@(Op _ prec fix) <- lookahead op
    if prec < minPrec || prec == minPrec && fix == LFix
        then return left
        else do
            right <- op >> intConst >>= opExpr prec
            opExpr minPrec (BinExpr opType left right)


----------------------------------------
-- Full Parser
----------------------------------------

parseExpr :: String -> Expr
parseExpr s = case runParser expr s of 
    Left err        -> error err
    Right (out, "") -> out
    Right (_, s)    -> error $ "unable to parse remaining string \"" ++ s ++ "\""


-- pretty-print expression tree
printExpr :: Expr -> String
printExpr expr = go expr ""
    where go (IntConst n) _ = show n
          go (BinExpr (Op t _ _) left right) indent = 
              show t ++ "\n" ++
              indent ++ "├ " ++ go left (indent ++ "│ ") ++ "\n" ++
              indent ++ "└ " ++ go right (indent ++ "  ") 


----------------------------------------
-- Testing
----------------------------------------

genOp :: Gen Char
genOp = elements "*/%+-&^|="


genInt :: Gen String
genInt = show <$> choose (0, 1000 :: Int)


genOpInt :: Gen String
genOpInt = (:) <$> genOp <*> genInt


-- (op int)*
genOpInts :: Gen String
genOpInts = sized $ \n ->
  frequency
    [ (1, return [])
    , (n, (++) <$> genOpInt <*> genOpInts)
    ]


-- int (op int)*
genExpr :: Gen String
genExpr = (++) <$> genInt <*> genOpInts


-- test whether all paths from the root to each leaf have increasing precedence
prop_precOrder :: Property
prop_precOrder = forAll genExpr $ \s -> isIncr (-1) $ parseExpr s
    where isIncr _ (IntConst _) = True
          isIncr minPrec (BinExpr (Op _ prec _) left right) = 
            if prec >= minPrec
                then isIncr prec left && isIncr prec right
                else False


-- test if inorder traversal of binary tree outputs the same input string
prop_inOrder :: Property
prop_inOrder = forAll genExpr $ \s -> s == (toStr $ parseExpr s)
    where toStr (IntConst n) = show n
          toStr (BinExpr (Op t _ _) left right) = toStr left ++ show t ++ toStr right

