module Parser ( Parser( P ), parse, (+++) ) where
import Data.Char

{-|
PARSERS and MONADS
Little Demo Parser

Look at parsers as another example of a state transformation pattern
done with Monads.

-}

{-|
    A Parser is a Monad type that represents an attempt to match
    some kind of value to the prefix of a String. If the match
    succeeds, execution of the Monad results in a Just structure
    with a tuple containing the characters matched and the rest of
    the string. If the match fails, execution returns Nothing.

    ("newtype" must be used because we are making this type
     instantiate the Monad class.)
-}
newtype Parser a = P ( String -> Maybe ( a, String ) )

{-|
    Because we had to make a new constructor for the Parser type,
    it is now more awkward to extract the function and call it.
    Therefore we provide this short cut function.
-}
parse :: Parser a -> String -> Maybe ( a, String )
parse ( P f ) = f

instance Monad Parser where

    {-|
        Instead of modifying the Parser's input string, return simply
        places the argument value as the pseudo-result of the parse.
    -}
    return v = P (\input -> Just ( v, input ))

    {-|
        Try to execute the first parser Monad. If it succeeds, execute
        the second one as well. If it fails, the overall sequence fails.
        This implements conditional succession: "and then" behavior.
        Unlike the more sophisticated (>>=), the result of the first
        parser execution is lost.

        (>>) :: Parser a -> Parser a -> Parser a
    -}
    p >> q = P (\input -> case parse p input of
                           Just (_,output) -> parse q output
                               -- Note that v, the result
                               -- from the first parse, is lost.
                           Nothing -> Nothing)

    {-|
        Monads can do the above sequencing operation (>>), and also (>>=), a
        more complex operation that can store the value returned by the
        left-hand Monad.
        Recall that the bind operator has this type:
        (>>=) :: Parser a -> ( a -> Parser b ) -> Parser b
    -}
    parser1 >>= parser2Fn = P (\inp -> case parse parser1 inp of
                                       Nothing      -> Nothing
                                       Just (v,out) -> parse (parser2Fn v) out)
    
    -- Compare this to the simpler monoid of functions and composition:
    -- f . g = (\inp -> case g inp of out -> f out) (Only 1 case; no failure.)

{-|
    Try to execute the first parser Monad. If it succeeds, it's as
    if the second one did not exist. If it fails, the overall result
    is the result of executing the second parser Monad. This
    implements alternation: "or else" behavior.

    (A MonadPlus also comprehends the "OR" operator (+++), called mplus
    in the library.)
-}
(+++) :: Parser a -> Parser a -> Parser a


p +++ q = P (\input -> case parse p input of
                           Just (a,output) -> Just (a,output)
                               -- Note that v, the result
                               -- from the first parse, is lost.
                           Nothing -> parse q input)








-- Some Basic Parsers

-- | Fetch the next character, regardless of what it is.
getCh :: Parser Char
getCh = P (\input -> case input of
                    "" -> Nothing -- exhausted input
                    (c:str) -> Just (c,str))

-- | Unconditionally fail.
failure :: Parser anyType
failure = P (\_ -> Nothing)

{-
    Here is a silly example of Parser use, with the operators (>>) and (>>=).
    It reads and returns the first and third characters from a string.
-}

firstAndThird_A :: Parser ( Char, Char )
firstAndThird_A = do
        x <- getCh
        getCh
        y <- getCh
        return ( x, y )

-- Alternatively,

firstAndThird_B = do { x <- getCh; getCh; y <- getCh; return ( x, y ) }

-- Finally, without the syntactic sugar,

firstAndThird_C = getCh
                    >>= (\x -> getCh
                      >> getCh
                        >>= (\y -> return ( x, y )))
            

-- parse firstAndThird "abcdef" ---> [ ( ( 'a', 'c' ), "def" ) ]

-- | Ensure that the next character read satisfies a predicate.
sat :: ( Char -> Bool ) -> Parser Char
sat pred = do
            x <- getCh
            if pred x
                then return x
                else failure

-- | Ensure that the next character read is exactly the one given.
char :: Char -> Parser Char
char x = sat (x==)

-- | Ensure that the next character read is an alphabetic character.
alpha :: Parser Char
alpha = sat isAlpha

-- | Ensure that the next character read is a digit.
digit :: Parser Char
digit = sat isDigit

-- | Match an entire literal string.
string :: String -> Parser String
string "" = return ""
string ( x : xs ) = do
                        char x -- Match the String's first character.
                        string xs -- Match the rest.
                        return ( x : xs )
    -- Note the lack of "<-" assignments. One does not have to retrieve
    -- the Parser's result; it's the same as the provided argument.

data Pair = English String String | French String String

matchOne :: Parser String
matchOne = string "One"

matchTwo :: Parser String
matchTwo = string "Two"

matchUn :: Parser String
matchUn = string "Un"

matchDeux :: Parser String
matchDeux = string "Deux"

seq1_2 :: Parser String
seq1_2 = ( matchOne >> matchTwo ) +++ ( matchUn >> matchDeux )

-- To evaluate a string: parse seq1_2 str


data Answer = Answer String String | FailedParse | ExtraData String String
            deriving Show

oneTwo :: Parser Answer
oneTwo = matchOne >>=
            (\one -> matchTwo >>=
                (\two -> return $ Answer one two)
            )

unDeux :: Parser Answer
unDeux = matchUn >>=
            (\one -> matchDeux >>=
                (\two -> return $ Answer one two)
            )

seq1_2B :: Parser Answer
seq1_2B = oneTwo +++ unDeux

{-Define evalB, a function that will accept the strings "OneTwo" and "UnDeux" and
return an instance of the Answer data type.
-}

{-|
    The Kleene * combinator
    (and then .. and then .. and then ... but with saved values)
-}
many :: Parser a -> Parser [ a ]
many p = many' p +++ return [] -- Match multiple times. If no match, empty list.

-- | Auxiliary function to build a list of values parsed
many' :: Parser a -> Parser [ a ]
many' p = do
            v <- p -- Save the first match.
                   -- If fail, many' fails, which causes many to return [].
            vs <- many p -- Get the list of the rest of
                         -- the matches from what's left.
            return ( v : vs ) -- Stick the first match on the front.

-- | Match and return digits from a [,,,] - formatted list.
multiDig :: Parser String
multiDig = do
        char '['
        d <- digit
        ds <- many ( do { char ','; digit } )
        char ']'
        return ( d : ds )

{-|
    An expression parser based on the above Parser Monad.
-}

-- expr  ::=  term '+' expr  |  term
expr :: Parser Int
expr = do {
           t <- term;
           do {
               char '+';
               e <- expr;
               return ( t + e )
           }
           +++ return t
       }

-- term  ::=  factor '*' term  |  factor
term :: Parser Int
term = do {
           f <- factor;
           do {
               char '*';
               t <- term;
               return ( f * t )
           }
           +++ return f
       }

ordZero = ord '0'

-- factor  ::=  digit*  |  '(' expr ')'
factor :: Parser Int
factor = do {
             d <- digit;
             return ( ord d - ordZero )
          }
          +++
          do {
              char '(';
              e <- expr;
              char ')';
              return e
          }

eval :: String -> Int
eval str = case ( parse expr str ) of
                Just ( parseResult, "" ) -> parseResult
                Just ( _, _ ) -> error "Leftover data."
                Nothing -> error "Not a legal expression."




data Node = Leaf { val :: Num}
              | Operation{ op :: Char, left :: Node , right :: Node }




{-          
    1.  Write an alternate set of definitions so that the expressions are
    translated into syntax trees rather than being evaluated. Here is a
    session.
        *Main> b_eval "3"
        Leaf {val = 3}
        *Main> b_eval "3+2"
        Operation {op = '+', left = Leaf {val = 3}, right = Leaf {val = 2}}
        *Main> b_eval "3+2*5"
        Operation {op = '+', left = Leaf {val = 3}, right = Operation {op = '*', left = Leaf {val = 2}, right = Leaf {val = 5}}}
        *Main> b_eval "3*2+5"
        Operation {op = '+', left = Operation {op = '*', left = Leaf {val = 3}, right = Leaf {val = 2}}, right = Leaf {val = 5}}
        *Main> b_eval "3*2+5*"
        *** Exception: Leftover data.
        *Main> b_eval "3*2+"
        *** Exception: Leftover data.
        *Main> b_eval "32"
        *** Exception: Leftover data.
        *Main> b_eval "a32"
        *** Exception: Not a legal expression.

    2. Allow numbers consisting of more than one digit.
    3. Allow subtraction and division.
-}
