data Node = Leaf { val :: Integer}
              | Operation{ op :: Char, left :: Node , right :: Node }


exprb :: Parser Node
expr = do {
           t <- term;
           do {
               char '+';
               e <- expr;
               return ( Operation '+' t e )
           }
           +++ return ( Leaf t)
       }

       term :: Parser Int
       term = do {
                  f <- factor;
                  do {
                      t <- term;
                     return ( Operation '*' t e )
                  }
                  +++ return (Leaf f)
              }

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
                        return ( Operation e '(' ')')
                    }

