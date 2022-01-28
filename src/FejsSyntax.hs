module FejsSyntax (Expr)
    where


    data Expr = 
                Happy Expr Expr -- :-), if fst e, do sec e
              | Sad Expr        -- :-(, negation of e
             -- | Flirty Expr     -- ;-), do e twice
              | Asterick String -- a variable
              | Roses Expr      -- ++
              | Skull Expr      -- --
              | Nose Int        -- --..., length of nose is the val
              | Teeth           -- :-D, true
              | Angry           -- :-@, false
              | Lack            -- nothing
             -- | Expr Expr 
        deriving (Eq, Show)


{-
    data Stmt =
                Asterick String        -- assigning value
              | Op Expr
              |-}