module Eval where
    import Syntax


    getAst str = Teeth

    eval :: Expr -> Expr
    eval (Happy Teeth e)         = eval e
    eval (Happy Angry _)         = Lack
    eval (AstAct str)            = eval $ getAst str
    eval (Sad Teeth)             = Angry
    eval (Sad Angry)             = Teeth
    eval (Nose i)                = Nose i
    --eval (Roses e)               = inc $ eval e
    --eval (Skull e)               = dec $ eval e
    eval Teeth                   = Teeth
    eval Angry                   = Angry
    eval _                       = Lack