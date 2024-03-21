
data Value t = Var String
           | Const 
           | Abs t t


class Term a where
    (|->) :: Value -> a -> a