{-# LANGUAGE InstanceSigs #-}
module MyErrorType where
import           Control.Monad

data MyError a = Good a
               | EmptyError
               | StackedError [String]
               deriving Eq

prettyShowList :: Show a => [a] -> String
prettyShowList []     = ""
prettyShowList (x:xs) = show x ++ "\n" ++ prettyShowList xs

instance (Show a) => Show (MyError a) where
    show (StackedError ls) = prettyShowList ls
    show EmptyError        = "EmptyError"
    show (Good a)          = "Good " ++ show a

-- instance Eq a => Eq (MyError a) where
--     (==) :: Eq a => MyError a -> MyError a -> Bool
--     EmptyError == EmptyError   = True
--     Good x == Good y           = x == y
--     FullError x == FullError y = x == y
--     _ == _                     = False

instance Functor MyError where
    fmap f (Good a)          = Good (f a)
    fmap _ EmptyError        = EmptyError
    fmap _ (StackedError ss) = StackedError ss

instance Applicative MyError where
    pure :: a -> MyError a
    pure = Good
    (<*>) :: MyError (a -> b) -> MyError a -> MyError b
    (<*>) = ap


instance Monad MyError where
    (>>=) :: MyError a -> (a -> MyError b) -> MyError b
    Good x >>= f          = f x
    EmptyError >>= _      = EmptyError
    StackedError ss >>= _ = StackedError ss

instance Semigroup a => Semigroup (MyError a) where
    (<>) :: MyError a -> MyError a -> MyError a
    Good x <> Good y                       = Good (x <> y)
    EmptyError <> x                        = x
    x <> EmptyError                        = x
    se@(StackedError _) <> (Good _)        = se
    (Good _) <> se@(StackedError _)        = se
    (StackedError xs) <> (StackedError ys) = StackedError $ xs <> ys

instance Semigroup a => Monoid (MyError a) where
    mempty :: Semigroup a => MyError a
    mempty = EmptyError

instance MonadFail MyError where
    fail :: String -> MyError a
    fail _ = EmptyError

