module AbstractSyntax where

import           Data.List
import           Data.Maybe
import qualified IntegerArithmetic as I
import           Latex


type Label = String

type TypeVar  =  String

data Type  =  TypeArrow      Type Type
           |  TypeBool
           |  TypeInt
           |  TypeError      String

instance Eq Type where
  tau1 == tau2 = typeEq [] tau1 tau2

typeEq :: [(TypeVar, TypeVar)] -> Type -> Type -> Bool
typeEq env tau tau' = case (tau, tau') of
  (TypeArrow tau1 tau2, TypeArrow tau1' tau2')   ->  typeEq env tau1 tau1' && typeEq env tau2 tau2'
  (TypeBool, TypeBool)                           ->  True
  (TypeInt, TypeInt)                             ->  True
  _ -> False

instance Show Type where
  show tau = case tau of
    TypeArrow tau1 tau2 ->  "->(" ++ show tau1 ++ "," ++ show tau2 ++ ")"
    TypeBool            ->  "Bool"
    TypeInt             ->  "Int"
    TypeError errMsg    ->  "Type Error: " ++ errMsg

type Var = String

data Term  =
              -- lambda-calculus forms
              Var         Var
           |  Abs         Var Type Term
           |  App         Term Term
              -- extensions (lazy conditional; general recursion; and let-binding)
           |  If          Term Term Term
              -- constants
           |  Const       Const
              -- primitive operator applications
           |  PrimApp     PrimOp [Term]
           deriving Eq

data Const = Tru | Fls | IntConst I.IntegerType
             deriving Eq

instance Show Const where
  show c = case c of
    Tru        ->  "true"
    Fls        ->  "false"
    IntConst i ->  show i

instance LatexShow Const where
  latexShow c = show c

constType :: Const -> Type
constType c = case c of
  Tru        ->  TypeBool
  Fls        ->  TypeBool
  IntConst _ ->  TypeInt

data PrimOp  =  IntAdd | IntSub | IntMul | IntDiv | IntNand | IntEq | IntLt
                deriving Eq

instance Show PrimOp where
  show p = case p of
    IntAdd  ->  "+"
    IntSub  ->  "-"
    IntMul  ->  "*"
    IntDiv  ->  "/"
    IntNand ->  "^"
    IntEq   ->  "="
    IntLt   ->  "<"

instance LatexShow PrimOp where
  latexShow p = case p of
    IntAdd  ->  "$+$"
    IntSub  ->  "$-$"
    IntMul  ->  "$\\times$"
    IntDiv  ->  "$/$"
    IntNand ->  "$\\uparrow$"
    IntEq   ->  "$=$"
    IntLt   ->  "$<$"

type PrimOpType = ([Type], Type)

arithmeticBinaryPrimOpType :: PrimOpType
arithmeticBinaryPrimOpType = ([TypeInt, TypeInt], TypeInt)

relationalBinaryPrimOpType :: PrimOpType
relationalBinaryPrimOpType = ([TypeInt, TypeInt], TypeBool)

primOpArity :: PrimOp -> Int
primOpArity p = case p of
  IntAdd  ->  2
  IntSub  ->  2
  IntMul  ->  2
  IntDiv  ->  2
  IntNand ->  2
  IntEq   ->  2
  IntLt   ->  2

primOpType :: PrimOp -> PrimOpType
primOpType p = case p of
  IntAdd  ->  arithmeticBinaryPrimOpType
  IntSub  ->  arithmeticBinaryPrimOpType
  IntMul  ->  arithmeticBinaryPrimOpType
  IntDiv  ->  arithmeticBinaryPrimOpType
  IntNand ->  arithmeticBinaryPrimOpType
  IntEq   ->  relationalBinaryPrimOpType
  IntLt   ->  relationalBinaryPrimOpType

primOpEval :: PrimOp -> [Term] -> Term
primOpEval IntAdd [Const (IntConst i1), Const (IntConst i2)] = Const (IntConst (I.intAdd i1 i2))
primOpEval IntSub [Const (IntConst i1), Const (IntConst i2)] = Const (IntConst (I.intSub i1 i2))
primOpEval IntMul [Const (IntConst i1), Const (IntConst i2)] = Const (IntConst (I.intMul i1 i2))
primOpEval IntDiv [Const (IntConst i1), Const (IntConst i2)] = Const (IntConst (I.intDiv i1 i2))
primOpEval IntNand [Const (IntConst i1), Const (IntConst i2)] = Const (IntConst (I.intNand i1 i2))
primOpEval IntEq [Const (IntConst i1), Const (IntConst i2)] = Const (if I.intEq i1 i2 then Tru else Fls)
primOpEval IntLt [Const (IntConst i1), Const (IntConst i2)] = Const (if I.intLt i1 i2 then Tru else Fls)

instance Show Term where
  show t = case t of
    Var x            ->  x
    Abs x tau t      ->  "abs(" ++ x ++ ": " ++ show tau ++ ". " ++ show t ++ ")"
    App t1 t2        ->  "app(" ++ show t1  ++ ", " ++ show t2 ++ ")"
    If t1 t2 t3      ->  "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3 ++ " fi"
    Const c          ->  show c
    PrimApp p ts     ->  show p ++ "(" ++ intercalate ", " (map show ts) ++ ")"

fv :: Term -> [Var]
fv t = case t of
  Var x        ->  [x]
  Abs x _ t    -> (fv t) \\ [x]
  App x y      -> [x,y] >>= fv
  If x y z     -> [x,y,z] >>= fv
  Const x      -> []
  PrimApp _ xs -> xs >>= fv

subst :: Var -> Term -> Term -> Term
subst x s t = case t of
  Var y -> if (y == x) then s else (Var y)
  Abs y tau bod -> if ((x /= y) && (not (y `elem` (fv s)))) then (Abs y tau (subst x s bod)) else t
  App y z -> App (subst x s y) (subst x s z)
  If y z w -> If (subst x s y) (subst x s z ) (subst x s w)
  Const _ -> t
  PrimApp func xs -> PrimApp func (fmap (subst x s) xs)


isValue :: Term -> Bool
isValue (Const _)   = True
isValue (Abs _ _ _) = True
isValue _           = False

