module AbstractSyntax where

import Data.Maybe
import Data.List
import Latex
import qualified IntegerArithmetic as I

type Label = String

type TypeVar  =  String

data Type  =  TypeArrow      Type Type
           |  TypeBool
           |  TypeInt
           |  TypeChar
           |  TypeUnit
           |  TypeRecord     [(Label, Type)]
           |  TypeVariant    [(Label, Type)]

instance Eq Type where
  tau1 == tau2 = typeEq [] tau1 tau2

typeEq :: [(TypeVar, TypeVar)] -> Type -> Type -> Bool
typeEq env tau tau' = case (tau, tau') of
  (TypeArrow tau1 tau2, TypeArrow tau1' tau2')   ->  typeEq env tau1 tau1' && typeEq env tau2 tau2'
  (TypeBool, TypeBool)                           ->  True
  (TypeInt, TypeInt)                             ->  True
  (TypeChar, TypeChar)                           ->  True
  (TypeUnit, TypeUnit)                           ->  True
  (TypeRecord ltaus, TypeRecord ltaus')          ->  ...
  (TypeVariant ltaus, TypeVariant ltaus')        ->  ...
  _                                              ->  False
        
instance Show Type where
  show tau = case tau of
    TypeArrow tau1 tau2   ->  "->(" ++ show tau1 ++ "," ++ show tau2 ++ ")"
    TypeBool              ->  "Bool"
    TypeInt               ->  "Int"
    TypeChar              ->  "Char"
    TypeUnit              ->  "Unit"
    TypeRecord ltaus      ->  "Record(" ++ intercalate ", " (map (\(l,tau) -> l ++ ": " ++ show tau) ltaus) ++ ")"
    TypeVariant ltaus     ->  "Variant(" ++ intercalate ", " (map (\(l,tau) -> l ++ ": " ++ show tau) ltaus) ++ ")"

instance LatexShow Type where
  latexShow tau = case tau of
    TypeArrow tau1 tau2   ->  "$\\rightarrow$ (" ++ latexShow tau1 ++ ", " ++ latexShow tau2 ++ ")"
    TypeBool              ->  "Bool"
    TypeInt               ->  "Int"
    TypeChar              ->  "Char"
    TypeUnit              ->  "Unit"
    TypeRecord ltaus      ->  "$\\lbrace$" ++ intercalate "," (map (\(l,tau) -> l ++ ": " ++ latexShow tau) ltaus) ++ "$\\rbrace$"
    TypeVariant ltaus     ->  "$\\langle$" ++ intercalate "," (map (\(l,tau) -> l ++ ": " ++ latexShow tau) ltaus) ++ "$\\rangle$"


type Var = String

data Term  =
              -- lambda-calculus forms
              Var         Var
           |  Abs         Var Type Term
           |  App         Term Term
              -- extensions (lazy conditional; general recursion; and let-binding)
           |  If          Term Term Term
           |  Fix         Term
           |  Let         Var Term Term
              -- constants
           |  Const       Const
              -- primitive operator applications
           |  PrimApp     PrimOp [Term]
              -- data structures
           |  Record      [(Label, Term)]
           |  Project     Term Label
           |  Tag         Label Term Type
           |  Case        Term [(Label, Var, Term)]
           deriving Eq

data Const = Tru | Fls | IntConst I.IntegerType | CharConst Char | Unit
             deriving Eq

instance Show Const where
  show c = case c of
    Tru              ->  "true"
    Fls              ->  "false"
    IntConst i       ->  show i
    CharConst c      ->  show c
    Unit             ->  "unit"

instance LatexShow Const where
  latexShow c = show c

constType :: Const -> Type
constType c = case c of
  Tru          ->  TypeBool
  Fls          ->  TypeBool
  IntConst _   ->  TypeInt
  CharConst _  ->  TypeChar
  Unit         ->  TypeUnit

data PrimOp  =  IntAdd | IntSub | IntMul | IntDiv | IntNand | IntEq | IntLt | CharOrd | CharChr
                deriving Eq

instance Show PrimOp where
  show p = case p of
    IntAdd   ->  "+"
    IntSub   ->  "-"
    IntMul   ->  "*"
    IntDiv   ->  "/"
    IntNand  ->  "^"
    IntEq    ->  "="
    IntLt    ->  "<"
    CharOrd  ->  "ord"
    CharChr  ->  "chr"

instance LatexShow PrimOp where
  latexShow p = case p of
    IntAdd   ->  "$+$"
    IntSub   ->  "$-$"
    IntMul   ->  "$\\times$"
    IntDiv   ->  "$/$"
    IntNand  ->  "$\\uparrow$"
    IntEq    ->  "$=$"
    IntLt    ->  "$<$"
    CharOrd  ->  "ord"
    CharChr  ->  "chr"

type PrimOpType = ([Type], Type)

arithmeticBinaryPrimOpType :: PrimOpType
arithmeticBinaryPrimOpType = ([TypeInt, TypeInt], TypeInt)

relationalBinaryPrimOpType :: PrimOpType
relationalBinaryPrimOpType = ([TypeInt, TypeInt], TypeBool)

primOpArity :: PrimOp -> Int
primOpArity p = case p of
  IntAdd   ->  2
  IntSub   ->  2
  IntMul   ->  2
  IntDiv   ->  2
  IntNand  ->  2
  IntEq    ->  2
  IntLt    ->  2
  CharOrd  ->  1
  CharChr  ->  1

primOpType :: PrimOp -> PrimOpType
primOpType p = case p of
  IntAdd   ->  arithmeticBinaryPrimOpType
  IntSub   ->  arithmeticBinaryPrimOpType
  IntMul   ->  arithmeticBinaryPrimOpType
  IntDiv   ->  arithmeticBinaryPrimOpType
  IntNand  ->  arithmeticBinaryPrimOpType
  IntEq    ->  relationalBinaryPrimOpType
  IntLt    ->  relationalBinaryPrimOpType
  CharOrd  ->  ([TypeChar], TypeInt)
  CharChr  ->  ([TypeInt], TypeChar)

primOpEval :: PrimOp -> [Term] -> Term
primOpEval IntAdd [Const (IntConst i1), Const (IntConst i2)] = Const (IntConst (I.intAdd i1 i2))
primOpEval IntSub [Const (IntConst i1), Const (IntConst i2)] = Const (IntConst (I.intSub i1 i2))
primOpEval IntMul [Const (IntConst i1), Const (IntConst i2)] = Const (IntConst (I.intMul i1 i2))
primOpEval IntDiv [Const (IntConst i1), Const (IntConst i2)] = Const (IntConst (I.intDiv i1 i2))
primOpEval IntNand [Const (IntConst i1), Const (IntConst i2)] = Const (IntConst (I.intNand i1 i2))
primOpEval IntEq [Const (IntConst i1), Const (IntConst i2)] = Const (if I.intEq i1 i2 then Tru else Fls)
primOpEval IntLt [Const (IntConst i1), Const (IntConst i2)] = Const (if I.intLt i1 i2 then Tru else Fls)
primOpEval CharOrd [Const (CharConst c)] = Const (IntConst (I.intOrd c))
primOpEval CharChr [Const (IntConst i)] = Const (CharConst (I.intChr i))

instance Show Term where
  show t = case t of
    Var x            ->  x
    Abs x tau t      ->  "abs(" ++ x ++ ": " ++ show tau ++ ". " ++ show t ++ ")"
    App t1 t2        ->  "app(" ++ show t1  ++ ", " ++ show t2 ++ ")"
    If t1 t2 t3      ->  "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3 ++ " fi"
    Fix t            ->  "fix(" ++ show t ++ ")"
    Let x t1 t2      ->  "let " ++ x ++ " = " ++ show t1 ++ " in " ++ show t2 ++ " end"
    Const c          ->  show c
    PrimApp p ts     ->  show p ++ "(" ++ intercalate ", " (map show ts) ++ ")"
    Record lts       ->  "record(" ++ intercalate ", " (map (\(l,t) -> l ++ " = " ++ show t) lts) ++ ")"
    Project t l      ->  "project(" ++ show t ++ "." ++ l ++ ")"
    Tag l t tau      ->  "tag(" ++ l ++ " = " ++ show t ++ " as " ++ show tau ++ ")"
    Case t lxts      ->  "case " ++ show t ++ " of "
                         ++ intercalate " | " (map (\(l,x,t) -> l ++ " = " ++ x ++ " => " ++ show t) lxts) ++ " esac"

showElidingTypes :: Term -> String
showElidingTypes t = case t of
    Var x            ->  x
    Abs x tau t      ->  "abs(" ++ x ++ ":. " ++ showElidingTypes t ++ ")"
    App t1 t2        ->  "app(" ++ showElidingTypes t1  ++ ", " ++ showElidingTypes t2 ++ ")"
    If t1 t2 t3      ->  "if " ++ showElidingTypes t1 ++ " then " ++ showElidingTypes t2 ++ " else " ++ showElidingTypes t3 ++ " fi"
    Fix t            ->  "fix(" ++ showElidingTypes t ++ ")"
    Let x t1 t2      ->  "let " ++ x ++ " = " ++ showElidingTypes t1 ++ " in " ++ showElidingTypes t2 ++ " end"
    Const c          ->  show c
    PrimApp p ts     ->  show p ++ "(" ++ intercalate ", " (map showElidingTypes ts) ++ ")"
    Record lts       ->  "record(" ++ intercalate ", " (map (\(l,t) -> l ++ " = " ++ showElidingTypes t) lts) ++ ")"
    Project t l      ->  "project(" ++ showElidingTypes t ++ "." ++ l ++ ")"
    Tag l t tau      ->  "tag(" ++ l ++ " = " ++ showElidingTypes t ++ " as)"
    Case t lxts      ->  "case " ++ showElidingTypes t ++ " of "
                         ++ intercalate " | " (map (\(l,x,t) -> l ++ " = " ++ x ++ " => " ++ showElidingTypes t) lxts) ++ " esac"

instance LatexShow Term where
  latexShow t = case t of
    Var x           ->  x
    Abs x tau t     ->  "$\\lambda$" ++ x ++ ": " ++ latexShow tau
                         ++ ". " ++ latexShow t
    App t1 t2       ->  "$\\blacktriangleright$ (" ++ latexShow t1  ++ ", " ++ latexShow t2 ++ ")"
    If t1 t2 t3     ->  "if " ++ latexShow t1 ++ " then " ++ latexShow t2
                         ++ " else " ++ latexShow t3 ++ " fi"
    Fix t           ->  "fix (" ++ latexShow t ++ ")"
    Let x t1 t2     ->  "let " ++ x ++ " = " ++ latexShow t1 ++ " in " ++ latexShow t2 ++ " end"
    Const c         ->  latexShow c
    PrimApp p ts    ->  latexShow p ++ " (" ++ intercalate ", " (map latexShow ts) ++ ")"
    Record lts      ->  "$\\lbrace$" ++ intercalate ", " (map (\(l,t) -> l ++ " $=$ " ++ latexShow t) lts) ++ "$\\rbrace$"
    Project t l     ->  latexShow t ++ "." ++ l
    Tag l t tau     ->  "$\\langle$" ++ l ++ " $=$ " ++ latexShow t ++ "$\\rangle$ as " ++ latexShow tau
    Case t lxts     ->  "case " ++ latexShow t ++ " of "
                        ++ intercalate " $\\talloblong$ " (map (\(l,x,t) -> l ++ "$=$" ++ x ++ "$\\Rightarrow$" ++ latexShow t) lxts)
                        ++ " esac"

fv :: Term -> [Var]
fv t = case t of
  Var x         -> [x]
  Abs x _ t     -> filter (/= x) (fv t)
  App x y       -> [x,y] >>= fv
  If x y z      -> [x,y,z] >>= fv
  Const _       -> []
  PrimApp _ xs  -> xs >>= fv

subst :: Var -> Term -> Term -> Term
subst x s t = case t of
  Var y             -> if (y == x) then s else (Var y)
  Abs y tau bod     -> if ((x /= y) && (not (y `elem` (fv s)))) then (Abs y tau (subst x s bod)) else t
  App y z           -> App (subst x s y) (subst x s z)
  If y z w          -> If (subst x s y) (subst x s z ) (subst x s w)
  Const _           -> t
  PrimApp func xs   -> PrimApp func (fmap (subst x s) xs)


isValue :: Term -> Bool
isValue t = case t of
  Abs _ _ _      ->  True
  Const _        ->  True
  Record lts     ->  all isValue (snd (unzip lts))
  Tag _ t _      ->  isValue t
  _              ->  False
