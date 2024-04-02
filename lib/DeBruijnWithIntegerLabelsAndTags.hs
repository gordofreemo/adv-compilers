module DeBruijnWithIntegerLabelsAndTags
       (Type(..),
        S.Const(..),
        S.PrimOp(..),
        Term(..),
        toDeBruijn,
        toDeBruijnType,
        primOpEval,
        constFromDeBruijn,
        isValue,
        shift,
        subst,
        typeExpand,
        (|->)
       ) where

import           Data.Bifunctor
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Maybe
import           Utils          as U

import qualified AbstractSyntax as S
import           Latex
import qualified Typing         as T

type Const = S.Const
type PrimOp = S.PrimOp

data Type  =  TypeArrow      Type Type
           |  TypeBool
           |  TypeInt
           |  TypeChar
           |  TypeUnit
           |  TypeRecord     [Type]
           |  TypeVariant    [Type]
           |  TypeVar        S.TypeVar
           |  TypeMu         S.TypeVar Type

instance Show Type where
  show tau = case tau of
    TypeArrow tau1 tau2   ->  "->(" ++ show tau1 ++ "," ++ show tau2 ++ ")"
    TypeBool              ->  "Bool"
    TypeInt               ->  "Int"
    TypeChar              ->  "Char"
    TypeUnit              ->  "Unit"
    TypeRecord taus       ->  "Record("
                                ++ intercalate ", " (map  (\(i,tau) -> "<" ++ show i ++ ">" ++ ": " ++ show tau)
                                                          (zip [0..] taus))
                                ++ ")"
    TypeVariant taus      ->  "Variant("
                                ++ intercalate ", " (map  (\(i,tau) -> "<" ++ show i ++ ">" ++ ": " ++ show tau)
                                                          (zip [0..] taus))
                                ++ ")"
    TypeVar xi            ->  xi
    TypeMu chi tau -> "Mu (" ++ chi ++ "." ++ show tau ++ ")"
    -- TypeMu 0 [xi] [tau]   ->  "Mu (" ++ xi ++ "." ++ show tau ++ ")"
    -- TypeMu i xis taus     ->  "Mu " ++ show i ++ " (" ++ intercalate "," xis ++ ").("
    --                             ++ intercalate "," (map show taus) ++ ")"

instance LatexShow Type where
  latexShow tau = case tau of
    TypeArrow tau1 tau2   ->  "$\\rightarrow$ (" ++ latexShow tau1 ++ ", " ++ latexShow tau2 ++ ")"
    TypeBool              ->  "Bool"
    TypeInt               ->  "Int"
    TypeChar              ->  "Char"
    TypeUnit              ->  "Unit"
    TypeRecord taus       ->  "$\\lbrace$"
                                ++ intercalate "," (map  (\(i,tau) -> "$<$" ++ show i ++ "$>$" ++ ": " ++ latexShow tau)
                                                         (zip [0..] taus))
                                ++ "$\\rbrace$"
    TypeVariant taus      ->  "$\\langle$"
                                ++ intercalate "," (map  (\(i,tau) -> "$<$" ++ show i ++ "$>$" ++ ": " ++ latexShow tau)
                                                         (zip [0..] taus))
                                ++ "$\\rangle$"
    TypeVar xi            ->  xi
    -- TypeMu 0 [xi] [tau]   ->  "$\\mu$(" ++ xi ++ "." ++ latexShow tau ++ ")"
    -- TypeMu i xis taus     ->  "$\\mu_{" ++ show i ++ "}$(" ++ intercalate "," xis ++ ").("
                                -- ++ intercalate "," (map latexShow taus) ++ ")"

instance Eq Type where
  tau1 == tau2 = typeEq [] tau1 tau2

typeEq :: [(S.TypeVar, S.TypeVar)] -> Type -> Type -> Bool
typeEq = undefined

ftv :: Type -> [S.TypeVar]
ftv = undefined

pickfresh :: String -> [String] -> String
pickfresh v avoidlist
  | v `elem` avoidlist  =  pickfresh (v++"'") avoidlist
  | otherwise           =  v

typeSubst :: [S.TypeVar] -> [Type] -> Type -> Type
typeSubst = undefined

typeExpand :: Type -> Type
typeExpand = undefined

instance Ord Type where
  a <= b = a == b || show a < show b

data Term  =
              -- lambda-calculus forms
              Var         Int
           |  Abs         Type Term
           |  App         Term Term
              -- extensions (lazy conditional; general recursion; and let-binding)
           |  If          Term Term Term
           |  Fix         Term
           |  Let         Term Term
              -- constants
           |  Const       Const
              -- primitive operator applications
           |  PrimApp     PrimOp [Term]
              -- data structures
           |  Record      [Term]
           |  Project     Term Int Int
           |  Tag         Int Term Type
           |  Case        Term Type [(Int,Term)]
           |  Fold        Type Term
           |  Unfold      Type Term

instance Show Term where
  show t = case t of
    Var n            ->  "_" ++ show n ++ "_"
    Abs tau t        ->  "abs(:" ++ show tau ++ "." ++ show t ++ ")"
    App t1 t2        ->  "app(" ++ show t1  ++ "," ++ show t2 ++ ")"
    If t1 t2 t3      ->  "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3 ++ " fi"
    Fix t1           ->  "fix(" ++ show t1 ++ ")"
    Let t1 t2        ->  "let " ++ show t1 ++ " in " ++ show t2 ++ " end"
    Const c          ->  show c
    PrimApp p ts     ->  show p ++ "(" ++ intercalate "," (map show ts) ++ ")"
    Record ts        ->  "record("
                           ++ intercalate "," (map (\(i,t) -> "<" ++ show i ++ ">" ++ "=" ++ show t) (zip [0..] ts))
                           ++ ")"
    Project t1 i k    ->  "project(" ++ show t1 ++ "." ++ "<" ++ show i ++ ">" ++ ")"
    Tag i t1 tau     ->  "tag(#" ++ show i ++ "=" ++ show t1 ++ " as " ++ show tau ++ ")"
    Case t1 tau its      ->  "case " ++ show t1 ++ " of "
                         ++ intercalate " || " (map (\(i,t) -> "<" ++ show i ++ ">" ++ "=>" ++ show t) its)
                         ++ " esac"
    Fold tau t1      ->  "fold(" ++ show tau ++ "," ++ show t1 ++ ")"
    Unfold tau t1    ->  "unfold(" ++ show tau ++ "," ++ show t1 ++ ")"

instance LatexShow Term where
  latexShow t = case t of
    Var n           ->  "\\underline{" ++ show n ++ "}"
    Abs tau t       ->  "$\\lambda$: " ++ latexShow tau
                         ++ ". " ++ latexShow t
    App t1 t2       ->  "$\\blacktriangleright$ (" ++ latexShow t1  ++ ", " ++ latexShow t2 ++ ")"
    If t1 t2 t3     ->  "if " ++ latexShow t1 ++ " then " ++ latexShow t2
                         ++ " else " ++ latexShow t3 ++ " fi"
    Fix t1          ->  "fix (" ++ latexShow t1 ++ ")"
    Let t1 t2       ->  "let " ++ latexShow t1 ++ " in " ++ latexShow t2 ++ " end"
    Const c         ->  latexShow c
    PrimApp p ts    ->  latexShow p ++ " (" ++ intercalate ", " (map latexShow ts) ++ ")"
    Record  ts      ->  "$\\lbrace$"
                          ++ intercalate ", " (map  (\(i,t) -> "$<$" ++ show i ++ "$>$" ++ " $=$ " ++ latexShow t)
                                                    (zip [0..] ts))
                          ++ "$\\rbrace$"
    Project t1 i k   ->  latexShow t1 ++ "." ++ "$<$" ++ show i ++ "$>$"
    Tag i t1 tau    ->  "$\\langle$" ++ show i ++ " $=$ " ++ latexShow t1 ++ "$\\rangle$ as " ++ latexShow tau
    Case t1 tau its     ->  "case " ++ latexShow t1 ++ " of "
                        ++ intercalate " $\\talloblong$ " (map  (\(i,t) ->  "$<$" ++ show i ++ "$>$"
                                                                            ++ "$\\Rightarrow$" ++ latexShow t)
                                                                its)
                        ++ " esac"
    Fold tau t1     ->  "fold [" ++ latexShow tau ++ "] " ++ latexShow t1
    Unfold tau t1   ->  "unfold [" ++ latexShow tau ++ "] " ++ latexShow t1

toDeBruijnType :: S.Type -> Type
toDeBruijnType tau =
  case tau of
    S.TypeArrow tau1 tau2   ->  TypeArrow (toDeBruijnType tau1) (toDeBruijnType tau2)
    S.TypeBool              ->  TypeBool
    S.TypeInt               ->  TypeInt
    S.TypeChar              ->  TypeChar
    S.TypeUnit              ->  TypeUnit
    S.TypeRecord ltaus      ->  TypeRecord (map toDeBruijnType (snd (unzip (sortBy (compare `on` fst) ltaus))))
    S.TypeVariant ltaus     ->  TypeVariant (map toDeBruijnType (snd (unzip (sortBy (compare `on` fst) ltaus))))
    S.TypeVar xi            ->  TypeVar xi
    S.TypeMu chi tau1     ->  TypeMu chi (toDeBruijnType tau1)
    S.TypeError err -> undefined

toDeBruijn :: S.Term -> Term
toDeBruijn t = f [] T.Empty t
  where
    f :: [S.Var] -> T.Context -> S.Term -> Term
    f bvs capGamma t = case t of
      -- S.Var x         ->  Var (fromJust (findIndex (==x) bvs))
      S.Var x         ->  Var (fromJust (elemIndex x bvs))
      S.Abs x tau t1   ->  Abs (toDeBruijnType tau) (f (x:bvs) (T.Bind capGamma (x, tau)) t1)
      S.App t1 t2     ->  App (f bvs capGamma t1) (f bvs capGamma t2)
      S.PrimApp op ts -> PrimApp op (map (f bvs capGamma) ts)
      S.If t1 t2 t3         -> If (f bvs capGamma t1) (f bvs capGamma t2) (f bvs capGamma t3)
      S.Fix t1        -> Fix (f bvs capGamma t1)
      S.Let x t1 t2        -> Let (f bvs capGamma t1) (f (x:bvs) (T.Bind capGamma (x, tau1)) t2)
        where tau1 = fromRight (error "type error") $ T.typing capGamma t2
      S.Const c        -> Const c
      S.Record lts         -> Record (map (f bvs capGamma . snd) lts)
      S.Project t1 l        -> Project (f bvs capGamma t1) (fromJust $ elemIndex l (map fst ltaus')) 0
        where
          ltaus' = case T.typing capGamma t1 of Right (S.TypeRecord ltaus) -> ltaus; _ -> error "type error"
      S.Tag l t1 tau1        -> Tag i (f bvs capGamma t1) (toDeBruijnType tau1)
        where i = case tau1 of S.TypeVariant lts -> fromJust $ elemIndex l (map fst lts); _ -> error "type error"
      S.Case t1 lxts         -> Case (f bvs capGamma t1) (toDeBruijnType tau1) its
        where
          (tau1, ltaus) = case T.typing capGamma t1 of (Right tau'@(S.TypeVariant ltaus')) -> (tau', ltaus'); _ -> error "type error"
          ls = map fst ltaus
          caseHelper :: (S.Label, S.Var, S.Term) -> (Int, Term)
          caseHelper (l', x', t') =
            (fromJust $ elemIndex l' ls,
            f (x':bvs) (T.Bind capGamma (x', fromJust $ lookup l' ltaus)) t')
          its = map caseHelper lxts
      S.Fold {}         -> undefined
      S.Unfold {}         -> undefined
      _ -> error $ "cannot convert to DB: " ++ show t

typeOfTagInType :: S.Label -> S.Type -> S.Type
typeOfTagInType l tau =
  case tau of
    S.TypeVariant ltaus -> snd (fromJust (find ((==l).fst) ltaus))

indexOfTagInType :: S.Label -> S.Type -> Int
indexOfTagInType l tau =
  case tau of
    S.TypeVariant ltaus -> fromJust (findIndex (==l) (sort (fst (unzip ltaus))))

indexOfRecordLabel :: S.Label -> S.Term -> T.Context -> Int
indexOfRecordLabel l t capGamma =
  case T.typing capGamma t of
    Right (S.TypeRecord ltaus) -> fromJust (findIndex (==l) (sort (fst (unzip ltaus))))

lengthOfRecord :: S.Term -> T.Context -> Int
lengthOfRecord t capGamma =
  case T.typing capGamma t of
    Right (S.TypeRecord ltaus) -> length ltaus

-- We define this to avoid code duplication in primOpEval (only the Const case is needed).
constFromDeBruijn :: Term -> S.Term
constFromDeBruijn t = case t of
  Const c  ->  S.Const c

primOpEval :: PrimOp -> [Term] -> Term
primOpEval p ts = toDeBruijn (S.primOpEval p (map constFromDeBruijn ts))


isValue :: Term -> Bool
isValue t = case t of
  Abs _ _    ->  True
  Const _    ->  True
  Record ts  ->  all isValue ts
  Tag _ t1 _ ->  isValue t1
  Fold _ t1  ->  isValue t1
  _          ->  False

-- | See TAPL pa. 79 for details
shift :: Int -> Int -> Term -> Term
shift c d t = case t of
  Var k      -> Var (if k < c then k else k + d)
  Abs tau t1 -> Abs tau (shift (c+1) d t1)
  App t1 t2  -> App (shift c d t1) (shift c d t2)
  _          -> error $ "this shift is not supported: " ++ show t

(|->) :: Int -> Term -> Term -> Term
(|->) = subst

subst :: Int -> Term -> Term -> Term
subst j s t = case t of
  Var k      -> if k == j then s else Var k
  Abs tau t1 -> Abs tau (subst (j+1) (shift 0 1 s) t1)
  App t1 t2  -> App (subst j s t1) (subst j s t2)
  _          -> undefined
  -- If y z w          -> If (subst x s y) (subst x s z ) (subst x s w)
  -- Const _           -> t
  -- PrimApp func xs   -> PrimApp func (fmap (subst x s) xs)
  -- Fix t' -> Fix (subst x s t') -- no idea if this works !!!!
  -- Project t1 label -> Project (subst x s t1) label
  -- Case t1 lvt -> Case (subst x s t1) ((\(l', v', t') -> (l', v', subst x s t')) <$> lvt)
  -- Record lt -> Record $ map (\(l', t') -> (l', subst x s t')) lt
  -- Tag l t1 tau1 -> Tag l (subst x s t1) tau1
  -- Let var t1 t2 -> Let var (subst x s t1) (if (var == x) then t2 else (subst x s t2))
  -- Unfold tau t1 -> Unfold tau (subst x s t1)
  -- Fold tau t1 -> Fold tau (subst x s t1)
  -- _ -> ErrorTerm "Not a valid term in 'subst'"
  -- _            -> error ("substitute " ++ x ++ " into " ++ show t ++ " is not implemented in subst")
