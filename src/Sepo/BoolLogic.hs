module Sepo.BoolLogic where

import qualified Data.Set as S
import Data.List (intercalate)
import Data.Foldable
import Prelude hiding (subtract)

{- This module is used for proving that the formulas for intersect and subtract in Sepo.Execution are correct
 - -}

data Var = Var {
	varName :: String,
	varPos :: Bool,
	varSet :: Bool
} deriving (Eq, Ord)
instance Show Var where
	show (Var name pos set) = name <> (if pos then "p" else "n") <> (if set then "s" else "p")

data Form
	= FV Var
	| Or Form Form
	| And Form Form
	| Not Form
	| Bool Bool
	deriving (Eq, Ord, Show)

type DNF = S.Set (S.Set (Bool, Var))

collect :: Form -> DNF

collect (Bool False) = S.empty
collect (Bool True) = S.singleton S.empty
collect (Not (Bool False)) = collect true
collect (Not (Bool True)) = collect false

collect (FV var) = S.singleton $ S.singleton (True, var)
collect (Not (FV var)) = S.singleton $ S.singleton (False, var)
collect (Not (Or a b)) = collect $ And (Not a) (Not b)
collect (Not (And a b)) = collect $ Or (Not a) (Not b)
collect (Or a b) = collect a <> collect b
collect (And a b) = S.fromList $ do
	a <- S.toList $ collect a
	b <- S.toList $ collect b
	pure $ a <> b
collect (Not (Not f)) = collect f

build :: DNF -> Form
build = go1
	where
		go1 dnf | S.null dnf = false
		go1 dnf = foldl1 Or $ S.map go2 dnf

		go2 con | S.null con = true
		go2 con = foldl1 And $ S.map go3 con

		go3 (True, var) = FV var
		go3 (False, var) = Not $ FV var

infixr 3 .&
(.&) :: Form -> Form -> Form
(.&) = And

infixr 2 .|
(.|) :: Form -> Form -> Form
(.|) = Or

true :: Form
true = Bool True

false :: Form
false = Bool False

data Filter = Filter {
	filterPosPred :: Form,
	filterPosSet :: Form,
	filterNegPred :: Form,
	filterNegSet :: Form
} deriving (Show)
filterEval :: Filter -> Form
filterEval (Filter {..}) = filterPosPred .| filterPosSet .| Not (filterNegPred .| filterNegSet)

filterVar :: String -> Filter
filterVar name = Filter {
		filterPosPred = FV $ Var name True False,
		filterPosSet  = FV $ Var name True True,
		filterNegPred = FV $ Var name False False,
		filterNegSet  = FV $ Var name False True
	}

a :: Filter
a = filterVar "a"
b :: Filter
b = filterVar "b"

intersect :: Filter -> Filter -> Filter
intersect a b = Filter {
		filterPosPred = filterPosPred a .& filterPosPred b,
		filterPosSet = foldl (.|) false [
				filterPosPred a .& filterPosSet b,
				filterPosSet a .& filterPosPred b,
				filterPosSet a .& filterPosSet b,
				filterPosSet a .& Not (filterNegPred b) .& Not (filterNegSet b),
				Not (filterNegPred a) .& Not (filterNegSet a) .& filterPosSet b
			],
		filterNegPred = foldl (.|) false [
				Not (filterPosPred a) .& filterNegPred a,
				Not (filterPosPred b) .& filterNegPred b,
				filterNegPred a .& filterNegPred b
			],
		filterNegSet = foldl (.|) false [
				Not (filterPosPred a) .& filterNegSet a,
				Not (filterPosPred b) .& filterNegSet b,
				filterNegPred a .& filterNegSet b,
				filterNegSet a .& filterNegPred b,
				filterNegSet a .& filterNegSet b
			]
	}

i = intersect a b

subtract :: Filter -> Filter -> Filter
subtract a b = Filter {
		filterPosPred = false,
		filterPosSet = foldl (.|) false [
				Not (filterNegPred a) .& Not (filterNegSet a) .& Not (filterPosPred b) .& Not (filterPosSet b) .& filterNegSet b,
				filterPosPred a .& Not (filterPosPred b) .& Not (filterPosSet b) .& filterNegSet b,
				filterPosSet a .& Not (filterPosPred b) .& Not (filterPosSet b) .& filterNegPred b,
				filterPosSet a .& Not (filterPosPred b) .& Not (filterPosSet b) .& filterNegSet b
			],
		filterNegPred = foldl (.|) false [
				Not (filterPosPred a) .& filterNegPred a,
				Not (filterNegPred b),
				filterPosPred b
			],
		filterNegSet = (Not (filterPosPred a) .& filterNegSet a) .| filterPosSet b
	}

s = subtract a b

pp = putStrLn . unlines . fmap (intercalate " ∧ ") . fmap (fmap $ \(p, v) -> (if p then "" else "¬") <> show (v :: Var)) . fmap S.toList . S.toList

minimize v = S.filter (not . flip any v . flip S.isProperSubsetOf) v

separate :: DNF -> Filter
separate dnf = Filter {
		filterPosPred = build pp,
		filterPosSet = build ps,
		filterNegPred = build np,
		filterNegSet = build ns
	}
	where
		(pp, w1) = S.partition (all $ not . varSet . snd) dnf
		(ps, neg) = S.partition (any $ (&&) <$> fst <*> varSet . snd) w1
		neg' = minimize $ collect $ Not $ build neg
		(np, ns) = S.partition (all $ not . varSet . snd) neg'
