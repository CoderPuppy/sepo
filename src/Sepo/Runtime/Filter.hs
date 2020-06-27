module Sepo.Runtime.Filter where

import Control.Applicative (liftA2)
import Sepo.Runtime.Values
import qualified Data.Map as M
import qualified Data.Text as T

data Filter = Filter {
	posPred :: Track -> Bool,
	posSet :: MultiSet Track,
	negPred :: Track -> Bool,
	negSet :: MultiSet Track
}
instance Semigroup Filter where
	a <> b = Filter {
			posPred = (||) <$> posPred a <*> posPred b,
			posSet = M.union (posSet a) (posSet b),
			negPred = (&&) <$> negPred a <*> negPred b,
			negSet = M.union (M.intersection (negSet a) (negSet b))
				(M.union
					(M.filterWithKey (flip $ const $ negPred a) (negSet b))
					(M.filterWithKey (flip $ const $ negPred b) (negSet a)))
		}
instance Monoid Filter where
	mempty = Filter (const False) M.empty (const True) M.empty

apply :: Filter -> Track -> Bool
apply (Filter {..}) track = posPred track || M.member track posSet || not (negPred track || M.member track negSet)

applyIntersect :: Filter -> M.Map Track a -> M.Map Track a
applyIntersect (Filter {..}) m = M.union
	(M.union
		(M.filterWithKey (flip $ const posPred) m)
		(M.intersection m posSet))
	(M.filterWithKey (flip $ const $ not . negPred) $
		M.difference m negSet)

applySubtract :: Filter -> M.Map Track a -> M.Map Track a
applySubtract (Filter {..}) m = M.union
	(M.filterWithKey (flip $ const $ not . posPred) $
		M.intersection m' negSet)
	(M.filterWithKey (flip $ const $ (&&) <$> negPred <*> not . posPred) m')
	where m' = M.difference m posSet

union :: Filter -> Filter -> Filter
union = (<>)

intersect :: Filter -> Filter -> Filter
intersect a b = Filter {
		posPred = (&&) <$> posPred a <*> posPred b,
		posSet = foldl M.union M.empty [
				M.filterWithKey (flip $ const $ posPred a) (posSet b),
				M.filterWithKey (flip $ const $ posPred b) (posSet a),
				M.intersection (posSet a) (posSet b),
				M.filterWithKey (flip $ const $ not . negPred b) $
					M.difference (posSet a) (negSet b),
				M.filterWithKey (flip $ const $ not . negPred a) $
					M.difference (posSet b) (negSet a)
			],
		negPred = foldl (liftA2 (||)) (const False) [
				(&&) <$> negPred a <*> negPred b,
				(&&) <$> (not <$> posPred a) <*> negPred a,
				(&&) <$> (not <$> posPred b) <*> negPred b
			],
		negSet = foldl M.union M.empty [
				-- M.filterWithKey (flip $ const $ not . posPred a) (negSet a),
				-- M.filterWithKey (flip $ const $ not . posPred b) (negSet b),
				-- M.filterWithKey (flip $ const $ negPred a) (negSet b),
				-- M.filterWithKey (flip $ const $ negPred b) (negSet a),
				M.filterWithKey (flip $ const $ (||) <$> negPred b <*> not . posPred a) (negSet a),
				M.filterWithKey (flip $ const $ (||) <$> negPred a <*> not . posPred b) (negSet b),
				M.intersection (negSet a) (negSet b)
			]
	}

subtract :: Filter -> Filter -> Filter
subtract a b = Filter {
		posPred = const False,
		posSet = M.filterWithKey (flip $ const $ not . posPred b) $ flip M.difference (posSet b) $ foldl M.union M.empty [
				M.filterWithKey (flip $ const $ not . negPred a) $ M.difference (negSet b) (negSet a),
				M.filterWithKey (flip $ const $ posPred a) (negSet b),
				M.filterWithKey (flip $ const $ negPred b) (posSet a),
				M.intersection (posSet a) (negSet b)
			],
		negPred = foldl (liftA2 (||)) (const False) [
				(&&) <$> not . posPred a <*> negPred a,
				not . negPred b,
				posPred b
			],
		negSet = M.union (posSet b)
			(M.filterWithKey (flip $ const $ not . posPred a) (negSet a))
	}

vIntersect :: Functor m => Value m -> Filter -> Value m
vIntersect a b = flip Value Nothing $ flip fmap (tracks a) $ \case
	Ordered tracks -> Ordered $ filter (apply b) tracks
	Unordered tracks -> Unordered $ applyIntersect b tracks

vSubtract :: Functor m => Value m -> Filter -> Value m
vSubtract a b = flip Value Nothing $ flip fmap (tracks a) $ \case
	Ordered tracks -> Ordered $ filter (not . apply b) tracks
	Unordered tracks -> Unordered $ applySubtract b tracks
