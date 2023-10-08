{- MergeStar.hs
     functions for constructing sets with *- and +-extended merge,
        labeling them,
        transducing them to linear form (i.e. strings),
        pretty-printing them.

This code was developed and tested in GHCi, versions 9.2.5 and 9.4.7

EXAMPLE: Load this file, then evaluate "ex4 30" for Irish coordination of 3 elements.
         Many other examples at bottom of file.

As described in Stabler & Yu 2023, only negative *- and +-features are handled.
In external merges, a negative *f (+f) selects 0 or more (1 or more)
  positive f elements 'simultaneously'.
Extending * and + to internal merges and to pos features intersects with
  earlier ideas about feature persistence -- for later work.
-}
import Data.Set (Set)
import qualified Data.Set as Set

data Cat = C | D | N | V | Vo | A | P | Wh | R | L | B | TM | Predx | Pred  deriving (Show, Eq, Ord)
data Feature = F Cat | Plus Cat | Star Cat  deriving (Show, Eq, Ord)
data SO = Lex [String] [Feature] [Feature] | M (Set SO) deriving (Show, Eq, Ord) 
data LabeledMover = Mv SO [String] [Feature] deriving (Show, Eq, Ord) 
data LabeledSO = LSO SO [String] [Feature] [Feature] [LabeledMover] deriving (Show, Eq, Ord) 

-- s converts list to set
s :: Ord a => [a] -> Set a
s = Set.fromList 

-- l converts set to list
l :: Set a -> [a]
l = Set.elems

-- map any sequence (list) of elements to the set of those elements
mergeStar x = M (s x)

--  wrap so in a label: (LSO so string pos-features neg-features movers), else error!
label (Lex w nw pw) = LSO (Lex w nw pw) w nw pw []
label (M mg) = case negsPoss (map label (l mg)) of
    -- make sure syntactic head is unique
    ((neg:[]), poss) -> case neg of
      -- check requirements of the negative feature
      (LSO x xs xn xp xmv) -> case (head xn, poss) of
        ( (F f),    (ly:[])  ) -> label' (M mg) x xs f (tail xn) xp xmv ly []
        ( (Plus f), (ly:more)) -> label' (M mg) x xs f (tail xn) xp xmv ly more
        ( (Star f), (ly:more)) -> label' (M mg) x xs f (tail xn) xp xmv ly more
        ( (Star f), []       ) -> LSO x xs xn xp xmv
        otherwise -> error "feature error in labeling"
    otherwise -> error "label error: multiple neg"

-- label' m by checking negative cat f of x against features in y and in more
label' m x xs f xn xp xmv (LSO y ys yn yp ymv) more =
  case selectMvr y ys f xmv of    -- try internal merge first
    Just (ys',xymv) ->            -- matching feature found by selectMvr
      if more == []               -- no +- or *-extensions for internal merge
      then LSO m (klinear x xs ys') xn xp xymv
      else error "label' error: moving complement with additional sisters"
    Nothing -> case head yp of    -- ok, try external merge
      F g ->
        if f == g                 -- matching feature found
        then let typ = tail yp in
          if typ == []
          then 
            if allMatch f more    -- In essence, this is the *-extension!
            then LSO m (klinear x xs (ys ++ allStrings more)) xn xp (xmv ++ ymv)
            else error "label' error: features of more do not match"
          else case (more, head typ) of
            ([], (F g)) ->
              let newmv = smc g (smc g [Mv y ys typ] ymv) xmv in
               LSO m (klinear x xs (delPh ys)) xn xp newmv
            otherwise -> error "label' error: more must be empty when |pos features|>1"
        else error "label' error: features do not match"
      otherwise -> error "label' error: positive features must be (F _)"

-- (smc f new) maps xmv to (xmv ++ new) if nothing in xmv begins with f, else error!
smc f new = foldr (\x y -> case x of
                      (Mv z zs ((F g):zp)) -> 
                        if f == g
                        then error ("error: smc violation on " ++ (show g))
                        else (x:y)
                      otherwise -> error "smc error: ill formed mover list"
                   ) new

-- Kayne-like linearization: complexes select on left, lex items select on right
klinear (M _) x y = y ++ x
klinear    _  x y = x ++ y

-- selectMvr, a partial function, selects mover that matches feature f, if any
selectMvr z zs f mvrs = case mvrs of
   [] -> Nothing
   (mv:mvrs') -> case mv of
     (Mv m ms ((F g):mp)) -> 
       if f /= g
       then case selectMvr z zs f mvrs' of
           Just (zs', mvrs'') -> Just (zs', (mv:mvrs''))
           Nothing -> Nothing
       else                     -- feature matches!
         if m /= z
         then Nothing
         else                   -- structure z is, in fact, m, as required
           if mp == []
           then Just (zs, mvrs')
           else Just (delPh zs, (Mv m ms mp):mvrs') -- mover launches again
     otherwise -> Nothing

-- return concatenation of strings from list of labels
allStrings = foldr (\x y -> case x of
                       (LSO _ xs _ _ _) -> xs++y
                   ) []

-- negsPoss splits list of labels into those with neg features and those without
negsPoss = foldr (\x y -> case x of
                     LSO _ _ (_:_) _ _ -> let (ns,ps) = y in ( x:ns, ps )
                     otherwise -> let (ns,ps) = y in ( ns, x:ps )
                 ) ([],[])

-- allMatch f returns true iff every mover in list has first feature f
-- In essence, this is the *-extension!
allMatch f = foldr (\x y -> case x of
                       (LSO _ _ [] (F h:[]) []) -> f == h && y
                   ) True

-- mark unpronounced phon contents with parentheses
delPh = map (\x -> if (length  x > 1 && head x == '(' && last x == ')')
                   then x
                   else "(" ++ x ++ ")"
           )

-- IO -- prettyPrint various structures
ppMg = ppMg' 0

ppMg' i (Lex w x y) = do { tab i ; putStr (joinstr " " w) ; putStr "::" ; putStr (fs2str x y) }
ppMg' i (M xs) = do { tab (i+2) ; putStrLn "{ " ; ppMgs (i+2) (l xs) ; putStr " }" }

tab 0 = putStr ""
tab n = do { putStr " "; tab (n-1) }

ppMgs _ [] = putStr ""
ppMgs i (x:[]) = ppMg' i x
ppMgs i (x:xs) = do { ppMg' i x ; putStrLn ", " ; ppMgs i xs }

ppMvs [] = putStrLn ""
ppMvs ((Mv x xs xp):more) = do
  {putStr " $ "; ppMg x ; putStr ":" ; putStrLn (fs2str [] xp) ; ppMvs more}

ppLSO (LSO x xs xn xp xmv) = do {ppMg x; putStr ":"; putStr (fs2str xn xp); ppMvs xmv}

fs2str [] p = (joinstr "." (map showF p))
fs2str n p = (joinstr "." (map showF n)) ++ " -o " ++ (joinstr "." (map showF p))

showF f = case f of { F x -> show x ; Plus x -> (show x) ++ "+" ; Star x -> (show x) ++ "*"}

joinstr _ [] = ""
joinstr _ (h:[]) = h
joinstr x (h:t) = h ++ x ++ (joinstr x t)

-- EXAMPLES -- Empty cats can be [], but [""] lets us see empty cat traces
-- simple wh movement (*- and +-extensions not needed)
l0 = Lex ["the"] [F N] [F D]
l1 = Lex ["students"] [] [F N]
l2 = Lex ["teachers"] [] [F N]
l3 = Lex ["saw"] [F D] [F V]
l4 = Lex ["they"] [] [F D]
l5 = Lex ["who"] [] [F D,F Wh]
l6 = Lex ["which"] [F N] [F D,F Wh]
l7 = Lex [""] [F V,F D] [F Vo] -- Vo(ice), aka little v, selects external argument
l8 = Lex [""] [F Vo,F Wh] [F C]
l9 = Lex [""] [F Vo] [F C]
l10 = Lex ["know"] [F C] [F V]
-- remnant movement for copy language (*- and +-extensions not needed)
x0 = Lex [""] [] [F C,F R,F L]
x1 = Lex [""] [F C,F R,F L] [F C]
x2 = Lex ["a"] [F C,F R] [F A,F R]
x3 = Lex ["b"] [F C,F R] [F B,F R]
x4 = Lex ["a"] [F A,F L] [F C,F L]
x5 = Lex ["b"] [F B,F L] [F C,F L]
-- simple Irish CP grammar with + extension
i0 = Lex [] [F TM] [F C]
i1 = Lex [] [F V] [F TM]
i2 = Lex [] [F Pred, F D] [F Predx]
i3 = Lex ["is"] [F A] [F V]
i4 = Lex ["cuma"] [F Predx] [F A]
i5 = Lex ["e"] [] [F D]
i6 = Lex ["na"] [F D] [F Pred]
i7 = Lex ["shamhradh"] [] [F D]
i8 = Lex ["fhomhar"] [] [F D]
i9 = Lex ["gheimhread"] [] [F D]
i10 = Lex ["no"] [F Pred, Plus Pred] [F Pred]  -- compare Pred* in place of Pred+
-- 10 steps for CP "they know which students the teachers saw"
mgex 0 = mergeStar [l6, l1]
mgex 1 = mergeStar [l3, mgex 0]
mgex 2 = mergeStar [l7, mgex 1]
mgex 3 = mergeStar [mergeStar [l0, l2], mgex 2]
mgex 4 = mergeStar [l8, mgex 3]
mgex 5 = mergeStar [mgex 0, mgex 4]
mgex 6 = mergeStar [l10, mgex 5]
mgex 7 = mergeStar [l7, mgex 6]
mgex 8 = mergeStar [l4, mgex 7]
mgex 9 = mergeStar [l9, mgex 8]
-- smc violation blocks "which students which teachers saw" -- in situ "which" needs diff features
mgex (-3) = mergeStar [mergeStar [l6, l2], mgex 2]
-- 3 steps for CP empty string "" in the copy language
mgex 10 = mergeStar [x0, x1]
mgex 11 = mergeStar [x0, mgex 10]
mgex 12 = mergeStar [x0, mgex 11]
-- 7 steps for CP "a a"
mgex 13 = mergeStar [x2, x0]         -- x2 is the left a
mgex 14 = mergeStar [x0, mgex 13]
mgex 15 = mergeStar [x4, mgex 14]    -- x4 is the right a
mgex 16 = mergeStar [x0, mgex 15]
mgex 17 = mergeStar [x1, mgex 16]
mgex 18 = mergeStar [mgex 14, mgex 17] -- remnant movement
mgex 19 = mergeStar [mgex 16, mgex 18] -- remnant movement
-- CP "is cuma e na shamhradh na fhomar no na gheimhreadh"
mgex 20 = mergeStar [i6, i7]
mgex 21 = mergeStar [i6, i8]
mgex 22 = mergeStar [i6, i9]
mgex 23 = mergeStar [i10, mgex 22]
mgex 24 = mergeStar [mgex 23, mgex 20, mgex 21]
mgex 25 = mergeStar [i2, mgex 24]
mgex 26 = mergeStar [i5, mgex 25]
mgex 27 = mergeStar [i4, mgex 26]
mgex 28 = mergeStar [i3, mgex 27]
mgex 29 = mergeStar [i1, mgex 28]
mgex 30 = mergeStar [i0, mgex 29]

ex2 n = do {ppMg (mgex n) ; putStrLn ""}
ex3 n = ppLSO (label (mgex n))
-- examples: klinear -- evaluate "ex4 n" for n >= 0
ex4 n = case label (mgex n) of
    (LSO x xs xn xp xmv) -> do
      ppMg (mgex n)
      putStrLn ""
      putStrLn (fs2str xn xp)
      ppMvs xmv
      putStrLn (joinstr " " xs)
