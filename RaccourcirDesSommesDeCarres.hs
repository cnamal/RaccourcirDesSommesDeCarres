{- La représentation des listes croissantes d'entiers en jeux
 - sous la forme d'une structure de donnée arborescente permet
 - de faire l'économie de toute répétition de calculs.
 - L'implémentation exaxcte permet de simplifier ces calculs. -}
data Arbre = ArbreVide | Arbre Int [Arbre] deriving (Eq)

{- Afin que chaque solution soit représentée sur une
 - ligne distincte. On fait du type Arbre une instance
 - de la classe Show de la façon suivante.             -}
instance Show Arbre where
  show = montrerLesListes . toutesLesListes where
    toutesLesListes  :: Arbre -> [[Int]]
    toutesLesListes  ArbreVide       = []    -- Récupère tous
    toutesLesListes (Arbre n [])     = [[n]] -- les chemins
    toutesLesListes (Arbre n arbres) =       -- dans l'arbre.
      map (n:) $ (concat . map toutesLesListes) arbres
    montrerLesListes :: [[Int]] -> String
    montrerLesListes = (foldr (\x y -> x ++ "\n" ++ y) "") .
		       map ( (foldr (\x y -> x ++ " " ++ y) "")
			     . map show . drop 1               )
			     -- le drop 1 élimine les 0 du début

{- Le coeur de l'algorithme, qui construit toutes les suites
 - croissantes de somme des carrés donnée en éliminant dés
 - que possible toutes les suites qui ne peuvent convenir.
 - En pratique, on appliquera la fonction avec début = 0 afin
 - d'assurer la présence de toutes les suites de nombres entiers
 - positifs distincs-}
arbreDeSommeDesCarresDonnee :: Int -> Int -> Int -> Int -> Arbre
arbreDeSommeDesCarresDonnee debut borne longueur sommeDesCarres
  | sommeDesCarres < 0        = ArbreVide      -- Optimisation.
  | longueur == 1 &&                           -- Ces lignes
    debut^2 == sommeDesCarres = Arbre debut [] -- assurent que
  | longueur == 1             = ArbreVide      -- l'arbre aura
  | arbres == []              = ArbreVide      -- la longueur
  | otherwise = Arbre debut arbres             -- souhaitée.
  where arbres =                          -- Seuls les sous-
	  filter arbreParfait $           -- arbres potentiels
	    [ arbreDeSommeDesCarresDonnee -- sont testés.
		debut'                       -- Fixer une borne
		borne                        -- accélère
		(longueur - 1)               -- grandement
		(sommeDesCarres - debut^2) | -- l'algorithme.
	      debut' <- [debut + 1 .. borne + 2 - longueur] ]
	arbreParfait :: Arbre -> Bool         -- Teste la
	arbreParfait  ArbreVide       = False -- présence
	arbreParfait (Arbre _ arbres) =       -- de sous-arbres
	  (and . map arbreParfait) arbres     -- non-conformes.

{- Etant donné une somme de la forme 1²+2² -}
raccourcir longueurInitiale longueurFinale =
  arbreDeSommeDesCarresDonnee 0 borne (longueurFinale + 1) somme
    where
      sommeCarre debut fin = sum $ map (^2) [debut..fin]
      somme = sommeCarre 1 longueurInitiale
      borne = floor . sqrt . fromIntegral $
		sommeCarre longueurFinale longueurInitiale

sommeCarre :: [Int] -> Int
sommeCarre xs = sum $ map (^2) xs

maxMoins2 longueur = sommeCarre [1..longueur-2]
diffmax   longueur = sommeCarre [longueur-1,longueur]
maxou     longueur = maxMoins2 longueur + diffmax longueur

listesDentiers longueur = melangeCroissant $
  take (longueur - 1) $
  repeat [1..(floor . sqrt . fromIntegral $ diffmax longueur :: Int)]

melangeCroissant []     = [[]]
melangeCroissant [xs]   = [[x] | x <- xs]
melangeCroissant (l:ls) = [(x:xs) | x <- l, xs <- melangeCroissant ls,x < head xs]

listesDeSommeMaxou longueur = [xs | xs <- listesDentiers longueur, sommeCarre xs == maxou longueur]

arbreCroissant debut _   1        = Arbre debut []
arbreCroissant debut fin longueur =
  Arbre debut 
        [arbreCroissant debut' fin (longueur - 1) | debut' <- [debut + 1 .. fin + 2 - longueur]]
