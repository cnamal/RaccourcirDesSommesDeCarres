{- La fonction suivante sera d'usage constant. -}
sommeCarre :: [Int] -> Int
sommeCarre = sum . map (^2)

{- Etant donné une somme de la forme 1²+2² +...+n², on se
 - demande si on on peut atteindre la même valeur avec une
 - somme de carrés de nombres positifs distincs non nuls
 - contenant un nombre m strictement inférieur à n de termes.
 - En appelant longueurInitiale le nombre n et longueurFinale
 - le nombre m, la fonction raccourcir qui suit renvoie
 - toutes les suites strictement croissantes qui conviennent. -}
raccourcir longueurInitiale longueurFinale =
  listesDeSommeDesCarresDonnee borne longueurFinale somme
    where
      somme = sommeCarre [1 .. longueurInitiale]
      borne = floor . sqrt . fromIntegral $
		sommeCarre [longueurFinale .. longueurInitiale]
           -- Au pire, les m-1 premiers nombres vont de 1 à m-1.

{- "listesCroissantes borne longueur" renvoie toutes les
 - listes croissantes d'entiers positifs non nuls
 - inférieurs ou égaux à borne de la longueur donnée.    -}
listesCroissantesDentiers borne longueur =
  melangeCroissant . take longueur . repeat $ [1 .. borne] where
    melangeCroissant []     = [[]]
    melangeCroissant [xs]   = [[x] | x <- xs]
    melangeCroissant (l:ls) = [ (x:xs) |
				x  <- l,
				xs <- melangeCroissant ls,
				x  < head xs               ]

{- Les listes créées par la fonction précédente
 - sont filtrées pour que ne soient conservées
 - que celles dont la somme des carrés convient -}
listesDeSommeDesCarresDonnee borne longueur somme =
  [xs | xs <- listesCroissantesDentiers borne longueur,
        sommeCarre xs == somme]
