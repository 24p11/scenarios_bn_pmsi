# Visite guidée du pipeline scenarios_bn_pmsi (v8)

Document d'orientation : *où* se trouve chaque chose, et surtout *pourquoi*
elle est construite ainsi. Les renvois sont de la forme
`fichier :: fonction`. Le journal (`MODIFICATIONS_V8.md`) reste la référence
exhaustive des modifications ; ce document est le récit qui les relie.

---

## 1. Les deux mondes, et le contrat entre eux

Tout le pipeline repose sur une frontière posée très tôt et jamais franchie :

- **Le monde EXTRACTION** a besoin de la base nationale. La connexion est
  ouverte à un seul endroit : le début d'`extraction_associations_codes_v8.R`.
  Ce monde ne produit que des **fichiers de comptes agrégés** (parquet) —
  jamais de données au niveau du séjour sur le disque. Les tables au niveau
  séjour (`prep_data_<an>`…) vivent en tables temporaires dans la base : elles
  disparaissent quand on se déconnecte et sont recréées à la demande.
- **Le monde AVAL** (sélection, tirage des DAS, habillage, finalisation,
  registre) n'ouvre **jamais** de connexion. Il ne lit que les parquets
  produits par l'extraction. C'est vérifié par un test qui coupe la connexion
  avant la phase de tirage pour prouver qu'elle ne sert plus.

Le « contrat » entre les deux mondes, c'est le contenu du dossier
`exports/` : le catalogue, les dix tables de référence, les variantes
administratives. Tous ces fichiers sont des comptes agrégés, seuillés ou
massifs — c'est aussi ce qui permettrait un jour de faire tourner l'aval en
dehors de la plateforme.

## 2. Les fichiers et leur rôle (une phrase chacun)

| Fichier | Rôle |
|---|---|
| `config_v8.R` | Tous les paramètres ; bloc PROFIL (diagnostic / production), surcharge par fichier externe, vérifications de cohérence. |
| `helpers_v8.R` | Les fonctions « pures » (calcul seul, pas d'accès base), testables sur ordinateur — sections A à G, voir §4. |
| `etapes_v8.R` | Le cœur : les requêtes base recopiées des v7 + les fonctions d'étape (`etape_...`) + le tableau de bord `etat_pipeline()`. |
| `extraction_associations_codes_v8.R` | Lanceur du monde extraction (46 lignes : config, chargements, connexion, 4 appels d'étapes). |
| `tirage_scenarios_v8.R` | Lanceur du monde aval (32 lignes, aucune connexion). |
| `utils.R`, `referentiels.R`, `exclusions.R` | Héritage v7 toujours chargé (opérateur `%+%`, codes diabète, listes d'exclusion). |
| `RUN.Rmd` | Notebook de l'amont (extraction, passe diagnostic). |
| `RUN_aval.Rmd` | Notebook de l'exploitation (du catalogue aux campagnes). |
| `RUN.md` | La référence texte des deux séquences + les règles de cache. |
| `MODIFICATIONS_V8.md` | Le journal : d'où vient chaque bloc de code, chaque écart autorisé, les questions Q1…Q40 (les cas limites déjà tranchés). |
| `referentiels/*.yaml` | La doctrine externalisée : codes diabète, exclusions de paires, typologie des séjours. |
| `tests/test_helpers.R` | ~300 vérifications des fonctions pures. |
| `tests/test_chaines_sqlite.R` | ~140 vérifications : les scripts réels exécutés sur une fausse base SQLite (avec un faux paquet `pRatihque`), y compris sessions coupées et campagnes C1/C2. |
| `demo/` | Le mode démo hors plateforme : générateur de données fictives et faux paquet `pRatihque` (source unique, aussi utilisés par les tests), base SQLite, lanceur du pipeline complet ; `demo/README.md`. |

## 3. Le flux de données de bout en bout

```
BASE NATIONALE (vues PMSI .fixe / .um / .diag / .rgp)
   │  etape_prep_data()          [etapes_v8.R :: prep_data]
   ▼
prep_data_<an>   table TEMPORAIRE en base — UNE ligne par séjour,
   │             unité la plus « prioritaire » retenue (réa d'abord),
   │             indicateurs diabète / HTA
   │
   ├── etape_refs() ───────────► exports/*.parquet   (les 10 références, §5b)
   │
   │  etape_partiels_longs()     [prep_scenarios2 : pour chaque séjour long,
   ▼                              les 2 DAS les plus sévères = la « graine »]
partiels/catalogue_partiel_<etbs>_<an>.parquet
   │             un fichier de comptes par (établissements × année) :
   │             c'est à la fois une sauvegarde de reprise et un cache
   │
   │  etape_catalogue(ans, etbs) [agrégation en 2 temps + conversion E669
   ▼                              + seuil de confidentialité > 10]
catalogue_longs_seuil.parquet    21,6 M de lignes : 12 pivots × graine × poids
   │  etape_repartitionner_catalogue()  [+ lettre du DP, DPEC/TPEC, id_profil]
   ▼
catalogue_longs_seuil/part_<lettre>.parquet     LE RÉFÉRENTIEL PERMANENT
   │  etape_selection_longs()    [quota par DP, k lignes, variantes ;
   ▼                              plafonds de classe ; registre]
selection_longs/<population>/  (+ meta_tirage.yaml : le contrat du tirage)
   │  etape_tirage_das_longs()   [ajout des DAS de complétion, par paquets]
   ▼
chunks/<population>/longs_chunk_XXXX.parquet    (reprise fichier par fichier)
   │  etape_habillage_longs()    [modes d'entrée/sortie, depuis le parquet
   ▼                              v_admin_longs — jamais depuis la base]
   │  etape_finalisation()       [contrôles, rapport, échantillon de revue]
   ▼
scenarios_longs_tirage_v8_<campagne>/<population>/part_*.parquet  (+ _meta.yaml)
   +  registre_tirages/registre_<campagne>.parquet
```

La branche **séjours courts** est parallèle et plus simple : `pivots_courts`
(seuil appliqué en base) → `etape_tirage_courts()` (nombre de pathologies
chroniques tiré dans la distribution observée chez les séjours longs — voir
§5b) → habillage → finalisation.

### Où vit quoi : la carte des dossiers d'une campagne

Deux familles sous le dossier d'exports du profil (`exports/` ou `exports_diagnostic/`,
un par profil, jamais mélangés) :

- **Permanent** — le catalogue en parts (`catalogue_longs_seuil/`), les dix tables
  de référence, le registre (`registre_tirages/`, qui ne se vide jamais), les
  corpus des campagnes passées (`scenarios_longs_tirage_v8_<Cn>/`, un dossier par
  campagne avec son `_meta.yaml`, jamais écrasé par une autre campagne), les
  scénarios courts datés. Écrits par le repartitionnement, les refs, la
  finalisation ; lus par la sélection et la livraison.
- **Par campagne** — la sélection (`selection_longs/`, `meta_tirage.yaml`), les
  paquets de tirage (`chunks/`), l'habillé (`habille/`), le rapport et la revue.
  Écrits par les étapes de la campagne, vidés (chunk gardé) à l'ouverture de la
  suivante.

Ce qui se migre d'un profil à l'autre : le catalogue et ses refs (condition Q13).
Ce qui ne se migre pas : les sorties de tirage, qui se refont sous le profil cible
(rapide, sans base) — le message « courts absent » le dit en trois branches.

## 4. `helpers_v8.R` : les sections A→G racontent l'histoire du projet

Les sections sont chronologiques — chacune correspond à un chantier. Voici ce
que fait chacune, étape par étape.

### A. Les helpers de tirage (le cœur historique)

- `sample_das_court` / `sample_das_long` : le tirage au sort des DAS. Pour un
  scénario donné, on regarde la table de référence de sa strate (même DP,
  sexe, classe d'âge…), et on tire des codes au hasard, chaque code ayant une
  probabilité proportionnelle à sa fréquence réelle dans la base.
- `tirer_nb_chroniques` : combien de pathologies chroniques donner à un
  séjour court ? On tire ce nombre dans la distribution observée chez les
  patients de même âge et sexe hospitalisés **longtemps** (là où le codage
  est complet), zéros compris.
- `dedup_categorie` : après tirage, on ne garde qu'un code par catégorie de
  3 caractères (pas deux variantes d'insuffisance cardiaque), plus quelques
  exclusions explicites listées dans un fichier yaml.
- Le rétro-codage diabète : dans les tables de référence, tous les codes
  diabète sont repliés en trois « néo-codes » (E10, E11 insulinotraité, E11
  non insulinotraité) pour que le diabète compte comme UNE maladie. Quand un
  néo-code est tiré, on le re-déplie en codes réels : le chiffre de
  complication est tiré dans une table où les « sans complication » (.9) ont
  été volontairement pénalisés.

### B. Industrialisation (paquets, reprise, plan de travail)

Cette section répond à un problème simple : les calculs durent des heures et
la plateforme peut planter — il ne faut jamais perdre le travail déjà fait.

- `pmap_chunks` : découpe un gros tirage en **paquets** (« chunks »). Chaque
  paquet est calculé puis immédiatement sauvegardé dans son propre fichier
  parquet. À la relance, les fichiers déjà présents sont **sautés** : on ne
  refait que ce qui manque. Trois précautions rendent ça sûr :
  1. *Un tirage aléatoire propre à chaque paquet* (graine = SEED + numéro du
     paquet) : une exécution interrompue puis reprise donne exactement le
     même résultat qu'une exécution d'une traite, au caractère près.
  2. *Un fichier compagnon* (`..._chunks_meta.yaml`, posé à côté des paquets)
     mémorise le découpage : nombre de lignes, taille des paquets, graine. À
     la reprise, si ces valeurs ne correspondent plus (par exemple parce
     qu'on a changé le budget), le programme **s'arrête** en l'expliquant —
     car le paquet n°12 de l'ancien découpage ne contient pas les mêmes
     lignes que le n°12 du nouveau, et les mélanger fausserait tout sans
     bruit.
  3. *Une écriture en deux temps* (fichier `.tmp` puis renommage) : un
     plantage en pleine écriture ne peut pas laisser un paquet à moitié plein
     qui serait pris pour complet.
  Le paramètre `chunk_range = c(i, j)` permet de ne traiter qu'une plage de
  paquets : c'est ce qui autorise le **parallélisme** — plusieurs sessions R,
  chacune sur sa plage, écrivant dans le même dossier.
- `taille_chunk` : la taille des paquets n'est plus fixe, elle se calcule à
  partir du volume à traiter, avec une règle simple : jamais plus de 50
  paquets (`NB_CHUNKS_MAX`), jamais de paquets minuscules.
- La *résolution des besoins* (début de l'extraction) : le script commence
  par dresser le **plan** de ce qui manque — quels fichiers de comptes
  partiels, quelles tables de référence — et n'exécute que ça. Comme les
  tables temporaires en base meurent entre deux sessions, seules les années
  dont on a réellement besoin sont re-préparées. Quand tout est déjà là, le
  script l'annonce et ne lance aucune requête.
- Le *recouvrement* : compare deux fichiers de comptes partiels (par exemple
  CHR/U 2024 et 2025) et répond à la question « qu'apporte une année de
  plus ? » — pourcentage de combinaisons déjà vues, nombre de DP nouveaux.
  C'est l'outil de décision du périmètre (combien d'années, quels
  établissements).
- `memoire_session` : liste les objets présents en mémoire, du plus gros au
  plus petit, dans tous les recoins du programme — pour trouver le coupable
  quand la RAM monte.

### C. Conversion E669 → E660 (une doctrine de codage)

Position de doctrine : dans la base nationale, les codes E669x (obésité
« sans précision ») sont des erreurs de codage — en France, avec un dossier
complet, on connaît la cause. Ils sont donc convertis en E660x (« dus à un
excès calorique »).

- `convertir_e669` : le cas simple. Un E669 **avec** classe d'IMC garde sa
  classe : E6692 devient E6602. Aucun tirage, aucune perte d'information.
- `repartir_e669_nu` : le cas difficile. Un E669 **nu** (sans classe d'IMC,
  fréquent dans les vieux millésimes) ne dit pas quelle classe donner. On ne
  l'invente pas : on **répartit ses effectifs** proportionnellement à la
  distribution des classes réellement observées, en cascade — d'abord la
  distribution de sa strate (âge × sexe) ; si la strate n'a aucun E660x
  observé, la distribution nationale toutes strates ; en tout dernier
  recours, la classe la plus basse (E6600). La répartition utilise la
  méthode des « plus forts restes » : un arrondi qui garantit que la somme
  des effectifs est conservée exactement.
- Où et quand : la conversion se fait **après** le rapatriement des données
  en R — les requêtes base ne sont jamais modifiées, et les fichiers de
  comptes partiels restent stockés en codes bruts (le cache reste valable,
  on peut activer ou désactiver la conversion sans tout recalculer). L'ordre
  est impératif : **convertir → re-sommer → appliquer le seuil**. Ainsi deux
  profils identiques à l'orthographe E669/E660 près fusionnent et comptent
  ensemble face au seuil de confidentialité — deux profils à 7 et 6 séjours,
  éliminés séparément, passent ensemble à 13.

### D. Mémoire 15 GiB (tenir dans la limite de la plateforme)

La plateforme limite la mémoire à 15 GiB. Deux points du code dépassaient ;
cette section contient les remèdes.

- Le premier coupable était `prep_scenarios2` : il rapatriait en R **toutes**
  les lignes séjour × DAS d'une année (des millions) pour ne garder au final
  que 2 DAS par séjour. Le remède : le tri et la sélection des 2 meilleurs
  DAS sont désormais faits **dans la base** (fenêtres SQL, les mêmes que
  celles de `prep_data`), et seules 2 lignes étroites par séjour reviennent
  en R. `collapse_graine` fait ensuite l'assemblage « das1 das2 » avec une
  astuce vectorielle (un seul appariement d'ensemble au lieu d'un collage
  par séjour) — rapide et économe.
- `agreger_partiels` : additionne la pile des fichiers de comptes partiels.
  Deux chemins pour le même résultat : le chemin **arrow** (production — le
  moteur arrow lit et agrège les fichiers avec une mémoire bornée, seul le
  résultat final entre en R) et un chemin **incrémental** en R pur (les
  fichiers lus un par un, le cumul tenu au fil de l'eau) utilisé par les
  tests et comme référence. Le bon chemin est choisi automatiquement.
- `mesurer_memoire` : après chaque rapatriement important, note la taille de
  l'objet et le pic mémoire dans `diagnostic_memoire.csv`, avec un
  avertissement au-dessus de 10 Go. C'est l'instrument de calibration : il
  dit si une étape s'approche de la limite avant qu'elle ne la crève.
- La discipline associée, documentée dans RUN.md : **Restart R avant chaque
  étape lourde**. R ne rend presque jamais la mémoire au système ; l'état
  utile étant sur disque, redémarrer ne coûte rien et repart avec les
  15 GiB entiers.

### E. Aval production (du catalogue au tirage de masse)

- `typologie_sejour` : classe chaque séjour dans la typologie STREAM
  (DPEC, le détail : « Accouchement normal mère », « Chirurgie adultes
  > 3 nuits »… ; TPEC, le regroupement : Médecine, Chirurgie, Obstétrique…).
  C'est la traduction fidèle du code Python de STREAM, avec une propriété à
  connaître absolument : **l'ordre des conditions fait la priorité**. Un
  séjour qui correspond à plusieurs règles reçoit la PREMIÈRE — les cas
  particuliers (greffes, obstétrique, néonatalogie, séances) sont testés
  avant le tout-venant médecine/chirurgie, sinon ils seraient avalés par
  lui. Pour le catalogue des longs, la durée est fixée à 3 (tous les séjours
  y font plus de 3 nuits par construction).
- `lire_catalogue` : le **lecteur unique** du catalogue. Depuis le
  découpage en parts par lettre de DP, plus personne ne charge le catalogue
  entier par accident : toute lecture passe par cette fonction, qui permet
  de ne demander que certaines lettres et certaines colonnes. (Un
  chargement direct du fichier de 21,6 M de lignes, c'est plusieurs Go d'un
  coup — l'accident est déjà arrivé.)
- La sélection `quota_dp_fixe` : décrite en détail au §5c.
- `indexer_ref_das` / `indexer_ref_chronique` : le remède au problème de vitesse du tirage. Avant, chaque
  scénario refiltrait la table de référence entière (des millions de
  lignes) pour trouver sa strate — quelques millisecondes × des millions de
  scénarios = des jours perdus. Désormais la table est découpée **une seule
  fois**, au démarrage, en une liste nommée par strate ; chaque scénario
  fait un accès direct par sa clé, sans parcourir la table. Résultat prouvé
  strictement identique (même graine aléatoire → mêmes tirages), vitesse
  multipliée par 50 à 100.

### F. Finitions exploitation (leçons des premières exécutions réelles)

- La migration entre profils : le catalogue avait été produit en profil
  « diagnostic » (dossier `exports_diagnostic/`) et la session de production
  le cherchait dans `exports/`. D'où deux aides : `localiser_catalogue`
  cherche le catalogue dans les autres dossiers d'exports du même projet
  (sous ses deux formes, fichier unique ou dossier de parts), et
  `condition_q13` vérifie si les tables de référence peuvent être copiées
  d'un profil à l'autre — c'est le cas si et seulement si six paramètres
  sont identiques (l'année de référence, les trois seuils des tables de
  référence, les deux paramètres de la conversion E669). Si l'un diffère, on
  copie le catalogue seul et on recalcule les références.
- Les messages « actionnables » : une erreur du type « catalogue absent »
  indique désormais le dossier exactement cherché, les endroits où un
  catalogue a été trouvé, et les options dans l'ordre du moins coûteux
  (copier) au plus coûteux (relancer l'extraction). Le principe général :
  un message d'erreur doit dire quoi faire, pas seulement ce qui manque.

### G. Campagnes (la mémoire entre les tirages successifs)

Le corpus se construit par campagnes de quelques centaines de milliers de
scénarios. Cette section donne au pipeline une mémoire de ce qui a déjà
servi.

- `id_profil_de` : donne à chaque ligne du catalogue un **identifiant
  stable**. La recette (baptisée `id_v1` et FIGÉE — la changer invaliderait
  toute la comptabilité) : concaténer les 12 pivots puis la graine, dans un
  ordre fixe, avec un séparateur invisible ; calculer l'empreinte sha256 de
  cette chaîne ; garder les 16 premiers caractères. Propriétés qui comptent :
  le même contenu donne le même identifiant, partout, pour toujours, quel
  que soit l'ordre des lignes du fichier — on peut donc **recalculer** les
  identifiants sur n'importe quel fichier ancien. Un test contient la valeur
  attendue en dur : si quelqu'un modifie la recette par mégarde, le test
  casse immédiatement.
- `hash_das_de` : l'empreinte du **jeu de DAS complet réellement tiré**
  (graine + complétions + codes de doctrine), les codes étant triés avant le
  calcul pour que l'ordre ne compte pas. Deux scénarios portant les mêmes
  codes ont la même empreinte — c'est l'outil de détection des doublons.
- Le **registre** (`registre_tirages/`) : un fichier parquet par campagne,
  listant chaque scénario produit (identifiant de profil, numéro de
  variante, empreinte du jeu de DAS, campagne, population, DP, classe DPEC).
  Règle stricte : **on ne fait qu'ajouter, jamais réécrire** — tenter de
  réécrire un fichier de campagne avec un contenu différent provoque un
  arrêt. `lire_registre` fournit les agrégats utiles : pour chaque profil,
  le plus grand numéro de variante déjà utilisé et les empreintes déjà
  produites ; par DP et par classe, la consommation cumulée.
- La sélection **sous registre**, en deux temps : (1) *la fraîcheur
  d'abord* — les lignes du DP sont tirées parmi celles jamais utilisées
  (leur identifiant est absent du registre) ; (2) si le DP n'a plus assez de
  lignes vierges, le *recyclage à variantes nouvelles* — on reprend des
  lignes déjà utilisées, mais la numérotation des variantes reprend après le
  dernier numéro du registre, et les jeux de DAS déjà enregistrés pour ce
  profil sont écartés (via leur empreinte). Un DP épuisé reste ainsi présent
  à chaque campagne, avec des scénarios réellement nouveaux. Chaque campagne
  a sa graine aléatoire propre, dérivée de (SEED, nom de campagne).
- `etape_retro_inscrire` : reconstruit le registre d'une campagne tirée
  AVANT l'existence du registre — possible précisément parce que les
  identifiants se recalculent sur les fichiers.

### H. Notebook campagnes, config locale (leçons des premières campagnes réelles)

Quatre défauts d'exploitation et leurs remèdes : le corpus final était nommé par
date (deux campagnes le même jour s'écrasaient) → nommé par campagne, avec un
`_meta.yaml` et un garde-fou (`verifier_dossier_final`) ; les fichiers datés
(`scenarios_courts_v8_<date>`) cassaient la relecture un autre jour →
`resoudre_export_date` relit le fichier du jour, sinon le plus récent, en
l'annonçant ; le chunk de palier pouvait tirer par inadvertance → `palier_actif()`
et une bannière qui affiche campagne, budget effectif et surcharge active ; une
campagne déjà inscrite pouvait être resélectionnée → `statut_campagne_registre`,
affiché en session ; une campagne inscrite est close : sa sélection présente est
relue (relancer la même commande reste sûr), toute autre situation s'arrête avant
calcul, et jamais de re-tirage. Enfin les chemins personnels ont
quitté le code versionné : `config_locale.R` (ignoré par git) ou la variable
d'environnement, sinon arrêt explicite. Les séjours courts reçoivent leurs
identifiants (recette `id_courts_v1`, préfixe `c`), sans registre.

Correctifs post-contrôle : un notebook appelait directement un dataset arrow,
ce qui échoue sous le repli mock (arrow partiel) — d'où `lire_corpus_final`,
lecteur unique du corpus final à repli comme `lire_catalogue` et `lire_registre`,
et une CI qui exécute tout aussi **sans** arrow. Les chunks de lecture passent par
`lire_si_present` (fichier absent = message « produit par telle étape », jamais
une erreur R brute). Le palier ne peut plus écrire au registre (imposé dans
`palier.R`, refusé par `etape_registre_campagne`). La branche courts a la parité
des longs : plages `chunk_range` parallélisables, débit par chunk, extrapolation.

## 5. Les décisions de conception à connaître pour lire le code

### 5a. Le catalogue des longs
- **Graine k=2 réelle** : chaque profil du catalogue porte les 2 DAS les plus
  sévères d'un vrai séjour — le noyau du scénario vient du réel ; seule la
  complétion est tirée au sort. (Une complétion tenant compte des
  co-occurrences a été étudiée puis écartée : trop de complexité pour le
  bénéfice, vu l'objectif texte → codes.)
- **Le seuil de confidentialité (> 10) se juge au niveau du PROFIL** (les 11
  pivots hors nombre de DAS), pas au niveau profil × graine. Décision DIM
  assumée : la paire de DAS agrégée sur ~10 ans n'est pas identifiante, et
  exiger > 10 par paire détruirait la couverture (les combinaisons sont vite
  quasi uniques). C'est ce qui explique **l'agrégation en deux temps**
  d'`etape_catalogue` : d'abord une petite table des totaux par profil (pour
  appliquer le seuil), ensuite seulement les combinaisons des profils
  retenus — les profils sous le seuil ne sont jamais chargés en mémoire.
- **La conversion E669 se fait avant le seuil** (voir §4C).
- **La colonne `poids`** = l'effectif du PROFIL, répété sur toutes les lignes
  d'un même profil. Additionner `poids` sur le catalogue n'a donc aucun
  sens ; il faut d'abord dédupliquer au profil.

### 5b. Les dix tables de référence (ce que le tirage consomme)
`ref_das_aigu` (les candidats de complétion des longs, par strate fine),
`ref_das_chronique` + `ref_nb_chroniques` (les courts : la prévalence ET le
nombre de pathologies chroniques sont estimés sur les séjours LONGS — les
maladies chroniques appartiennent au patient, pas au séjour, et le
sous-codage des séjours courts ne doit pas être reproduit),
`ref_comp_diabete` (complications du diabète, .9 pénalisés),
`distribution_e660` (les classes d'IMC observées, pour répartir les E669
nus), `pivots_courts`, `v_admin_courts` / `v_admin_longs` (modes d'entrée,
de sortie, mode de prise en charge), et deux référentiels de mesure pour
l'aval Python : `referentiel_substitution_imprecis` (la future substitution
des codes « sans précision » — elle ne se fait PAS ici) et
`referentiel_paires_chroniques`.

### 5c. La sélection de campagne (quota_dp_fixe)
Priorité de doctrine : **la représentativité des diagnostics passe avant
celle des situations cliniques.**
- `X = budget de la population / nombre de DP` scénarios par DP ;
- `k = NB_LIGNES_PAR_DP` lignes distinctes tirées (probabilité
  proportionnelle au poids, sans remise), chacune déclinée en `X/k`
  variantes de complétion. Leçon du premier tirage d'essai (263 k produits
  pour 500 k visés) : à k=1, on demande trop de variantes à une seule
  strate et l'unicité en élimine la moitié — k=5 recommandé ;
- **plafonds par CLASSE** (accouchement normal, bébé normal — activités très
  standardisées) : chaque DP de la classe garde d'abord 1 représentant (ce
  principe prime sur le plafond), le surplus est réparti au poids ;
- **sous registre** : voir §4G.

### 5d. L'unicité souple
Les variantes d'une même ligne doivent porter des jeux de DAS différents. On
élimine les doublons **sans re-tirer** : le volume visé est un ordre de
grandeur, pas un engagement — le réalisé peut être en dessous, et le rapport
le chiffre. Conséquence pratique : une ligne et toutes ses variantes vivent
dans le MÊME paquet de tirage (le découpage se fait par lignes de sélection).

## 6. La robustesse : pourquoi « relancer la même commande » marche toujours

Quatre mécanismes, tous couverts par les tests :
1. **La reprise par fichiers** : chaque étape saute ce qui existe déjà sur le
   disque (comptes partiels, références, parts du catalogue, paquets de
   tirage, sélection). Après un plantage, on relance la même commande : seul
   le manquant est refait.
2. **Les garde-fous de compatibilité** : les fichiers compagnons (yaml)
   mémorisent les paramètres sous lesquels chaque produit a été fabriqué, et
   toute relance sous des paramètres différents s'arrête avec un message
   explicite. Jamais de mélange silencieux d'anciens et de nouveaux
   résultats. (C'est la famille d'erreurs « paramètres différents de la
   section figée » : le garde-fou fait son travail.)
3. **La reproductibilité** : une graine aléatoire globale, et des graines
   dérivées par paquet, par (population, lettre), par campagne. Une reprise
   partielle, ou un tirage réparti sur plusieurs sessions parallèles, donne
   strictement le même résultat qu'une exécution d'une traite.
4. **La discipline mémoire** : ne rapatrier que des comptes agrégés, le plus
   tard possible ; libérer aussitôt ; mesurer (`diagnostic_memoire.csv`) ;
   Restart R avant chaque étape lourde.

## 7. La règle d'or (pourquoi le code a cette allure)

Les requêtes vers la base (tout ce qui va d'un `atihble()` à un `collect()`
ou `compute()`) sont des **copies** des scripts v7, modifiées uniquement par
des écarts nommés et tracés au journal — parce que ce code n'est pas testable
en dehors de l'espace sécurisé, et qu'une réécriture « plus propre » non
testée est un risque, pas un progrès. C'est pourquoi on trouve du code v7
perfectible mais fonctionnel recopié tel quel, et pourquoi chaque écart a son
avant/après au journal. Tout le code **neuf** vit dans les helpers, testés.

## 8. Se repérer en pratique

- **Où en suis-je ?** → `etat_pipeline()` (l'état de chaque étape, fichiers
  présents / attendus, sans connexion).
- **Qui définit ce paramètre ?** → `grep -n "NOM_PARAM" config_v8.R`
- **Que contient ce fichier de sortie ?** → chaque parquet a son fichier
  compagnon yaml à côté ; commencer par le lire.
- **Ça a cassé** → le message des garde-fous dit quoi faire ; sinon la
  section correspondante de `MODIFICATIONS_V8.md` ; les questions Q1…Q40
  sont l'inventaire des cas limites déjà tranchés.
- **La RAM monte** → `memoire_session()`, puis Restart R.
- **« Objet introuvable »** → la session ne charge pas la bonne version du
  code : `git -C <PATH_PROJET> log --oneline -1`, puis Restart R.
- **Voir tourner le pipeline sans la base** → `Rscript demo/creer_base_demo.R`
  puis `Rscript demo/lancer_demo.R` (base SQLite fictive, sorties sous `demo/resultats/`) ;
  ou les notebooks eux-mêmes, chunk « Mode démo » en tête (`demo/session_demo.R`),
  vérifiés en CI par `demo/executer_notebook.R`.
- **« Racine du projet inconnue »** → `SCENARIOS_PMSI_PATH` dans l'environnement,
  ou `config_locale.R` à la racine (copier `config_locale.exemple.R`).
- **Données de la démo** → aléatoires, sans aucune validité épidémiologique :
  utiles pour lire les étapes et les livrables, jamais pour une conclusion.

## 9. Chronologie des chantiers (pour lire le journal)

1. v8 fusionné (courts + longs, 14 corrections des v7) → 2. une ligne par
séjour dans prep_data (écart B1-10) → 3. suppression de la branche
ambulatoire → 4. scission extraction / tirage, profils, comptes partiels,
paquets → 5. taille de paquets dynamique → 6. conversion E669 → 7. mémoire
15 GiB → 8. orchestration par étapes → 9. aval production (typologie,
catalogue en parts, quota par DP, index de tirage) → 10. finitions
exploitation → 11. campagnes (identifiants, registre, plafonds de classe,
recyclage) → 12. packaging GitHub + mode démo → 13. notebook campagnes, config
locale, démo dans les notebooks. Chaque chantier = une section du journal, avec ses questions.
