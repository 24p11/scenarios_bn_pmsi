###############################################################################
# config.R — configuration du pipeline scenarios_bn_pmsi (v8, industrialisation)
#
# Sourcé par extraction.R, tirage.R et les tests.
# TROIS NIVEAUX DE PARAMÈTRES, ordre de chargement :
#   1. config.R (ce fichier)   = la DOCTRINE et les DÉFAUTS — versionné, public ; aucune valeur de poste (chemin
#                                 absolu, pschema, identifiant) ni valeur « courante » d'exploitation ;
#   2. config_locale.R         = le POSTE (racine du dépôt, PATH_RESULTS, pschema) — local, gitignoré, modèle
#                                 config_locale.exemple.R ; sourcé AVANT le bloc PROFIL ;
#   3. SCENARIOS_PMSI_SURCHARGE = la DÉCISION D'EXPLOITATION : campagne.R (chunk ouvrir_campagne de 02_campagne.Rmd)
#                                 OU palier.R (chunk palier_surcharge de 03_outils_maintenance.Rmd) — exclusifs ; locaux, gitignorés ; sourcés
#                                 après le bloc PROFIL ; la surcharge démo (demo/session_demo.R) est le troisième cas ;
#   puis les dérivés (chemins) et les vérifications, enfin set.seed(SEED).
# Toutes les constantes paramétrables ; aucun nombre magique dans les scripts.
###############################################################################
`%+%` <- function(x, y) paste0(x, y)   # aussi défini dans utils.R (identique)

# Racine du projet : variable d'environnement SCENARIOS_PMSI_PATH, sinon config_locale.R (racine du
# dépôt, IGNORÉ par git : valeurs propres au poste — SCENARIOS_PMSI_PATH, pschema, ... ; modèle :
# config_locale.exemple.R). Aucun chemin personnel n'est versionné (lot « notebook campagnes », §6).
MESSAGE_PATH_PROJET_ABSENT <- paste0("Racine du projet inconnue : définissez (1) la variable d'environnement SCENARIOS_PMSI_PATH ",
  "(Sys.setenv(SCENARIOS_PMSI_PATH = \"<racine du dépôt>\") avant le source), ou (2) un fichier config_locale.R à la racine du dépôt ",
  "(copiez config_locale.exemple.R, non versionné) contenant SCENARIOS_PMSI_PATH <- \"<racine du dépôt>\".")
PATH_PROJET <- Sys.getenv("SCENARIOS_PMSI_PATH", unset = "")
if(exists("PATH_RESULTS", inherits = FALSE)) rm(PATH_RESULTS)   # jamais hérité d'un source précédent : posé par config_locale.R, une surcharge, ou le défaut
CONFIG_LOCALE <- if(nzchar(PATH_PROJET)) file.path(PATH_PROJET, "config_locale.R") else "config_locale.R"   # sourcé AVANT le bloc PROFIL
if(file.exists(CONFIG_LOCALE)){
  source(CONFIG_LOCALE, local = FALSE)
  if(!nzchar(PATH_PROJET) && exists("SCENARIOS_PMSI_PATH")) PATH_PROJET <- SCENARIOS_PMSI_PATH
}
if(!nzchar(PATH_PROJET)) stop(MESSAGE_PATH_PROJET_ABSENT, call. = FALSE)
if(!grepl("/$", PATH_PROJET)) PATH_PROJET <- paste0(PATH_PROJET, "/")
# Répertoire de travail (résultats) : config_locale.R (ou une surcharge) peut le poser — PATH_RESULTS <- "<nouveau
# répertoire>/", vide au départ, peuplé par etape_reorganiser — ; sinon <racine>/results/. Arborescence : bloc CHEMINS.
if(!exists("PATH_RESULTS", inherits = FALSE)) PATH_RESULTS <- paste0(PATH_PROJET, "results/")
CHEMINS_SURCHARGES <- list()   # soupape : un magasin partagé peut être détourné pour un profil (bloc PROFIL ou surcharge), ex. CHEMINS_SURCHARGES$references <- "/ailleurs/10_references/"
PATH_PAIRES_EXCLUES <- paste0(PATH_PROJET, "referentiels/exclusions_paires.yaml")
PATH_TYPOLOGIE      <- paste0(PATH_PROJET, "referentiels/typologie_sejours.yaml")   # typologie DPEC / TPEC (chantier aval)

# Profil d'exécution : "diagnostic" (apport marginal de chaque (établissements, année),
# validation de la complétion sur petit volume) ou "production". Sélectionne le bloc PROFIL
# ci-dessous ; chaque valeur reste surchargeable individuellement après le bloc.
PROFIL <- Sys.getenv("SCENARIOS_PMSI_PROFIL", unset = "diagnostic")
if(!PROFIL %in% c("diagnostic", "production")) stop("PROFIL inconnu : " %+% PROFIL)

AN_REF         <- 26L               # année de référence (tables de référence des longs, photographie v_admin)
ANS_COURTS     <- NULL              # années du TIRABLE courts (pivots + refs de saturation courts + v_admin_courts, CUMUL des comptes) ;
                                    # NULL = AN_REF (comportement historique) ; ex. surcharge ANS_COURTS <- c(24L, 25L, 26L) — les ids des
                                    # pivots sont des hash de contenu : stables sous extension (pivots existants inchangés au registre)
SEED           <- 20260907
VERSION_SCRIPT <- "v8-industrialisation-1"   # écrit dans 00_partiels/_meta.yaml et les métas

SEUIL_PIVOT        <- 10            # divulgation : nb > SEUIL_PIVOT au niveau des pivots (§2.2)
SEUIL_REF_DAS      <- 20            # effectif min de codes candidats d'une strate de référence (§6.2)
SEUIL_REF_IMPRECIS <- 20            # export §7.5 : nb >= seuil
SEUIL_REF_PAIRES   <- 50            # export §7.6 : nb >= seuil

DUREE_COURTS    <- 0:2              # séjours courts : durée < 3
DUREE_LONGS     <- 3:100            # séjours longs
DUREE_MIN_REF   <- 3                # tables de référence DAS : séjours de durée > DUREE_MIN_REF (v7.2 l.445, §6.2)
NBDA_MAX        <- 25               # nbda %in% 1:NBDA_MAX pour les séjours longs
K_GRAINE_LONGS  <- 2                # nb de DAS réels en graine (§2.1)

NB_TIRAGES_COURTS         <- 3      # héritage v7 (corpus courts FIXE : 3 variantes par pivot, adopté en C1) ; le tirage courts PAR CAMPAGNE
                                    # est piloté par le budget (NB_CRH_CIBLE_COURTS / RATIO_COURTS), réparti sur les pivots au poids
NB_VARIANTES_ADMIN_COURTS <- 1L     # DÉFAUT (Q76 actée, doctrine unifiée avec les longs) : UN scénario court = UNE tenue admin tirée pondérée ;
                                    # N > 1 = paramètre de campagne (N tenues sans remise, id_scenario suffixé -aN) ; 2 = comportement v7.1.2 (ex slice(1:2))
NB_VARIANTES_ADMIN_LONGS  <- 1L     # DÉFAUT (Q74 actée) : UN scénario = UNE tenue admin tirée pondérée par les effectifs ; N > 1 = paramètre de
                                    # campagne (campagne.R) : N tenues sans remise, id_scenario suffixé -a2..-aN ; NA = toutes les combinaisons
                                    # (comportement v7.2, accepté). Même nombre aux niveaux de repli (âge exact -> cage -> mode_hospit × cage × racine).
AGE_MAX_OUVERT            <- 95     # borne haute de la classe ouverte "[80-[" pour le tirage d'âge
# (NB_TIRAGES_LONGS et MAX_SCENARIOS_LONGS supprimés : remplacés par MODE_SELECTION /
#  BUDGET_TOTAL_LONGS du bloc PROFIL.)

# Cible dégradée du nombre de DAS chroniques par classe d'âge (bornes incluses),
# utilisée quand la strate (cage, sexe) de ref_nb_chroniques est vide (§6.2).
CIBLES_NB_CHRONIQUES <- list(
  "[0-1["   = c(0, 0), "[1-5["   = c(0, 0), "[5-10["  = c(0, 0),
  "[10-15[" = c(0, 0), "[15-18[" = c(0, 0),
  "[18-30[" = c(0, 1), "[30-40[" = c(0, 1),
  "[40-50[" = c(1, 2), "[50-60[" = c(1, 2),
  "[60-70[" = c(2, 3),
  "[70-80[" = c(3, 5), "[80-["   = c(3, 5)
)

# Établissements (TYPES_ETBS_LONGS : voir bloc PROFIL)
TYPE_ETBS_REF_DIABETE <- "CHR/U"            # v7.1.2 l.196 / v7.2 l.457

# GHM — listes obstétriques v7.2 l.518-524 : reprises telles quelles. Non utilisées dans le
# tirage v8 (le filtre de test nb>5000 / sample_n(3000) disparaît, §2.5) ; conservées
# pour l'allocation en aval.
GHM_ACC_NORMAL    <- c("14C03A", "14C07A", "14C08A", "14Z11A", "14Z12A",
                       "14Z13A", "14Z13T", "14Z14A", "14Z14T")
RACINES_ACC_PATHO <- c("14C07", "14C08", "14Z10", "14Z11", "14Z12", "14Z13", "14Z14")
GHM_BB_NORMAL     <- c("15M05A", "15M06A", "15M07A", "15M08A", "15M09A",
                       "15M10A", "15M11A", "15M13A", "15M14A")
RACINES_BB_MED    <- c("15M05", "15M06", "15M07", "15M08", "15M09",
                       "15M10", "15M11", "15M13", "15M14")

# Types d'autorisation d'unité (v7.2 l.20-22 ; les définitions l.16-19 étaient écrasées)
TYPEAUT_UHCD <- c("07A", "07B")
TYPEAUT_SC   <- c("01A", "01B", "13A", "13B", "03A", "03B")
TYPEAUT_USI  <- c("02E", "02A")             # non utilisé (repris de v7.2)

# Priorité des unités pour réduire prep_data à UNE ligne par séjour (écart B1-10, Q5 résolue).
# Documente l'ordre codé en littéral dans le case_when de prep_data (chaîne dbplyr).
# UHCD impérativement dernier : sinon un séjour multi-RUM passé par l'UHCD serait réduit à sa
# ligne UHCD puis supprimé par le filtre (nbrum == 1 & type_unite == "UHCD") | type_unite != "UHCD".
PRIORITE_TYPE_UNITE <- c("SC" = 1L, "SC-NEONAT" = 2L, "NEONAT" = 3L, "GERIATRIE" = 4L,
                         "HC" = 5L, "HP" = 6L, "UHCD" = 7L)

# Pénalisation des effectifs des codes diabète .9 (v7.1.2 l.208-213)
CAGE_PED          <- c("[1-5[", "[10-15[", "[5-10[", "[0-1[")
CAGE_AGES         <- c("[50-60[", "[60-70[", "[70-80[", "[80-[")
PENALITE_9_AGES   <- 0.2
PENALITE_9_AUTRES <- 0.5

# Pivots par branche
PIVOTS_COURTS      <- c("mode_hospit", "sexe", "cage", "ghm2", "diag2", "duree")        # v7.1.2 l.217
PIVOTS_LONGS       <- c("mode_hospit", "sexe", "age", "cage", "racine", "ghm2", "diabete", "hta",
                        "diag2", "nbda", "type_unite", "prep_sc")                       # v7.2 l.483 + §2.8
PIVOTS_LONGS_SEUIL <- setdiff(PIVOTS_LONGS, "nbda")                                     # v7.2 l.528-529
COLS_ADMIN         <- c("mode_entree", "mode_sortie", "mdp")                            # colonnes d'habillage
# Chantier « type_unite côté courts, via l'habillage » (journal §26) : besoin aval (la substitution des DP imprécis exempte
# les UHCD, qui vivent surtout chez les courts). type_unite N'ENTRE PAS dans PIVOTS_COURTS ni dans la recette id_courts_v1
# (figées : un pivot de plus changerait tous les identifiants, orphelinerait C1 au registre, casserait le recyclage) ; il est
# une colonne de plus de la photographie ref_v_admin_courts, tirée AVEC la tenue admin (même tirage pondéré, même repli).
# Clé du magasin 10_references : un magasin photographié sans type_unite est en écart -> FORCER_REFS (le méta l'impose).
COLS_ADMIN_COURTS  <- c(COLS_ADMIN, "type_unite")                                       # colonnes apportées à l'habillage des COURTS
# Clés de l'habillage admin des longs (chantier « courts en campagnes + habillage robuste », défaut trouvé en revue clinique) :
# nbda SORT des clés (ref_v_admin_longs sans nbda : la table rétrécit, les variantes se cumulent entre valeurs de nbda) ;
# repli hiérarchique (helpers K2) : (0) ces 6 clés -> (1) cage au lieu de l'âge exact -> (2) mode_hospit × cage × racine.
# Clé du magasin 10_references : un magasin photographié avec nbda est en écart -> FORCER_REFS (le méta l'impose de lui-même).
CLES_ADMIN_LONGS   <- c("mode_hospit", "sexe", "age", "cage", "ghm2", "diag2")

# Export §7.5 : motif de repérage des libellés « sans précision »
MOTIF_IMPRECIS <- "sans précision|non précisé"

# Conversion E669 -> E660 (doctrine : E669x = erreur de codage, cf. helpers section C).
# Post-collect uniquement, sur toutes les surfaces (diag2, graines, DAS, référentiels).
# Les partiels restent en codes bruts : la conversion s'applique à la ré-agrégation et
# n'est PAS une clé de verifier_partiels_meta.
CONVERSION_E669  <- TRUE     # les deux profils ; écrit dans meta.yaml
BARE_E669_DEFAUT <- "0"      # ultime repli pour un E669 nu sans distribution E660x observée (-> E6600)

# Mémoire (plateforme sécurisée : 15 GiB). Doctrine : ne collecter que des agrégats, le plus
# tard possible, libérer immédiatement ; le niveau séjour ne quitte jamais la base.
COLLECT_PAR_MORCEAUX <- TRUE  # prep_scenarios2 : un collect par modalité de cage depuis la table temporaire top-k
SEUIL_ALERTE_GO      <- 10    # diagnostic_memoire.csv : avertissement visible si le pic gc() dépasse ce seuil (Go)
# Recouvrement entre deux partiels d'une même catégorie d'établissements (etbs, anA, anB) :
# mesure de déduplication pour la décision de périmètre (recouvrement.csv). Extensible.
PAIRES_RECOUVREMENT  <- list(c("CHR/U", 24, 25))

# Millésime de la table des niveaux de CMA (mco_diag_niveau, colonnes v20xx) selon
# l'année de données (v7.2 l.280-282). Utilisé partout à la place de v2025 (§5.8).
anseqta_de <- function(an){
  dplyr::case_when(an <= 17 ~ "21",
                   an > 17 & an <= 22 ~ "23",
                   TRUE ~ "25")
}

# Produits de référence exportés par l'extraction (10_references/<nom>.parquet) ; l'absence
# d'un produit déclenche sa (re)création. Les REFS_CHRONIQUES exigent prep_das_chronique.
# Ordre = ordre de calcul : ref_das_chronique puis distribution_e660 (calculée sur ses comptes
# bruts) avant toute ref convertie.
# Préfixe unique ref_ (chantier « livrable unique + nommage ») : le nom logique = le nom de fichier (ref_<nom>.parquet).
NOMS_REFS <- c("ref_das_chronique", "ref_distribution_e660", "ref_das_aigu", "ref_nb_chroniques", "ref_comp_diabete",
               "ref_pivots_courts", "ref_v_admin_courts", "ref_v_admin_longs",
               "ref_substitution_imprecis", "ref_paires_chroniques")
REFS_CHRONIQUES <- c("ref_das_chronique", "ref_distribution_e660", "ref_nb_chroniques", "ref_paires_chroniques")
# Refs construites sur ANS_COURTS (le tirable courts et ses refs de saturation : même périmètre, cohérence du magasin) ;
# les autres restent sur AN_REF. ref_pivots_courts est écrit dans 30_courts/ (le tirable = catalogue des courts).
REFS_COURTS <- c("ref_pivots_courts", "ref_v_admin_courts", "ref_das_chronique", "ref_distribution_e660", "ref_nb_chroniques")
# STATUT des références (micro-lot « arbitrages Q86-Q92 », Q88 actée, journal §26.7) : « exportable » = agrégat seuillé pouvant
# quitter la plateforme sécurisée ; « interne » = consommée par le pipeline SUR la plateforme, jamais exportée — le DÉFAUT de
# toute référence non explicitement marquée exportable (dont les photographies v_admin, NON seuillées : un seuillage casserait la
# couverture de l'habillage ; c'est leur statut interne qui rend cela sûr). Le livrable lui-même est exportable. Le statut est
# écrit au méta du magasin 10_references (champ statut, par référence) ; il ne change pas le contenu des fichiers.
REFS_EXPORTABLES <- c("ref_substitution_imprecis")
statut_ref  <- function(nom) ifelse(nom %in% REFS_EXPORTABLES, "exportable", "interne")
statuts_refs <- function(noms) as.list(stats::setNames(statut_ref(noms), noms))   # liste nommée prête pour le méta yaml

## ---- Bloc PROFIL ----
if(PROFIL == "diagnostic"){
  ANS_HISTORIQUE     <- 17:AN_REF           # TOUTES les années
  TYPES_ETBS_LONGS   <- c("CHR/U", "CH")    # LES DEUX catégories (ordre v7.2 : CHR/U puis CH)
  MODE_SELECTION     <- "quota_dp"          # avec remise (conservé pour le diagnostic)
  NB_CRH_CIBLE       <- 1000L               # volume de la campagne (ex BUDGET_TOTAL_LONGS)
} else {
  # À choisir d'après diagnostic_apports.csv (RUN.md, étape 2)
  ANS_HISTORIQUE     <- 17:AN_REF
  TYPES_ETBS_LONGS   <- c("CHR/U", "CH")
  MODE_SELECTION     <- "quota_dp_fixe"     # k lignes par DP, variantes déduites (catalogue_complet retiré pour ce corpus)
  NB_CRH_CIBLE       <- 500000L             # DÉFAUT : volume de la campagne, un ORDRE DE GRANDEUR (la valeur de la campagne vient de campagne.R)
  # CHEMINS_SURCHARGES$references <- "..."  # exemple de soupape : magasin partagé détourné pour ce seul profil (non utilisé par défaut)
}
QUOTA_MIN_PAR_UNITE <- 5L        # mode quota_dp : plancher par type_unite présent au catalogue du DP
# Mode quota_dp_fixe (production par campagnes ; doctrine : représentativité des DP avant celle des
# situations cliniques, la diversité des contextes se reconstituant ENTRE les campagnes).
NB_LIGNES_PAR_DP <- 1L           # DÉFAUT : k lignes distinctes tirées par DP (sans remise), variantes déduites (campagne.R pour l'ajuster)
POPULATIONS <- list(              # partition EXACTE des modalités de cage (vérifiée, stop sinon)
  pediatrie = c("[0-1[", "[1-5[", "[5-10[", "[10-15[", "[15-18["),
  adulte    = c("[18-30[", "[30-40[", "[40-50[", "[50-60[", "[60-70[", "[70-80[", "[80-[")
)
# Plafond du TOTAL d'une classe DPEC, par population (amendement Q33 : le plafond par (DP × DPEC) était
# inopérant sur les classes standardisées — 25 000 accouchements normaux / 15 000 bébés normaux constatés).
# Chaque DP de la classe reçoit d'abord 1 représentant (prime sur le plafond), le surplus est réparti au poids.
PLAFONDS_DPEC <- list("Accouchement normal mère" = 100L, "Bébé normal" = 100L)   # extensible
# Campagnes itératives (registre des tirages, append-only). DÉFAUTS documentés : la décision d'exploitation d'une
# campagne (CAMPAGNE, NB_CRH_CIBLE, NB_LIGNES_PAR_DP, REGISTRE_ACTIF, PLAFONDS_DPEC ajustés) ne s'édite PAS ici mais
# dans campagne.R, écrit depuis le notebook 02_campagne.Rmd (chunk ouvrir_campagne) et activé par SCENARIOS_PMSI_SURCHARGE.
CAMPAGNE       <- "C1"    # DÉFAUT : identifiant court de la campagne, OBLIGATOIRE, tracé partout (sélection, livrable, registre)
REGISTRE_ACTIF <- TRUE    # DÉFAUT : FALSE = comportement sans registre (tests / diagnostic / palier)
# Séjours courts EN CAMPAGNE (même statut que les longs dans le corpus ; tirage par campagne à variantes nouvelles, registre commun).
RATIO_COURTS        <- 1.0   # DÉFAUT — PROVISOIRE, à calibrer avec l'équipe apprentissage (point ouvert du consortium) : budget courts = RATIO × volume longs de la campagne
NB_CRH_CIBLE_COURTS <- NULL  # DÉFAUT : NULL = RATIO_COURTS × volume longs (attendu de la sélection) ; un entier impose un budget courts absolu (campagne.R)
LOT_CHUNKS_FINALISATION <- 10L   # finalisation en flux : nb de chunks relus par lot
SEUIL_EXPORT_MONOFICHIER <- 2000000L   # au-delà, l'export final reste en parts (pas de monofichier)
# Chunking DYNAMIQUE (les deux profils) : la taille des chunks est dimensionnée par les données,
# taille_chunk(n) = max(CHUNK_SIZE_MIN, ceiling(n / NB_CHUNKS_MAX)) -> au plus NB_CHUNKS_MAX chunks
# par tirage, jamais de chunks minuscules. CHUNK_SIZE_FIXE non-NA court-circuite le calcul.
# Reprise : mêmes n / chunk_size / seed_base que le sidecar <prefixe>_chunks_meta.yaml, sinon stop().
NB_CHUNKS_MAX   <- 50L           # borne haute du nombre de chunks par tirage
CHUNK_SIZE_MIN  <- 500L          # plancher : en dessous, moins de chunks que NB_CHUNKS_MAX
CHUNK_SIZE_FIXE <- NA_integer_   # surcharge manuelle : si non-NA, court-circuite le calcul
GARDER_CHUNKS       <- TRUE      # conserver les chunks de tirage après assemblage (reprise)
FORCER_REFS         <- FALSE     # TRUE : recalculer les refs (magasin partagé 10_references/) malgré un _meta.yaml en écart
FORCER_PARTIELS     <- FALSE     # TRUE : vider et recalculer les partiels (magasin partagé 00_partiels/) malgré un _meta.yaml en écart
FORCER_CATALOGUE    <- FALSE     # TRUE : régénérer le catalogue (magasin partagé 20_catalogue/) malgré un _meta.yaml en écart
FORCER_COURTS       <- FALSE     # TRUE : re-tirer les séjours courts (magasin partagé 30_courts/) malgré un _meta.yaml en écart
# Livrable unique par campagne (<profil>/60_export_final/scenarios_<CAMPAGNE>.parquet) : longs + courts en union de schémas
SEUIL_MONOFICHIER   <- 5000000L  # au-delà (lignes), le livrable est écrit en parts scenarios_<CAMPAGNE>/part_XXXX.parquet
NB_REVUE            <- 50L       # échantillon de revue tiré du livrable unifié ...
PART_REVUE_COURTS   <- 0.5       # ... dont cette proportion de séjours courts (le reste : longs)

## ---- Surcharges individuelles (après le bloc PROFIL) ----
# Exemples : BUDGET_TOTAL_LONGS <- 100000L (palier production) ; ANS_HISTORIQUE <- 22:AN_REF
# Surcharge externe optionnelle : fichier R pointé par SCENARIOS_PMSI_SURCHARGE, évalué ici
# (tests, ou palier sans éditer ce fichier).
SURCHARGE_CONFIG <- Sys.getenv("SCENARIOS_PMSI_SURCHARGE", unset = "")
if(nzchar(SURCHARGE_CONFIG)) source(SURCHARGE_CONFIG, local = FALSE)

## ---- Dérivés (après surcharges) ----
if(!MODE_SELECTION %in% c("catalogue_complet", "quota_dp", "quota_dp_fixe")) stop("MODE_SELECTION inconnu : " %+% MODE_SELECTION)
if(!is.character(CAMPAGNE) || !nzchar(CAMPAGNE) || grepl("[^A-Za-z0-9_-]", CAMPAGNE)) stop("CAMPAGNE : identifiant court obligatoire ([A-Za-z0-9_-]) : " %+% CAMPAGNE)
if(!exists("BUDGET_TOTAL_LONGS")) BUDGET_TOTAL_LONGS <- NB_CRH_CIBLE   # alias de compatibilité (anciens scripts / surcharges)
if(is.null(ANS_COURTS)) ANS_COURTS <- AN_REF
ANS_COURTS <- sort(unique(as.integer(ANS_COURTS)))
if(!is.numeric(RATIO_COURTS) || length(RATIO_COURTS) != 1 || is.na(RATIO_COURTS) || RATIO_COURTS <= 0) stop("RATIO_COURTS : nombre > 0 attendu", call. = FALSE)
if(!is.null(NB_CRH_CIBLE_COURTS) && (!is.numeric(NB_CRH_CIBLE_COURTS) || length(NB_CRH_CIBLE_COURTS) != 1 || is.na(NB_CRH_CIBLE_COURTS) || NB_CRH_CIBLE_COURTS < 1)) stop("NB_CRH_CIBLE_COURTS : NULL ou entier >= 1 attendu", call. = FALSE)
if(length(NB_VARIANTES_ADMIN_LONGS) != 1 || (!is.na(NB_VARIANTES_ADMIN_LONGS) && (!is.numeric(NB_VARIANTES_ADMIN_LONGS) || NB_VARIANTES_ADMIN_LONGS < 1))) stop("NB_VARIANTES_ADMIN_LONGS : entier >= 1 (tenues admin par scénario) ou NA (toutes, v7.2) attendu", call. = FALSE)
if(length(NB_VARIANTES_ADMIN_COURTS) != 1 || is.na(NB_VARIANTES_ADMIN_COURTS) || !is.numeric(NB_VARIANTES_ADMIN_COURTS) || NB_VARIANTES_ADMIN_COURTS < 1) stop("NB_VARIANTES_ADMIN_COURTS : entier >= 1 (tenues admin par scénario court) attendu", call. = FALSE)
ANSEQTA_REF <- anseqta_de(AN_REF)

## ---- CHEMINS : bloc UNIQUE de l'arborescence par étapes (après surcharges ; aucune concaténation ailleurs) ----
# Magasins PARTAGÉS entre profils : tout objet qui ne dépend que de paramètres, pas du profil (chaque magasin porte un
# _meta.yaml avec les paramètres qui le définissent, vérifié à chaque chargement : verifier_magasin). Chaque chemin de
# magasin partagé est surchargeable par profil via CHEMINS_SURCHARGES (soupape, non utilisée par défaut).
if(!grepl("/$", PATH_RESULTS)) PATH_RESULTS <- paste0(PATH_RESULTS, "/")
chemin_magasin <- function(cle, defaut) if(!is.null(CHEMINS_SURCHARGES[[cle]])) sub("/*$", "/", CHEMINS_SURCHARGES[[cle]]) else paste0(PATH_RESULTS, defaut)
DIR_PARTIELS    <- chemin_magasin("partiels",    "00_partiels/")     # cache d'extraction                         [PARTAGÉ]
DIR_REFERENCES  <- chemin_magasin("references",  "10_references/")   # les 10 ref_*.parquet + _meta.yaml           [PARTAGÉ]
DIR_CATALOGUE_M <- chemin_magasin("catalogue",   "20_catalogue/")    # catalogue_longs_seuil/ (parts + _meta.yaml) [PARTAGÉ]
DIR_COURTS      <- chemin_magasin("courts",      "30_courts/")       # le TIRABLE courts : ref_pivots_courts.parquet + _meta.yaml (ANS_COURTS, seuils) ;
                                                                     # + corpus courts historique (scenarios_courts.parquet, adoption C1)   [PARTAGÉ]
DIR_DIAGNOSTICS <- chemin_magasin("diagnostics", "90_diagnostics/")  # apports, recouvrement ; mémoire par profil  [PARTAGÉ]
# PAR PROFIL (production/ ou diagnostic/)
DIR_PROFIL       <- paste0(PATH_RESULTS, PROFIL, "/")
DIR_CAMPAGNES    <- paste0(DIR_PROFIL, "40_campagnes/")     # <CAMPAGNE>/selection, chunks, habille (transitoires)
DIR_REGISTRE_M   <- paste0(DIR_PROFIL, "50_registre/")      # registre_tirages/ (permanent, JAMAIS vidé)
DIR_EXPORT_FINAL <- paste0(DIR_PROFIL, "60_export_final/")  # scenarios_<C>.parquet + _meta, rapport_<C>.txt, revue, top30
MAGASINS_PARTAGES <- c(partiels = DIR_PARTIELS, references = DIR_REFERENCES, catalogue = DIR_CATALOGUE_M, courts = DIR_COURTS, diagnostics = DIR_DIAGNOSTICS)

# Valeurs effectives écrites dans les meta.yaml (PROFIL et tout ce qui dépend du profil ou
# d'une surcharge). Fonction pure : lit les variables dans `env`.
NOMS_CONFIG_META <- c("PROFIL", "VERSION_SCRIPT", "AN_REF", "ANS_COURTS", "ANS_HISTORIQUE", "TYPES_ETBS_LONGS", "SEED",
                      "SEUIL_PIVOT", "SEUIL_REF_DAS", "SEUIL_REF_IMPRECIS", "SEUIL_REF_PAIRES",
                      "DUREE_COURTS", "DUREE_LONGS", "DUREE_MIN_REF", "NBDA_MAX", "K_GRAINE_LONGS",
                      "NB_TIRAGES_COURTS", "NB_VARIANTES_ADMIN_COURTS", "NB_VARIANTES_ADMIN_LONGS", "PIVOTS_COURTS", "CLES_ADMIN_LONGS", "COLS_ADMIN_COURTS", "RATIO_COURTS", "NB_CRH_CIBLE_COURTS",
                      "MODE_SELECTION", "NB_CRH_CIBLE", "NB_LIGNES_PAR_DP", "CAMPAGNE", "REGISTRE_ACTIF", "QUOTA_MIN_PAR_UNITE", "NB_CHUNKS_MAX", "CHUNK_SIZE_MIN", "CHUNK_SIZE_FIXE",
                      "GARDER_CHUNKS", "FORCER_REFS", "PIVOTS_LONGS",
                      "CONVERSION_E669", "BARE_E669_DEFAUT", "COLLECT_PAR_MORCEAUX", "SEUIL_ALERTE_GO")
valeurs_effectives_config <- function(env = globalenv()){
  v <- mget(NOMS_CONFIG_META, envir = env)
  lapply(v, function(x) if(is.numeric(x) && length(x) > 1) as.integer(x) else x)
}

set.seed(SEED)

