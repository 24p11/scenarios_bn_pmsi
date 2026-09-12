###############################################################################
# config_v8.R — configuration du pipeline scenarios_bn_pmsi (v8, industrialisation)
#
# Sourcé par extraction_associations_codes_v8.R, tirage_scenarios_v8.R et les tests.
# Toutes les constantes paramétrables ; aucun nombre magique dans les scripts.
# Structure : constantes communes -> bloc PROFIL -> surcharges individuelles ->
# surcharge externe optionnelle (SCENARIOS_PMSI_SURCHARGE) -> dérivés -> set.seed(SEED).
###############################################################################
`%+%` <- function(x, y) paste0(x, y)   # aussi défini dans utils.R (identique)

PATH_PROJET <- Sys.getenv("SCENARIOS_PMSI_PATH",
                          unset = "~/commun/projets_communs/DIM_siege/divers_projets/Scenario_crh_fictifs/")
if(!grepl("/$", PATH_PROJET)) PATH_PROJET <- paste0(PATH_PROJET, "/")
PATH_RESULTS        <- paste0(PATH_PROJET, "results/")
PATH_PAIRES_EXCLUES <- paste0(PATH_PROJET, "referentiels/exclusions_paires.yaml")

# Profil d'exécution : "diagnostic" (apport marginal de chaque (établissements, année),
# validation de la complétion sur petit volume) ou "production". Sélectionne le bloc PROFIL
# ci-dessous ; chaque valeur reste surchargeable individuellement après le bloc.
PROFIL <- Sys.getenv("SCENARIOS_PMSI_PROFIL", unset = "diagnostic")
if(!PROFIL %in% c("diagnostic", "production")) stop("PROFIL inconnu : " %+% PROFIL)

AN_REF         <- 26L               # année de référence (tables de référence, pivots courts)
SEED           <- 20260907
VERSION_SCRIPT <- "v8-industrialisation-1"   # écrit dans partiels_meta.yaml et les meta.yaml

SEUIL_PIVOT        <- 10            # divulgation : nb > SEUIL_PIVOT au niveau des pivots (§2.2)
SEUIL_REF_DAS      <- 20            # effectif min de codes candidats d'une strate de référence (§6.2)
SEUIL_REF_IMPRECIS <- 20            # export §7.5 : nb >= seuil
SEUIL_REF_PAIRES   <- 50            # export §7.6 : nb >= seuil

DUREE_COURTS    <- 0:2              # séjours courts : durée < 3
DUREE_LONGS     <- 3:100            # séjours longs
DUREE_MIN_REF   <- 3                # tables de référence DAS : séjours de durée > DUREE_MIN_REF (v7.2 l.445, §6.2)
NBDA_MAX        <- 25               # nbda %in% 1:NBDA_MAX pour les séjours longs
K_GRAINE_LONGS  <- 2                # nb de DAS réels en graine (§2.1)

NB_TIRAGES_COURTS         <- 3      # nb de variantes de DAS par pivot (séjours courts, ex-boucle nb_min:nb_max)
NB_VARIANTES_ADMIN_COURTS <- 2      # variantes d'habillage admin par scénario court (ex slice(1:2))
NB_VARIANTES_ADMIN_LONGS  <- NA     # NA = toutes les variantes (comportement v7.2)
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

# Export §7.5 : motif de repérage des libellés « sans précision »
MOTIF_IMPRECIS <- "sans précision|non précisé"

# Millésime de la table des niveaux de CMA (mco_diag_niveau, colonnes v20xx) selon
# l'année de données (v7.2 l.280-282). Utilisé partout à la place de v2025 (§5.8).
anseqta_de <- function(an){
  dplyr::case_when(an <= 17 ~ "21",
                   an > 17 & an <= 22 ~ "23",
                   TRUE ~ "25")
}
DATE_TAG <- format(Sys.Date(), "%Y%m%d")

# Produits de référence exportés par l'extraction (EXPORTS_DIR/<nom>.parquet) ; l'absence
# d'un produit déclenche sa (re)création. Les REFS_CHRONIQUES exigent prep_das_chronique.
NOMS_REFS <- c("ref_das_aigu", "ref_das_chronique", "ref_nb_chroniques", "ref_comp_diabete",
               "pivots_courts", "v_admin_courts", "v_admin_longs",
               "referentiel_substitution_imprecis", "referentiel_paires_chroniques")
REFS_CHRONIQUES <- c("ref_das_chronique", "ref_nb_chroniques", "referentiel_paires_chroniques")

## ---- Bloc PROFIL ----
if(PROFIL == "diagnostic"){
  ANS_HISTORIQUE     <- 17:AN_REF           # TOUTES les années
  TYPES_ETBS_LONGS   <- c("CHR/U", "CH")    # LES DEUX catégories (ordre v7.2 : CHR/U puis CH)
  MODE_SELECTION     <- "quota_dp"
  BUDGET_TOTAL_LONGS <- 1000L
  CHUNK_SIZE         <- 200L
  EXPORTS_DIR        <- paste0(PATH_RESULTS, "exports_diagnostic/")
} else {
  # À choisir d'après diagnostic_apports.csv (RUN.md, étape 2)
  ANS_HISTORIQUE     <- 17:AN_REF
  TYPES_ETBS_LONGS   <- c("CHR/U", "CH")
  MODE_SELECTION     <- "catalogue_complet"
  BUDGET_TOTAL_LONGS <- 10000000L
  CHUNK_SIZE         <- 2000L
  EXPORTS_DIR        <- paste0(PATH_RESULTS, "exports/")
}
PARTIELS_DIR        <- paste0(PATH_RESULTS, "partiels/")   # PARTAGÉ entre profils (cache inter-profils)
QUOTA_MIN_PAR_UNITE <- 5L        # mode quota_dp : plancher par type_unite présent au catalogue du DP
GARDER_CHUNKS       <- TRUE      # conserver les chunks de tirage après assemblage (reprise)
FORCER_REFS         <- FALSE     # TRUE : ignorer l'existence des refs et les recalculer (changement d'AN_REF, correction amont)

## ---- Surcharges individuelles (après le bloc PROFIL) ----
# Exemples : BUDGET_TOTAL_LONGS <- 100000L (palier production) ; ANS_HISTORIQUE <- 22:AN_REF
# Surcharge externe optionnelle : fichier R pointé par SCENARIOS_PMSI_SURCHARGE, évalué ici
# (tests, ou palier sans éditer ce fichier).
SURCHARGE_CONFIG <- Sys.getenv("SCENARIOS_PMSI_SURCHARGE", unset = "")
if(nzchar(SURCHARGE_CONFIG)) source(SURCHARGE_CONFIG, local = FALSE)

## ---- Dérivés (après surcharges) ----
if(!MODE_SELECTION %in% c("catalogue_complet", "quota_dp")) stop("MODE_SELECTION inconnu : " %+% MODE_SELECTION)
CHUNKS_DIR  <- paste0(EXPORTS_DIR, "chunks/")
ANSEQTA_REF <- anseqta_de(AN_REF)

# Valeurs effectives écrites dans les meta.yaml (PROFIL et tout ce qui dépend du profil ou
# d'une surcharge). Fonction pure : lit les variables dans `env`.
NOMS_CONFIG_META <- c("PROFIL", "VERSION_SCRIPT", "AN_REF", "ANS_HISTORIQUE", "TYPES_ETBS_LONGS", "SEED",
                      "SEUIL_PIVOT", "SEUIL_REF_DAS", "SEUIL_REF_IMPRECIS", "SEUIL_REF_PAIRES",
                      "DUREE_COURTS", "DUREE_LONGS", "DUREE_MIN_REF", "NBDA_MAX", "K_GRAINE_LONGS",
                      "NB_TIRAGES_COURTS", "NB_VARIANTES_ADMIN_COURTS", "NB_VARIANTES_ADMIN_LONGS",
                      "MODE_SELECTION", "BUDGET_TOTAL_LONGS", "QUOTA_MIN_PAR_UNITE", "CHUNK_SIZE",
                      "GARDER_CHUNKS", "FORCER_REFS", "EXPORTS_DIR", "PARTIELS_DIR", "PIVOTS_LONGS")
valeurs_effectives_config <- function(env = globalenv()){
  v <- mget(NOMS_CONFIG_META, envir = env)
  lapply(v, function(x) if(is.numeric(x) && length(x) > 1) as.integer(x) else x)
}

set.seed(SEED)

