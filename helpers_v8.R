###############################################################################
# helpers_v8.R — helpers purs du pipeline scenarios_bn_pmsi (v8, industrialisation)
#
# Sourcé par extraction_associations_codes_v8.R, tirage_scenarios_v8.R et les tests.
# Règles : aucune variable globale implicite (toute table de référence est un argument),
# aucun `<<-`, dépendances limitées à dplyr/tidyr/tibble/stringr/purrr/base (+ arrow via
# les arguments `ecrire`/`lire` de pmap_chunks). Ces fonctions masquent les versions
# homonymes de utils.R (retro_code_diabete, get_codes_diabete_from_neo).
# Partie A : helpers de tirage (ex-section 4 du v8 mono-fichier, déplacée telle quelle).
# Partie B : industrialisation (chunking, sélection, résolution des besoins, livrables).
###############################################################################
`%+%` <- function(x, y) paste0(x, y)

## ---- A. Helpers de tirage (ex-section 4) ----

# Classes d'âge (même découpage que prep_data, v7.1.2 l.53-66 / l.238-251)
decoupe_cage <- function(age){
  dplyr::case_when(
    age < 1 ~ "[0-1[",
    age < 5 ~ "[1-5[",
    age < 10 ~ "[5-10[",
    age < 15 ~ "[10-15[",
    age < 18 ~ "[15-18[",
    age < 30 ~ "[18-30[",
    age < 40 ~ "[30-40[",
    age < 50 ~ "[40-50[",
    age < 60 ~ "[50-60[",
    age < 70 ~ "[60-70[",
    age < 80 ~ "[70-80[",
    TRUE ~ "[80-["
  )
}

# Tirage d'un âge (scalaire) dans une classe semi-ouverte "[a-b[" -> a:(b-1) ;
# classe ouverte "[a-[" -> a:age_max. Libellé inconnu -> NA (§5.6).
sample_age_ligne <- function(cage, age_max = 95){
  cage <- as.character(cage)
  if(length(cage) != 1 || is.na(cage)) return(NA_integer_)
  bornes <- as.integer(unlist(regmatches(cage, gregexpr("[0-9]+", cage))))
  if(length(bornes) == 0) return(NA_integer_)
  if(length(bornes) == 1) bornes <- c(bornes, as.integer(age_max) + 1L)
  if(bornes[2] <= bornes[1]) return(bornes[1])
  v <- seq.int(bornes[1], bornes[2] - 1L)
  v[sample.int(length(v), 1)]
}

# Version vectorisée : un tirage indépendant par ligne (§5.6)
sample_age <- function(cage, age_max = 95){
  vapply(as.character(cage), sample_age_ligne, integer(1), age_max = age_max, USE.NAMES = FALSE)
}

# Dédoublonnage à la catégorie 3 caractères (au plus un code par substr(code,1,3), premier
# arrivé conservé) + exclusions par paires de préfixes (§6.4). La graine et les codes
# doctrine doivent être passés en tête de `codes`.
dedup_categorie <- function(codes, paires_exclues = list()){
  codes <- as.character(codes)
  codes <- codes[!is.na(codes) & nzchar(codes)]
  gardes <- character(0)
  for(x in codes){
    if(substr(x, 1, 3) %in% substr(gardes, 1, 3)) next
    exclu <- FALSE
    for(p in paires_exclues){
      p <- as.character(p)
      if(length(p) != 2) stop("exclusions_paires : chaque entrée doit être une paire [prefixeA, prefixeB]")
      if((startsWith(x, p[1]) && any(startsWith(gardes, p[2]))) ||
         (startsWith(x, p[2]) && any(startsWith(gardes, p[1])))){
        exclu <- TRUE
        break
      }
    }
    if(exclu) next
    gardes <- c(gardes, x)
  }
  gardes
}

# Néo-code diabète à partir d'un code (DP ou DAS) ; `defaut` sinon (v7.2 l.341-344)
neo_code_de_diag <- function(diag, code_did, code_dnid_ins, code_dnid, defaut = "N"){
  dplyr::case_when(diag %in% code_dnid_ins ~ "E11i",
                   diag %in% code_dnid ~ "E11ni",
                   diag %in% code_did ~ "E10",
                   TRUE ~ defaut)
}

# Rétro-codage néo-code + complication -> code CIM-10 (utils.R l.336-342).
# 5e caractère : "0" = insulinotraité (E11i, cf. code_dnid_ins = E1120...),
# "8" = non insulinotraité ou sans précision (E11ni, cf. code_dnid = E1128...).
# utils.R inversait 0/8 : corrigé ici (voir MODIFICATIONS_V8.md, écart H1).
retro_code_diabete <- function(neocode, comp){
  comp <- as.character(comp)
  dplyr::case_when(neocode == "E10" ~ paste0("E10", comp),
                   neocode == "E11i" ~ paste0("E11", comp, "0"),
                   neocode == "E11ni" ~ paste0("E11", comp, "8"))
}

# Chemins (codes_diabete.yaml) des codes astérisques obligatoires par type de complication
CHEMINS_ASTERISQUES_DIABETE <- c(
  "2" = "renal/asterisques_obligatoires",
  "3" = "oculaire/asterisques_obligatoires",
  "4" = "neurologique/asterisques_obligatoires",
  "5" = "vasculaire_peripherique/asterisques_obligatoires",
  "6" = "autres_precisees/asterisques_obligatoires"
)

# Tirage de la complication (4e caractère) d'un diabète : distribution empirique
# ref_comp_diabete (cage, diabete, comp, nb), repli toutes classes d'âge si strate vide,
# repli "9" si aucune information. comp "8" (non précisées) -> "9" ; comp "7" (multiples)
# -> 3 à 4 complications distinctes parmi 2:6 (v7 : utils.R l.350-358, v7.2 l.373-385).
tirer_comp_diabete <- function(diabete_, cage_, ref_comp_diabete, comp_forcee = NULL){
  if(!is.null(comp_forcee)){
    comp <- as.character(comp_forcee)
  } else {
    ok <- !is.na(ref_comp_diabete$nb) & ref_comp_diabete$nb > 0 & ref_comp_diabete$diabete == diabete_
    prep_diab <- ref_comp_diabete[ok & ref_comp_diabete$cage == cage_, ]
    if(nrow(prep_diab) == 0) prep_diab <- ref_comp_diabete[ok, ]
    if(nrow(prep_diab) == 0) return(list(comp = "9", comps = "9"))
    comp <- as.character(prep_diab$comp[sample.int(nrow(prep_diab), 1, prob = prep_diab$nb)])
  }
  if(comp == "8") comp <- "9"
  comps <- comp
  if(comp == "7") comps <- sample(c("2", "3", "4", "5", "6"), sample(3:4, 1))
  list(comp = comp, comps = comps)
}

# Codes CIM-10 à insérer pour un néo-code diabète : code E10x / E11xx + astérisques
# obligatoires pour chaque complication 2:6 (utils.R l.345-382, signature explicite §5.2).
# codes_diab : tibble (code, chemin) issu de lire_codes_diabete().
get_codes_diabete_from_neo <- function(diabete_, cage_, ref_comp_diabete, codes_diab, comp_forcee = NULL){
  tc <- tirer_comp_diabete(diabete_, cage_, ref_comp_diabete, comp_forcee)
  code_diabete_sample <- retro_code_diabete(diabete_, tc$comp)
  comp_diag <- character(0)
  for(c_ in tc$comps){
    if(!c_ %in% names(CHEMINS_ASTERISQUES_DIABETE)) next
    candidats <- codes_diab$code[grepl(CHEMINS_ASTERISQUES_DIABETE[[c_]], codes_diab$chemin)]
    if(length(candidats) == 0) next
    comp_diag <- c(comp_diag, candidats[sample.int(length(candidats), 1)])
  }
  c(comp_diag, code_diabete_sample)
}

# Insertion de I10 quand le patient est hypertendu et qu'aucun code hta_autres n'est
# présent (v7.2 l.365). I10 n'est jamais gardé en plus d'un code hta_autres.
ajoute_hta <- function(das, hta_flag, hta_autres){
  das <- das[das != "I10"]
  if(!is.na(hta_flag) && hta_flag != "N" && length(intersect(hta_autres, das)) == 0) das <- c("I10", das)
  das
}

# Regroupement des objets de référence passés aux fonctions de tirage (§5.2 : plus de
# variable globale implicite).
construire_refs <- function(comp_diabete, codes_diab, codes_comp_sat_diab, hta_autres,
                            code_did, code_dnid_ins, code_dnid, neo_codes, paires_exclues = list()){
  stopifnot(is.data.frame(comp_diabete), all(c("cage", "diabete", "comp", "nb") %in% names(comp_diabete)),
            is.data.frame(codes_diab), all(c("code", "chemin") %in% names(codes_diab)),
            is.character(hta_autres), is.character(neo_codes), is.list(paires_exclues))
  list(comp_diabete = comp_diabete, codes_diab = codes_diab, codes_comp_sat_diab = codes_comp_sat_diab,
       hta_autres = hta_autres, code_did = code_did, code_dnid_ins = code_dnid_ins, code_dnid = code_dnid,
       neo_codes = neo_codes, paires_exclues = paires_exclues)
}

# Table de prévalence chronique en deux niveaux : strate (diag2, cage, sexe) et repli (cage, sexe) (§6.2)
prep_ref_chronique <- function(ref_das_chronique){
  list(
    strate = ref_das_chronique |> dplyr::summarise(nb_das = sum(nb_das), .by = c(diag2, das, sexe, cage)),
    repli  = ref_das_chronique |> dplyr::summarise(nb_das = sum(nb_das), .by = c(das, sexe, cage))
  )
}

# Codes candidats d'une strate, avec repli si moins de seuil_ref codes (§6.2)
candidats_chroniques <- function(diag, sexe_, cage_, ref_chro, seuil_ref){
  tmp <- ref_chro$strate |> dplyr::filter(diag2 == diag, sexe == sexe_, cage == cage_)
  source <- "strate"
  if(nrow(tmp) < seuil_ref){
    tmp <- ref_chro$repli |> dplyr::filter(sexe == sexe_, cage == cage_)
    source <- "repli"
  }
  list(tmp = tmp, source = source)
}

# Nombre cible de DAS chroniques : distribution empirique (cage, sexe, nb_chro, nb) sinon
# cible dégradée uniforme dans cibles_defaut[[cage]] (§6.2)
tirer_nb_chroniques <- function(cage_, sexe_, ref_nb_chro, cibles_defaut = list()){
  tmp <- ref_nb_chro[ref_nb_chro$cage == cage_ & ref_nb_chro$sexe == sexe_ & !is.na(ref_nb_chro$nb) & ref_nb_chro$nb > 0, ]
  if(nrow(tmp) > 0) return(as.integer(tmp$nb_chro[sample.int(nrow(tmp), 1, prob = tmp$nb)]))
  b <- cibles_defaut[[cage_]]
  if(is.null(b)) return(0L)
  v <- seq.int(b[1], b[2])
  as.integer(v[sample.int(length(v), 1)])
}

# Filtre GHM en C (v7.1.2 l.131-133) : exclut R2630, F0x et F1x sauf F17.
# v7.1.2 écrivait substr(das,1,2)!="F10" (2 caractères comparés à 3 : toujours vrai, F1x
# jamais exclu) ; corrigé en substr(das,1,2)!="F1" (MODIFICATIONS_V8.md, écart H2).
filtre_das_ghm_c <- function(tmp, ghm2_){
  if(substr(ghm2_,3,3)=="C"){
    tmp<-tmp |> dplyr::filter(das !="R2630", substr(das,1,2)!="F0", ( substr(das,1,2)!="F1"  | substr(das,1,3)=="F17") )
  }
  tmp
}

# Cœur de tirage des séjours courts (§6.2). Source : v7.1.2 l.117-164 (sample_das), refondu :
# nb de DAS tiré dans ref_nb_chro (plus de boucle 2:4), repli de strate, dedup_categorie,
# diabète/HTA, nb_tirages variantes. Retourne NULL si la strate est vide.
sample_das_court <- function(mode_hospit, sexe, cage, ghm2, diag2, duree, nb = NA,
                             ref_chro, ref_nb_chro, refs,
                             nb_tirages = 1, seuil_ref = 20, cibles_defaut = list(), age_max = 95){
  mode_hospit_ = as.character(mode_hospit)
  sexe_ = as.character(sexe)
  cage_ = as.character(cage)
  ghm2_ = as.character(ghm2)
  diag = as.character(diag2)
  duree_ = as.integer(duree)
  poids_ = as.numeric(nb)
  
  cand <- candidats_chroniques(diag, sexe_, cage_, ref_chro, seuil_ref)
  tmp <- filtre_das_ghm_c(cand$tmp, ghm2_)
  if(nrow(tmp) < 1) return(NULL)
  
  diabete_dp <- neo_code_de_diag(diag, refs$code_did, refs$code_dnid_ins, refs$code_dnid)
  
  df_tmp <- NULL
  for(i in seq_len(nb_tirages)){
    nb_cible <- tirer_nb_chroniques(cage_, sexe_, ref_nb_chro, cibles_defaut)
    age_i <- sample_age_ligne(cage_, age_max)
    das_samples <- character(0)
    if(nb_cible > 0){
      das_samples <- sample(x = tmp$das, prob = tmp$nb_das, size = min(nb_cible, nrow(tmp)))
    }
    das_samples <- dedup_categorie(das_samples, refs$paires_exclues)
    
    # Diabète : flag issu du DP, sinon d'un néo-code tiré ; néo-codes remplacés par les codes réels
    diabete_ <- diabete_dp
    neo_tires <- intersect(refs$neo_codes, das_samples)
    if(diabete_ == "N" && length(neo_tires) > 0) diabete_ <- neo_tires[1]
    das_samples <- das_samples[!das_samples %in% refs$neo_codes]
    codes_diabete <- character(0)
    if(diabete_ != "N") codes_diabete <- get_codes_diabete_from_neo(diabete_, cage_, refs$comp_diabete, refs$codes_diab)
    
    # HTA : flag porté par un I10 tiré ; I10 retiré si un code hta_autres est présent
    hta_ <- if("I10" %in% das_samples) "I10" else "N"
    das_final <- dedup_categorie(c(codes_diabete, ajoute_hta(das_samples, hta_, refs$hta_autres)), refs$paires_exclues)
    
    tibble::tibble(mode_hospit = mode_hospit_, sexe = sexe_, cage = cage_, ghm2 = ghm2_, diag2 = diag,
                   duree = duree_, poids = poids_, variante = i, age = age_i,
                   source_ref = cand$source, nb_cible = nb_cible, nb_das = length(das_final),
                   diabete_scenario = diabete_, hta_scenario = hta_,
                   diagnostic_associes = paste(das_final, collapse = " ")) |>
      dplyr::bind_rows(df_tmp) -> df_tmp
  }
  
  return(df_tmp)
}

# Cœur de tirage des séjours longs (§6.3). Source : v7.2 l.330-422 (sample_das), corrigé :
# §5.1 sexe == sexe_ ; §5.2 age en argument ; §5.4 dedup_categorie ; §5.5 codes_diab ;
# tables de référence en argument. Retourne NULL si la strate est vide.
sample_das_long <- function(mode_hospit, sexe, age, cage, racine, ghm2, diabete, hta, diag2, nbda,
                            diagnostic_associes, type_unite = NA, prep_sc = NA, poids = NA,
                            ref_das_aigu, refs, nb_tirage = 1){
  
  mode_hospit_ = as.character(mode_hospit)
  sexe_ = as.character(sexe)
  age_ = as.character(age)
  cage_ = as.character(cage)
  racine_ = as.character(racine)
  ghm2_ = as.character(ghm2)
  type_unite_ = as.character(type_unite)
  prep_sc_ = as.numeric(prep_sc)
  poids_ = as.numeric(poids)
  
  da = unlist(stringr::str_split(diagnostic_associes," "))
  da = da[!is.na(da) & nzchar(da)]
  diag = as.character(diag2)
  diabete_ = as.character(diabete)
  diabete_ = neo_code_de_diag(diag, refs$code_did, refs$code_dnid_ins, refs$code_dnid, defaut = diabete_)
  hta_ = as.character(hta)
  
  nbda_ = as.integer(nbda)
  
  ref_das_aigu |> dplyr::filter(diag2 == diag,mode_hospit == mode_hospit_, sexe == sexe_, cage== cage_,ghm2==ghm2_,!das%in%da )  -> tmp   # §5.1 (ex sexe_ ==sexe_)
  
  if(nrow(tmp)<1) return(NULL)
  
  df_tmp<-NULL
  
  nb_max = min(nbda_,nrow(tmp))
  
  for(i in seq_len(nb_tirage)){
    
    sample(x= tmp$das,prob = tmp$n,size = nb_max)->das_samples
    
    das_samples <- dedup_categorie(c(da, das_samples), refs$paires_exclues)   # §5.4 (ex filter_chap) ; la graine passe en premier
    
    das_samples <- ajoute_hta(das_samples, hta_, refs$hta_autres)
    
    #Pour le diabète :
    # - Vérification des DAS finalement choisis :
    #   * Si complication en lien avec diabète = complication multiples
    #   * Sinon : répartition en fonction de l'âge et du diabète
    if(diabete_ != "N"){
      comp_forcee <- if(length(intersect(c(diag, das_samples), refs$codes_comp_sat_diab)) != 0) "7" else NULL
      codes_diabete <- get_codes_diabete_from_neo(diabete_, cage_, refs$comp_diabete, refs$codes_diab, comp_forcee)
      das_samples <- dedup_categorie(c(da, codes_diabete, das_samples), refs$paires_exclues)
    }
    
    tibble::tibble(mode_hospit = mode_hospit_, sexe = sexe_, age = age_, cage = cage_, racine = racine_,
                   ghm2 = ghm2_, diabete = as.character(diabete), hta = hta_, diag2 = diag, nbda = nbda_,
                   type_unite = type_unite_, prep_sc = prep_sc_, poids = poids_, variante = i,
                   graine = paste(da, collapse = " "), diabete_scenario = diabete_,
                   nb_das = length(das_samples),
                   diagnostic_associes = paste(das_samples, collapse = " ")) |>
      dplyr::bind_rows(df_tmp) -> df_tmp
    
  }
  
  return(df_tmp)
  
}

# Codes CIM-10 dont le libellé indique « sans précision » (§7.5) ; codes sans point.
codes_imprecis_de_cim <- function(cim, motif = "sans précision|non précisé"){
  stopifnot(all(c("code", "libelle") %in% names(cim)))
  code <- gsub(".", "", as.character(cim$code), fixed = TRUE)
  unique(code[grepl(motif, enc2utf8(as.character(cim$libelle)), ignore.case = TRUE, perl = TRUE)])
}

# --- Helpers du rapport de contrôle (§8.2) ---
split_das <- function(x){
  x <- ifelse(is.na(x), "", as.character(x))
  lapply(strsplit(x, " ", fixed = TRUE), function(v) v[nzchar(v)])
}

# Vérifications programmatiques d'un jeu de scénarios. Retourne une liste de compteurs
# (0 attendu partout). NA si la colonne nécessaire est absente.
controler_scenarios <- function(df, hta_autres, seuil_pivot){
  n <- nrow(df)
  res <- list(n = n, doublons_categorie = NA, diabete_hors_flag = NA, i10_avec_hta_autres = NA, poids_sous_seuil = NA)
  if("poids" %in% names(df)) res$poids_sous_seuil <- sum(!(df$poids > seuil_pivot), na.rm = TRUE)
  if(!"diagnostic_associes" %in% names(df) || n == 0) return(res)
  das <- split_das(df$diagnostic_associes)
  res$doublons_categorie <- sum(vapply(das, function(v) any(duplicated(substr(v, 1, 3))), logical(1)))
  res$i10_avec_hta_autres <- sum(vapply(das, function(v) "I10" %in% v && length(intersect(v, hta_autres)) > 0, logical(1)))
  if("diabete_scenario" %in% names(df)){
    a_diab <- vapply(das, function(v) any(substr(v, 1, 3) %in% c("E10", "E11")), logical(1))
    res$diabete_hors_flag <- sum(a_diab & df$diabete_scenario == "N")
  }
  res
}

# Distribution du nombre de DAS par classe d'âge (comptes)
distribution_nb_das <- function(df){
  if(!"diagnostic_associes" %in% names(df) || nrow(df) == 0) return(NULL)
  df |>
    dplyr::mutate(nb_das = lengths(split_das(diagnostic_associes))) |>
    dplyr::summarise(n = dplyr::n(), moy = round(mean(nb_das), 2), min = min(nb_das),
                     q50 = stats::median(nb_das), max = max(nb_das), .by = cage) |>
    dplyr::arrange(cage)
}

# Taux de DAS « sans précision » parmi les DAS de sortie (mesuré, pas corrigé)
taux_imprecis <- function(df, codes_imprecis){
  if(!"diagnostic_associes" %in% names(df) || nrow(df) == 0) return(NA_real_)
  v <- unlist(split_das(df$diagnostic_associes))
  if(length(v) == 0) return(NA_real_)
  round(mean(v %in% codes_imprecis), 4)
}

## ---- B. Industrialisation ----

# --- B1. Chunking avec reprise --------------------------------------------------------
# Découpe df en chunks de chunk_size lignes ; chunk i : si <dossier>/<prefixe>_chunk_%04d<ext>
# existe -> sauté (reprise) ; sinon set.seed(seed_base + i), pmap(f, ...), écriture du chunk.
# Fin : relecture de tous les chunks, retour assemblé. Le seed par chunk garantit :
# reprise après plantage == exécution complète, bit à bit. `ecrire`/`lire` sont injectables
# (arrow par défaut ; les tests peuvent passer saveRDS/readRDS).
pmap_chunks <- function(df, f, chunk_size, dossier, prefixe, seed_base, ...,
                        garder_chunks = TRUE, ecrire = arrow::write_parquet, lire = arrow::read_parquet,
                        ext = ".parquet", verbose = TRUE){
  stopifnot(is.data.frame(df), chunk_size >= 1)
  if(!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)
  n <- nrow(df)
  n_chunks <- if(n == 0) 0L else as.integer(ceiling(n / chunk_size))
  fichiers <- character(0)
  for(i in seq_len(n_chunks)){
    fichier <- file.path(dossier, sprintf("%s_chunk_%04d%s", prefixe, i, ext))
    fichiers <- c(fichiers, fichier)
    if(file.exists(fichier)){
      if(verbose) cat(sprintf("  chunk %s %04d/%04d : déjà présent, sauté\n", prefixe, i, n_chunks))
      next
    }
    idx <- ((i - 1) * chunk_size + 1):min(i * chunk_size, n)
    set.seed(seed_base + i)
    res <- purrr::pmap(df[idx, , drop = FALSE], f, ...) |> purrr::list_rbind()
    if(is.null(res) || nrow(res) == 0 || ncol(res) == 0) res <- tibble::tibble(.chunk_vide = logical(0))
    ecrire(res, fichier)
    if(verbose) cat(sprintf("  chunk %s %04d/%04d : %d lignes\n", prefixe, i, n_chunks, nrow(res)))
  }
  out <- purrr::map(fichiers, function(fi) tibble::as_tibble(lire(fi))) |> purrr::list_rbind()
  if(".chunk_vide" %in% names(out)) out$.chunk_vide <- NULL
  if(!garder_chunks) unlink(fichiers)
  out
}

# --- B2. Sélection des séjours longs -------------------------------------------------
# Mode "catalogue_complet" : chaque ligne du catalogue -> nb_variantes tirages de complétion.
selection_catalogue_complet <- function(df, budget){
  stopifnot(nrow(df) > 0, budget >= 1)
  nb_variantes <- as.integer(ceiling(budget / nrow(df)))
  list(nb_variantes = nb_variantes, nrow = nrow(df), volume_attendu = nrow(df) * nb_variantes)
}

# Répartition d'un entier x entre des clés, méthode des plus forts restes ; à égalité de
# reste (cas général : parts égales), priorité décroissante puis ordre alphabétique des clés.
repartir_equitable <- function(x, cles, priorite = NULL){
  n <- length(cles)
  base <- x %/% n
  reste <- x %% n
  parts <- rep(as.integer(base), n)
  if(reste > 0){
    ordre <- if(is.null(priorite)) order(cles) else order(-priorite, cles)
    parts[ordre[seq_len(reste)]] <- parts[ordre[seq_len(reste)]] + 1L
  }
  stats::setNames(parts, cles)
}

tirer_avec_remise <- function(df, taille, col_poids){
  if(taille <= 0 || nrow(df) == 0) return(df[0, , drop = FALSE])
  idx <- sample.int(nrow(df), taille, replace = TRUE, prob = df[[col_poids]])
  df[idx, , drop = FALSE]
}

# Mode "quota_dp" : X = ceiling(budget / nb de diag2) scénarios par DP exactement.
# Par diag2 : U = types d'unités présents, P = quota_min_par_unite.
#   si length(U) * P >= X : X réparti équitablement entre les types (plus forts restes),
#                           tirage pondéré par poids avec remise dans chaque type ;
#   sinon : P par type (pondéré, avec remise, dans le type), puis X - length(U) * P au poids
#           avec remise sur tout le catalogue du DP (origine "libre").
selection_quota_dp <- function(df, budget, quota_min_par_unite, col_dp = "diag2", col_unite = "type_unite", col_poids = "poids"){
  stopifnot(all(c(col_dp, col_unite, col_poids) %in% names(df)), nrow(df) > 0, budget >= 1)
  dps <- unique(df[[col_dp]])
  X <- as.integer(ceiling(budget / length(dps)))
  P <- as.integer(quota_min_par_unite)
  res <- vector("list", length(dps))
  for(k in seq_along(dps)){
    d <- df[df[[col_dp]] == dps[k], , drop = FALSE]
    poids_type <- tapply(d[[col_poids]], d[[col_unite]], sum)
    U <- names(poids_type)
    parts <- list()
    if(length(U) * P >= X){
      quotas <- repartir_equitable(X, U, priorite = as.numeric(poids_type))
      for(u in U) if(quotas[[u]] > 0){
        s <- tirer_avec_remise(d[d[[col_unite]] == u, , drop = FALSE], quotas[[u]], col_poids)
        s$origine <- "plancher_" %+% u
        parts[[length(parts) + 1]] <- s
      }
    } else {
      for(u in U){
        s <- tirer_avec_remise(d[d[[col_unite]] == u, , drop = FALSE], P, col_poids)
        s$origine <- "plancher_" %+% u
        parts[[length(parts) + 1]] <- s
      }
      s <- tirer_avec_remise(d, X - length(U) * P, col_poids)
      s$origine <- "libre"
      parts[[length(parts) + 1]] <- s
    }
    res[[k]] <- dplyr::bind_rows(parts)
  }
  out <- dplyr::bind_rows(res)
  out$id_selection <- seq_len(nrow(out))
  list(selection = out, quota_par_dp = X, nb_dp = length(dps))
}

# Effectifs sélectionnés par diag2 × type_unite (mode quota_dp), format long + total par DP
effectifs_selection <- function(sel){
  sel |>
    dplyr::summarise(n = dplyr::n(), .by = c(diag2, type_unite)) |>
    tidyr::pivot_wider(names_from = type_unite, values_from = n, values_fill = 0L) |>
    dplyr::mutate(total = rowSums(dplyr::pick(-diag2))) |>
    dplyr::arrange(diag2)
}

# --- B3. Résolution des besoins (extraction) ------------------------------------------
etbs_label  <- function(etbs) gsub("[^A-Za-z0-9]", "", etbs)
nom_partiel <- function(etbs, an) sprintf("catalogue_partiel_%s_%s.parquet", etbs_label(etbs), an)
nom_ref     <- function(nom) nom %+% ".parquet"

# Fonction pure : à partir des fichiers présents (noms de base), calcule le plan :
# itérations (etbs, an) à faire, refs à faire, années à préparer (prep_data), besoin de
# prep_das_chronique. Ordre des itérations = celui des boucles (types × années).
resoudre_besoins <- function(types_etbs, ans, an_ref, fichiers_partiels, fichiers_exports,
                             forcer_refs = FALSE, noms_refs, refs_chroniques){
  it <- data.frame(etbs = rep(types_etbs, each = length(ans)), an = rep(as.integer(ans), times = length(types_etbs)),
                   stringsAsFactors = FALSE)
  it$fichier <- nom_partiel(it$etbs, it$an)
  it$a_faire <- !(it$fichier %in% basename(fichiers_partiels))
  refs <- data.frame(nom = noms_refs, fichier = nom_ref(noms_refs), stringsAsFactors = FALSE)
  refs$a_faire <- isTRUE(forcer_refs) | !(refs$fichier %in% basename(fichiers_exports))
  annees <- sort(unique(c(it$an[it$a_faire], if(any(refs$a_faire)) as.integer(an_ref))))
  list(iterations = it, refs = refs, annees_a_preparer = as.integer(annees),
       prep_das_chronique = any(refs$a_faire & refs$nom %in% refs_chroniques),
       rien_a_faire = !any(it$a_faire) && !any(refs$a_faire))
}

imprimer_plan <- function(plan){
  it <- plan$iterations; refs <- plan$refs
  cat("== PLAN D'EXTRACTION ==\n")
  cat(sprintf("Itérations (etbs, an) : %d à faire, %d sautées (partiel présent)\n", sum(it$a_faire), sum(!it$a_faire)))
  for(i in seq_len(nrow(it))) cat(sprintf("  %-6s %-6s %s\n", it$etbs[i], it$an[i], if(it$a_faire[i]) "A FAIRE" else "sauté"))
  cat(sprintf("Références : %d à faire, %d sautées\n", sum(refs$a_faire), sum(!refs$a_faire)))
  for(i in seq_len(nrow(refs))) cat(sprintf("  %-36s %s\n", refs$nom[i], if(refs$a_faire[i]) "A FAIRE" else "sautée"))
  cat("Années à préparer (prep_data) : ", if(length(plan$annees_a_preparer)) paste(plan$annees_a_preparer, collapse = ", ") else "aucune", "\n")
  cat("prep_das_chronique(AN_REF)    : ", if(plan$prep_das_chronique) "oui" else "non", "\n")
  if(plan$rien_a_faire) cat("TOUT EST A JOUR : aucune requête base, passage direct à l'agrégation.\n")
  invisible(plan)
}

# --- B4. Partiels : méta et instrumentation --------------------------------------------
# Les partiels dépendent de K_GRAINE_LONGS et de la logique amont (prep_data, prep_scenarios2,
# NBDA_MAX, DUREE_LONGS, PIVOTS_LONGS : filtres des séjours et grain des comptes), PAS du seuil
# > SEUIL_PIVOT ni du périmètre d'années. Toute différence sur ces clés rend les partiels
# invalides -> stop() ; VERSION_SCRIPT différent -> avertissement seulement.
CLES_PARTIELS_BLOQUANTES <- c("K_GRAINE_LONGS", "NBDA_MAX", "DUREE_LONGS", "PIVOTS_LONGS")
CLES_PARTIELS_AVERTISSEMENT <- c("VERSION_SCRIPT")

meta_partiels_courant <- function(k, nbda_max, duree_longs, pivots, version){
  list(K_GRAINE_LONGS = as.integer(k), NBDA_MAX = as.integer(nbda_max),
       DUREE_LONGS = as.integer(range(duree_longs)), PIVOTS_LONGS = as.character(pivots),
       VERSION_SCRIPT = as.character(version), date = as.character(Sys.Date()))
}

# Retourne list(erreur = message ou NULL, avertissements = character()).
verifier_partiels_meta <- function(existant, courant){
  if(is.null(existant)) return(list(erreur = NULL, avertissements = character(0)))
  meme <- function(ch) identical(as.character(unlist(existant[[ch]])), as.character(unlist(courant[[ch]])))
  detail <- function(ch) sprintf("%s (partiels : %s ; courant : %s)", ch,
                                 paste(unlist(existant[[ch]]), collapse = ","), paste(unlist(courant[[ch]]), collapse = ","))
  bloquantes <- CLES_PARTIELS_BLOQUANTES[!vapply(CLES_PARTIELS_BLOQUANTES, meme, logical(1))]
  erreur <- NULL
  if(length(bloquantes) > 0){
    erreur <- sprintf("partiels_meta.yaml : %s. Les partiels ont été construits avec d'autres paramètres amont : vider PARTIELS_DIR avant de relancer.",
                      paste(vapply(bloquantes, detail, character(1)), collapse = " ; "))
  }
  av <- character(0)
  for(ch in CLES_PARTIELS_AVERTISSEMENT) if(!meme(ch)) av <- c(av, "partiels_meta.yaml : " %+% detail(ch))
  list(erreur = erreur, avertissements = av)
}

# Ligne d'instrumentation après une itération : apport marginal en lignes et en diag2.
apports_iteration <- function(etbs, an, statut, df_partiel, df_cumul, diag2_avant){
  diag2_apres <- unique(df_cumul$diag2)
  data.frame(etbs = etbs, an = as.integer(an), statut = statut,
             nb_lignes_partiel = nrow(df_partiel), nb_lignes_cumul = nrow(df_cumul),
             nb_diag2_cumul = length(diag2_apres), nb_diag2_nouveaux = length(setdiff(diag2_apres, diag2_avant)),
             stringsAsFactors = FALSE)
}

# --- B5. Tirage : références et méta ---------------------------------------------------
# Pénalisation des effectifs .9 des codes diabète (v7.1.2 l.207-214), appliquée côté tirage.
penaliser_comp_diabete <- function(df, cage_ped, cage_ages, penalite_ages, penalite_autres){
  df |>
    dplyr::mutate(tot = sum(nb,na.rm=TRUE),.by=c(cage,diabete)) |>
    dplyr::mutate(nb = dplyr::case_when(comp=="9"&cage%in%cage_ages~tot*penalite_ages,
                                        comp=="9"&! cage%in%cage_ped~tot*penalite_autres,
                                        TRUE~nb)) |>
    dplyr::select(-tot)
}

# Fichiers manquants dans un dossier (noms de base) ; character(0) si tout est là.
fichiers_manquants <- function(dossier, noms) noms[!file.exists(file.path(dossier, noms))]

# meta_tirage.yaml : la sélection/les chunks ne sont valides que pour ces paramètres.
verifier_meta_tirage <- function(existant, courant, cles){
  if(is.null(existant)) return(NULL)
  diff <- cles[vapply(cles, function(ch) !identical(as.character(unlist(existant[[ch]])), as.character(unlist(courant[[ch]]))), logical(1))]
  if(length(diff) == 0) return(NULL)
  sprintf("meta_tirage.yaml : paramètres différents de la sélection figée (%s). Vider CHUNKS_DIR, selection_longs.parquet et meta_tirage.yaml avant de relancer.",
          paste(diff, collapse = ", "))
}

# --- B6. Livrables de validation ---------------------------------------------------------
libelles_cim <- function(cim){
  stopifnot(all(c("code", "libelle") %in% names(cim)))
  code <- gsub(".", "", as.character(cim$code), fixed = TRUE)
  lib <- as.character(cim$libelle)
  ok <- !duplicated(code)
  stats::setNames(lib[ok], code[ok])
}

# n lignes stratifiées par col_cmd (round-robin sur les CMD, ordre aléatoire sous le seed)
echantillonner_revue <- function(df, n, col_cmd = "cmd"){
  if(nrow(df) == 0) return(df)
  df |>
    dplyr::mutate(.r = stats::runif(dplyr::n())) |>
    dplyr::arrange(.r) |>
    dplyr::mutate(.k = dplyr::row_number(), .by = dplyr::all_of(col_cmd)) |>
    dplyr::arrange(.k, .r) |>
    dplyr::slice_head(n = n) |>
    dplyr::select(-.r, -.k)
}

# Colonnes lisibles : DP et chaque DAS avec libellé CIM, graine marquée [G] (longs)
formater_revue <- function(df, branche, lib){
  if(nrow(df) == 0) return(tibble::tibble())
  das <- split_das(df$diagnostic_associes)
  graine <- if("graine" %in% names(df)) split_das(df$graine) else rep(list(character(0)), nrow(df))
  libelle <- function(code){ l <- unname(lib[code]); ifelse(is.na(l), "?", l) }
  das_lib <- mapply(function(d, g){
    if(length(d) == 0) return("")
    paste0(d, " (", libelle(d), ")", ifelse(d %in% g, " [G]", ""), collapse = " | ")
  }, das, graine)
  col <- function(nom, defaut = NA){
    if(nom %in% names(df)) return(df[[nom]])
    if(length(defaut) == nrow(df)) defaut else rep(defaut, nrow(df))
  }
  tibble::tibble(branche = branche, cmd = substr(df$ghm2, 1, 2), ghm2 = df$ghm2,
                 type_unite = col("type_unite", NA_character_), sexe = df$sexe,
                 age = as.character(col("age")), cage = col("cage", NA_character_), duree = col("duree"),
                 mode_entree = col("mode_entree", NA_character_), mode_sortie = col("mode_sortie", NA_character_),
                 dp = df$diag2, dp_libelle = libelle(df$diag2),
                 nb_das = lengths(das), das_libelles = unname(das_lib),
                 diabete_scenario = col("diabete_scenario", NA_character_), hta = col("hta", col("hta_scenario", NA_character_)))
}

# Top n DAS par CMD (fréquence dans les DAS de sortie)
top_das_par_cmd <- function(df, n_top = 30){
  if(nrow(df) == 0) return(tibble::tibble(cmd = character(0), das = character(0), n = integer(0), rang = integer(0)))
  das <- split_das(df$diagnostic_associes)
  tibble::tibble(cmd = rep(substr(df$ghm2, 1, 2), lengths(das)), das = unlist(das)) |>
    dplyr::count(cmd, das, name = "n") |>
    dplyr::arrange(cmd, dplyr::desc(n), das) |>
    dplyr::mutate(rang = dplyr::row_number(), .by = cmd) |>
    dplyr::filter(rang <= n_top)
}

## ---- C. Conversion E669 -> E660 (doctrine : E669x = erreur de codage) ----
# Périmètre STRICT : ^E669 uniquement (E661, E662, E668 intacts). Conversion entièrement
# post-collect ; les partiels restent en codes bruts (la conversion n'est pas une clé de
# verifier_partiels_meta : elle s'applique à la ré-agrégation).
# Règles : (a) E669 à suffixe -> E660 + suffixe conservé (déterministe) ;
#          (b) E669 NU -> effectifs répartis selon la distribution observée des E660x, en cascade
#              strate (cage, sexe) -> toutes strates -> "E660" + defaut, plus forts restes
#              (sum(n) conservé exactement, aucun aléa).

convertir_e669 <- function(codes){
  codes <- as.character(codes)
  suff <- !is.na(codes) & grepl("^E669.", codes)
  codes[suff] <- paste0("E660", substring(codes[suff], 5))
  codes
}

est_e669_nu <- function(codes) !is.na(codes) & codes == "E669"

# Répartition d'un effectif n selon des parts (normalisées), arrondi aux plus forts restes :
# sum(résultat) == n exactement. Égalité de reste -> ordre des parts.
repartir_proportionnel <- function(n, parts){
  if(length(parts) == 0) return(numeric(0))
  parts <- parts / sum(parts)
  brut <- n * parts
  base <- floor(brut)
  reste <- round(n - sum(base))
  if(reste > 0){
    ordre <- order(-(brut - base), seq_along(parts))
    base[ordre[seq_len(reste)]] <- base[ordre[seq_len(reste)]] + 1
  }
  base
}

# Distribution de référence des E660x : lignes par strate (cage, sexe, code, n, part) puis
# lignes globales (cage = sexe = NA). Calculée AVANT toute conversion de df_ref.
distribution_e660 <- function(df_ref, col_code = "das", col_n = "nb_das"){
  vide <- tibble::tibble(cage = character(0), sexe = character(0), code = character(0), n = numeric(0), part = numeric(0))
  if(!all(c(col_code, col_n) %in% names(df_ref))) return(vide)
  d <- df_ref[!is.na(df_ref[[col_code]]) & grepl("^E660", df_ref[[col_code]]), , drop = FALSE]
  if(nrow(d) == 0) return(vide)
  d <- tibble::tibble(cage = if("cage" %in% names(d)) as.character(d$cage) else NA_character_,
                      sexe = if("sexe" %in% names(d)) as.character(d$sexe) else NA_character_,
                      code = as.character(d[[col_code]]), n = as.numeric(d[[col_n]]))
  globale <- d |> dplyr::summarise(n = sum(n), .by = code) |> dplyr::mutate(cage = NA_character_, sexe = NA_character_, part = n / sum(n))
  strate <- d[!is.na(d$cage) & !is.na(d$sexe), ]
  if(nrow(strate) > 0){
    strate <- strate |> dplyr::summarise(n = sum(n), .by = c(cage, sexe, code)) |> dplyr::mutate(part = n / sum(n), .by = c(cage, sexe))
  } else strate <- vide
  dplyr::bind_rows(strate, globale) |> dplyr::select(cage, sexe, code, n, part) |> dplyr::arrange(!is.na(cage), cage, sexe, code)
}

# Classes cibles d'un E669 nu, cascade strate -> globale -> défaut. Retourne tibble(code, part).
classes_e660 <- function(dist, cage_ = NA, sexe_ = NA, defaut = "0"){
  if(nrow(dist) > 0 && !is.na(cage_) && !is.na(sexe_)){
    s <- dist[!is.na(dist$cage) & dist$cage == cage_ & dist$sexe == sexe_, , drop = FALSE]
    if(nrow(s) > 0) return(s[order(s$code), c("code", "part")])
  }
  g <- dist[is.na(dist$cage), , drop = FALSE]
  if(nrow(g) > 0) return(g[order(g$code), c("code", "part")])
  tibble::tibble(code = paste0("E660", defaut), part = 1)
}

# Éclate les lignes dont col_code == "E669" (nu) en autant de lignes que de classes E660x
# (cascade sur cage/sexe si présentes), effectifs col_n répartis aux plus forts restes.
# cols_strate : colonnes conservées telles quelles. Même schéma en sortie.
repartir_e669_nu <- function(df, col_code, cols_strate, col_n, dist, defaut = "0"){
  nu <- est_e669_nu(df[[col_code]])
  if(!any(nu)) return(df)
  garde <- df[!nu, , drop = FALSE]
  lignes <- df[nu, , drop = FALSE]
  a_cage <- "cage" %in% names(df); a_sexe <- "sexe" %in% names(df)
  out <- vector("list", nrow(lignes))
  for(i in seq_len(nrow(lignes))){
    cl <- classes_e660(dist, if(a_cage) as.character(lignes$cage[i]) else NA, if(a_sexe) as.character(lignes$sexe[i]) else NA, defaut)
    alloc <- repartir_proportionnel(lignes[[col_n]][i], cl$part)
    k <- which(alloc > 0)
    if(length(k) == 0) next
    l <- lignes[rep(i, length(k)), , drop = FALSE]
    l[[col_code]] <- cl$code[k]
    l[[col_n]] <- alloc[k]
    out[[i]] <- l
  }
  dplyr::bind_rows(garde, dplyr::bind_rows(out))
}

reagreger <- function(df, cols, col_n){
  res <- df |> dplyr::summarise(.n_tmp = sum(.data[[col_n]]), .by = dplyr::all_of(cols))
  names(res)[names(res) == ".n_tmp"] <- col_n
  res[, intersect(names(df), names(res)), drop = FALSE]
}

# Table de comptes : conversion suffixée, répartition du nu, ré-agrégation sum(col_n) par
# (cols_strate, col_code). Schéma de sortie = cols_strate + col_code + col_n (ordre d'origine).
convertir_e669_comptes <- function(df, col_code, cols_strate, col_n, dist, defaut = "0"){
  df[[col_code]] <- convertir_e669(df[[col_code]])
  df <- repartir_e669_nu(df, col_code, cols_strate, col_n, dist, defaut)
  reagreger(df, c(cols_strate, col_code), col_n)
}

# Combinaisons de codes séparés par un espace (graines) : conversion de chaque code, E669 nu
# réparti par la cascade (ligne éclatée en autant de lignes que de classes), codes re-triés
# (ordre C, comme dplyr::arrange(das) dans prep_scenarios2) et dédoublonnés, ré-agrégation.
convertir_e669_combo <- function(df, col_combo, cols_strate, col_n, dist, defaut = "0"){
  combos <- lapply(split_das(df[[col_combo]]), convertir_e669)
  norm <- function(v) paste(sort(unique(v), method = "radix"), collapse = " ")
  df[[col_combo]] <- vapply(combos, norm, character(1), USE.NAMES = FALSE)
  a_nu <- vapply(combos, function(v) "E669" %in% v, logical(1))
  if(!any(a_nu)) return(reagreger(df, c(cols_strate, col_combo), col_n))
  garde <- df[!a_nu, , drop = FALSE]
  lignes <- df[a_nu, , drop = FALSE]; combos_nu <- combos[a_nu]
  a_cage <- "cage" %in% names(df); a_sexe <- "sexe" %in% names(df)
  out <- vector("list", nrow(lignes))
  for(i in seq_len(nrow(lignes))){
    cl <- classes_e660(dist, if(a_cage) as.character(lignes$cage[i]) else NA, if(a_sexe) as.character(lignes$sexe[i]) else NA, defaut)
    alloc <- repartir_proportionnel(lignes[[col_n]][i], cl$part)
    k <- which(alloc > 0)
    if(length(k) == 0) next
    l <- lignes[rep(i, length(k)), , drop = FALSE]
    l[[col_combo]] <- vapply(cl$code[k], function(cd) norm(replace(combos_nu[[i]], combos_nu[[i]] == "E669", cd)), character(1), USE.NAMES = FALSE)
    l[[col_n]] <- alloc[k]
    out[[i]] <- l
  }
  reagreger(dplyr::bind_rows(garde, dplyr::bind_rows(out)), c(cols_strate, col_combo), col_n)
}

# Tables sans effectif (v_admin) : conversion suffixée ; E669 nu remplacé par CHAQUE classe de
# la cascade (une ligne par classe) ; dédoublonnage.
convertir_e669_distinct <- function(df, col_code, dist, defaut = "0"){
  df[[col_code]] <- convertir_e669(df[[col_code]])
  nu <- est_e669_nu(df[[col_code]])
  if(any(nu)){
    garde <- df[!nu, , drop = FALSE]; lignes <- df[nu, , drop = FALSE]
    a_cage <- "cage" %in% names(df); a_sexe <- "sexe" %in% names(df)
    out <- vector("list", nrow(lignes))
    for(i in seq_len(nrow(lignes))){
      cl <- classes_e660(dist, if(a_cage) as.character(lignes$cage[i]) else NA, if(a_sexe) as.character(lignes$sexe[i]) else NA, defaut)
      l <- lignes[rep(i, nrow(cl)), , drop = FALSE]; l[[col_code]] <- cl$code
      out[[i]] <- l
    }
    df <- dplyr::bind_rows(garde, dplyr::bind_rows(out))
  }
  dplyr::distinct(df)
}

# Comptage des ^E669 résiduels dans des colonnes de codes (scalaires ou combinaisons)
compter_e669 <- function(df, cols){
  cols <- intersect(cols, names(df))
  if(length(cols) == 0 || nrow(df) == 0) return(0L)
  sum(vapply(cols, function(cc) sum(grepl("^E669", unlist(split_das(df[[cc]])))), integer(1), USE.NAMES = FALSE))
}

# Effectifs E660x par classe dans des colonnes de codes
effectifs_e660 <- function(df, cols){
  cols <- intersect(cols, names(df))
  if(length(cols) == 0 || nrow(df) == 0) return(tibble::tibble(code = character(0), n = integer(0)))
  v <- unname(unlist(lapply(cols, function(cc) unlist(split_das(df[[cc]])))))
  v <- v[grepl("^E660", v)]
  if(length(v) == 0) return(tibble::tibble(code = character(0), n = integer(0)))
  tibble::tibble(code = v) |> dplyr::count(code, name = "n") |> dplyr::arrange(code)
}

# Mesure d'impact de la conversion sur le catalogue agrégé (avant / après, avant seuil).
# Profils « entrés » : clés pivot > seuil après conversion sans clé > seuil avant (clés
# avant comparées après conversion suffixée de diag2) ; « sortis » : l'inverse.
impact_conversion_catalogue <- function(avant, apres, pivots_seuil, seuil, col_n = "n"){
  # clés pivot > seuil, sommées sur les codes TELS QUELS ; les clés retenues sont ensuite
  # exprimées avec diag2 converti (suffixe) pour être comparables avant / après
  seuil_cles <- function(d){
    s <- d |> dplyr::summarise(nb = sum(.data[[col_n]]), .by = dplyr::all_of(pivots_seuil)) |> dplyr::filter(nb > seuil)
    do.call(paste, c(lapply(pivots_seuil, function(p) if(p == "diag2") convertir_e669(s[[p]]) else as.character(s[[p]])), sep = "\r"))
  }
  k_avant <- unique(seuil_cles(avant)); k_apres <- unique(seuil_cles(apres))
  diag2_e669 <- grepl("^E669", avant$diag2)
  das_e669 <- vapply(split_das(avant$diagnostic_associes), function(v) sum(grepl("^E669", v)), integer(1))
  list(n_total_avant = sum(avant[[col_n]]), n_total_apres = sum(apres[[col_n]]),
       lignes_avant = nrow(avant), lignes_apres = nrow(apres), lignes_fusionnees = nrow(avant) - nrow(apres),
       e669_diag2_suffixe = sum(avant[[col_n]][diag2_e669 & avant$diag2 != "E669"]),
       e669_diag2_nu = sum(avant[[col_n]][avant$diag2 == "E669"]),
       e669_graine_suffixe = sum(avant[[col_n]] * vapply(split_das(avant$diagnostic_associes), function(v) sum(grepl("^E669.", v)), integer(1))),
       e669_graine_nu = sum(avant[[col_n]] * vapply(split_das(avant$diagnostic_associes), function(v) sum(v == "E669"), integer(1))),
       profils_seuil_avant = length(k_avant), profils_seuil_apres = length(k_apres),
       profils_entres = length(setdiff(k_apres, k_avant)), profils_sortis = length(setdiff(k_avant, k_apres)))
}

# Conversions changeant le niveau CMA : niveau par code observé dans une table (code, niveau)
# brute ; compte les effectifs E669x dont le niveau diffère de celui de la cible E660x
# (cible non observée -> "inconnu"). Mesure, ne bloque pas.
impact_niveau_cma <- function(df_raw, col_code = "das", col_niveau = "niveau", col_n = "nb_das"){
  vide <- list(effectif_e669 = 0, niveau_change = 0, cible_inconnue = 0)
  if(!all(c(col_code, col_niveau, col_n) %in% names(df_raw))) return(vide)
  niv <- df_raw |> dplyr::distinct(.data[[col_code]], .data[[col_niveau]]) |> dplyr::distinct(.data[[col_code]], .keep_all = TRUE)
  niv <- stats::setNames(as.character(niv[[col_niveau]]), as.character(niv[[col_code]]))
  e <- df_raw[grepl("^E669.", df_raw[[col_code]]), , drop = FALSE]
  if(nrow(e) == 0) return(vide)
  cible <- convertir_e669(e[[col_code]])
  niv_cible <- unname(niv[cible])
  list(effectif_e669 = sum(e[[col_n]]),
       niveau_change = sum(e[[col_n]][!is.na(niv_cible) & niv_cible != as.character(e[[col_niveau]])]),
       cible_inconnue = sum(e[[col_n]][is.na(niv_cible)]))
}
