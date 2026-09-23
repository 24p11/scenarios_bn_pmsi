###############################################################################
# helpers.R — helpers purs du pipeline scenarios_bn_pmsi (v8, industrialisation)
#
# Sourcé par extraction.R, tirage.R et les tests.
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
  if(inherits(ref_chro$strate, "index_ref_chro")){   # accès direct (indexer_ref_chronique) : identique au filtre
    tmp <- ref_chro$strate[[cle_strate(diag, sexe_, cage_)]]; if(is.null(tmp)) tmp <- vide_comme(ref_chro$colonnes_strate)
    source <- "strate"
    if(nrow(tmp) < seuil_ref){ tmp <- ref_chro$repli[[cle_strate(sexe_, cage_)]]; if(is.null(tmp)) tmp <- vide_comme(ref_chro$colonnes_repli); source <- "repli" }
    return(list(tmp = tmp, source = source))
  }
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
# Campagnes (chantier « courts en campagnes ») : mêmes mécaniques que sample_das_long — variantes numérotées
# à partir de variante_debut (variante_max du pivot au registre + 1), dédoublonnage souple des variantes
# (dedoublonner), exclusion des hash_das déjà enregistrés pour ce pivot (hash_exclus), AUCUN re-tirage ;
# la doctrine de tirage (saturation, diabète, I10, dedup de catégorie) est inchangée.
sample_das_court <- function(mode_hospit, sexe, cage, ghm2, diag2, duree, nb = NA,
                             ref_chro, ref_nb_chro, refs,
                             nb_tirages = 1, seuil_ref = 20, cibles_defaut = list(), age_max = 95,
                             dedoublonner = FALSE, id_profil = NA_character_, variante_debut = 1L, hash_exclus = "", ...){
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
                   duree = duree_, poids = poids_, variante = as.integer(variante_debut) + i - 1L, age = age_i,
                   source_ref = cand$source, nb_cible = nb_cible, nb_das = length(das_final),
                   diabete_scenario = diabete_, hta_scenario = hta_,
                   diagnostic_associes = paste(das_final, collapse = " ")) |>
      dplyr::bind_rows(df_tmp) -> df_tmp
  }
  
  # Unicité souple (campagnes) : variantes d'un même pivot dédoublonnées sur le jeu complet de DAS ; AUCUN re-tirage.
  if(isTRUE(dedoublonner) && !is.null(df_tmp)){
    df_tmp$nb_variantes_demandees <- as.integer(nb_tirages)
    df_tmp <- dedoublonner_variantes(df_tmp, "diagnostic_associes")
  }
  # Identifiants stables (recette id_courts_v1) et exclusion des jeux déjà enregistrés au registre pour ce pivot
  if(!is.null(df_tmp) && !is.na(id_profil)){
    df_tmp$id_profil <- as.character(id_profil)
    df_tmp$hash_das <- hash_das_de(df_tmp$diagnostic_associes)
    exclus <- strsplit(as.character(hash_exclus), " ", fixed = TRUE)[[1]]; exclus <- exclus[nzchar(exclus)]
    if(length(exclus) > 0) df_tmp <- df_tmp[!df_tmp$hash_das %in% exclus, , drop = FALSE]
    df_tmp$id_scenario <- id_scenario_de(df_tmp$id_profil, df_tmp$variante)
    if(nrow(df_tmp) == 0) return(NULL)
  }
  
  return(df_tmp)
}

# Cœur de tirage des séjours longs (§6.3). Source : v7.2 l.330-422 (sample_das), corrigé :
# §5.1 sexe == sexe_ ; §5.2 age en argument ; §5.4 dedup_categorie ; §5.5 codes_diab ;
# tables de référence en argument. Retourne NULL si la strate est vide.
sample_das_long <- function(mode_hospit, sexe, age, cage, racine, ghm2, diabete, hta, diag2, nbda,
                            diagnostic_associes, type_unite = NA, prep_sc = NA, poids = NA,
                            ref_das_aigu, refs, nb_tirage = 1, dedoublonner = FALSE,
                            id_profil = NA_character_, variante_debut = 1L, hash_exclus = "", ...){
  
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
  
  if(inherits(ref_das_aigu, "index_ref_das")){   # accès direct à la strate (indexer_ref_das) : identique au filtre, prouvé en test
    tmp <- ref_das_aigu[[cle_strate(diag, mode_hospit_, sexe_, cage_, ghm2_)]]
    if(is.null(tmp)) return(NULL)
    tmp <- tmp[!tmp$das %in% da, , drop = FALSE]
  } else {
  ref_das_aigu |> dplyr::filter(diag2 == diag,mode_hospit == mode_hospit_, sexe == sexe_, cage== cage_,ghm2==ghm2_,!das%in%da )  -> tmp   # §5.1 (ex sexe_ ==sexe_)
  }
  
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
                   type_unite = type_unite_, prep_sc = prep_sc_, poids = poids_, variante = as.integer(variante_debut) + i - 1L,
                   graine = paste(da, collapse = " "), diabete_scenario = diabete_,
                   nb_das = length(das_samples),
                   diagnostic_associes = paste(das_samples, collapse = " ")) |>
      dplyr::bind_rows(df_tmp) -> df_tmp
    
  }
  
  # Unicité souple (quota_dp_fixe) : les nb_tirage variantes d'une même ligne sont dédoublonnées
  # sur le jeu complet de DAS (graine + complétion + doctrine, ordre indifférent) ; AUCUN re-tirage.
  if(isTRUE(dedoublonner) && !is.null(df_tmp)){
    df_tmp$nb_variantes_demandees <- as.integer(nb_tirage)
    df_tmp <- dedoublonner_variantes(df_tmp, "diagnostic_associes")
  }
  # Campagnes : identifiants stables et exclusion des jeux déjà enregistrés au registre pour ce profil
  # (recyclage à variantes nouvelles : doublon inter-campagnes éliminé comme un doublon intra-ligne, sans re-tirage)
  if(!is.null(df_tmp) && !is.na(id_profil)){
    df_tmp$id_profil <- as.character(id_profil)
    df_tmp$hash_das <- hash_das_de(df_tmp$diagnostic_associes)
    exclus <- strsplit(as.character(hash_exclus), " ", fixed = TRUE)[[1]]; exclus <- exclus[nzchar(exclus)]
    if(length(exclus) > 0) df_tmp <- df_tmp[!df_tmp$hash_das %in% exclus, , drop = FALSE]
    df_tmp$id_scenario <- id_scenario_de(df_tmp$id_profil, df_tmp$variante)
    if(nrow(df_tmp) == 0) return(NULL)
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

# --- B1. Chunking DYNAMIQUE avec reprise ----------------------------------------------------
# Taille de chunk dimensionnée par les données : au plus nb_chunks_max chunks par tirage, jamais
# de chunks minuscules (plancher chunk_size_min) ; chunk_size_fixe non-NA court-circuite le calcul.
taille_chunk <- function(n, nb_chunks_max = NB_CHUNKS_MAX, chunk_size_min = CHUNK_SIZE_MIN, chunk_size_fixe = CHUNK_SIZE_FIXE){
  if(!is.na(chunk_size_fixe)) return(as.integer(chunk_size_fixe))
  stopifnot(nb_chunks_max >= 1, chunk_size_min >= 1)
  as.integer(max(chunk_size_min, ceiling(n / nb_chunks_max)))
}

# Sidecar de reprise : <dossier>/<prefixe>_chunks_meta.yaml (n, chunk_size, seed_base, nb_chunks,
# date), écrit AVANT le premier chunk. Les index de chunks désignent des PLAGES DE LIGNES de df :
# reprendre sur un découpage différent (n, chunk_size ou seed_base) corromprait silencieusement
# le résultat -> stop() explicite. Chunks présents sans sidecar (dossier antérieur au chantier)
# -> même stop, en l'expliquant.
verifier_chunks_meta <- function(existant, courant, dossier, prefixe){
  cles <- c("n", "chunk_size", "seed_base")
  if(is.null(existant)){
    return(sprintf("pmap_chunks : des chunks `%s_chunk_*` existent dans %s sans sidecar %s_chunks_meta.yaml (dossier antérieur au chantier « chunking dynamique » : découpage inconnu). Videz %s ou restaurez le sidecar.", prefixe, dossier, prefixe, dossier))
  }
  diff <- cles[vapply(cles, function(k) !identical(as.numeric(unlist(existant[[k]])), as.numeric(courant[[k]])), logical(1))]
  if(length(diff) == 0) return(NULL)
  sprintf("pmap_chunks : découpage incompatible avec les chunks existants de `%s` dans %s ; videz %s ou restaurez les paramètres : attendu %s ; reçu %s.",
          prefixe, dossier, dossier,
          paste(sprintf("%s = %s", cles, vapply(cles, function(k) paste(unlist(existant[[k]]), collapse = ","), character(1))), collapse = ", "),
          paste(sprintf("%s = %s", cles, vapply(cles, function(k) as.character(courant[[k]]), character(1))), collapse = ", "))
}

# Découpe df en chunks de chunk_size lignes (NULL = taille_chunk(nrow(df))) ; chunk i : si
# <dossier>/<prefixe>_chunk_%04d<ext> existe -> sauté (reprise) ; sinon set.seed(seed_base + i),
# pmap(f, ...), écriture du chunk. Fin : relecture de tous les chunks, retour assemblé. Le seed
# par chunk garantit : reprise après plantage == exécution complète, bit à bit. `ecrire`/`lire`
# sont injectables (arrow par défaut ; les tests peuvent passer saveRDS/readRDS).
# chunk_range = c(i, j) : ne traite que les chunks i..j (PARALLÉLISME par sessions sur plages
# disjointes du même dossier ; sidecar partagé, vérifié, pas réécrit s'il existe et concorde) ;
# assembler = FALSE (défaut quand une plage est donnée) : pas de relecture finale. Écriture
# ATOMIQUE de chaque chunk (.tmp puis file.rename) : un plantage ne laisse jamais un chunk
# partiel pris pour complet (les .tmp orphelins sont ignorés et recalculés). Débit imprimé.
pmap_chunks <- function(df, f, chunk_size = NULL, dossier, prefixe, seed_base, ...,
                        garder_chunks = TRUE, ecrire = arrow::write_parquet, lire = arrow::read_parquet,
                        ext = ".parquet", verbose = TRUE, chunk_range = NULL, assembler = is.null(chunk_range)){
  stopifnot(is.data.frame(df))
  if(!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)
  n <- nrow(df)
  if(is.null(chunk_size)) chunk_size <- taille_chunk(n)
  stopifnot(chunk_size >= 1)
  n_chunks <- if(n == 0) 0L else as.integer(ceiling(n / chunk_size))
  if(verbose) cat(sprintf("  [chunks %s] n = %d lignes ; chunk_size = %d ; %d chunk(s) dans %s\n", prefixe, n, as.integer(chunk_size), n_chunks, dossier))
  # Garde-fou de reprise (sidecar)
  f_meta <- file.path(dossier, prefixe %+% "_chunks_meta.yaml")
  courant <- list(n = as.integer(n), chunk_size = as.integer(chunk_size), seed_base = as.numeric(seed_base), nb_chunks = n_chunks, date = as.character(Sys.Date()))
  motif_chunk <- "^" %+% prefixe %+% "_chunk_[0-9]{4}" %+% gsub(".", "\\.", ext, fixed = TRUE) %+% "$"
  chunks_presents <- list.files(dossier, pattern = motif_chunk)
  if(length(chunks_presents) > 0 || file.exists(f_meta)){
    msg <- verifier_chunks_meta(if(file.exists(f_meta)) yaml::read_yaml(f_meta) else NULL, courant, dossier, prefixe)
    if(!is.null(msg)) stop(msg, call. = FALSE)
  }
  if(!file.exists(f_meta)) yaml::write_yaml(courant, f_meta)   # écrit AVANT le premier chunk ; jamais réécrit s'il concorde
  a_traiter <- seq_len(n_chunks)
  if(!is.null(chunk_range)){
    stopifnot(length(chunk_range) == 2, chunk_range[1] >= 1, chunk_range[2] >= chunk_range[1])
    a_traiter <- a_traiter[a_traiter >= chunk_range[1] & a_traiter <= chunk_range[2]]
    if(verbose) cat(sprintf("  [chunks %s] plage traitée : %d..%d\n", prefixe, chunk_range[1], min(chunk_range[2], n_chunks)))
  }
  fichiers <- file.path(dossier, sprintf("%s_chunk_%04d%s", prefixe, seq_len(n_chunks), ext))
  n_faits <- 0L; t_cum <- 0; lignes_cum <- 0L   # extrapolation (bannière tous les 10 chunks traités dans cette session)
  for(i in a_traiter){
    fichier <- fichiers[i]
    if(file.exists(fichier)){
      if(verbose) cat(sprintf("  chunk %s %04d/%04d : déjà présent, sauté\n", prefixe, i, n_chunks))
      next
    }
    idx <- ((i - 1) * chunk_size + 1):min(i * chunk_size, n)
    t0 <- Sys.time()
    set.seed(seed_base + i)
    res <- purrr::pmap(df[idx, , drop = FALSE], f, ...) |> purrr::list_rbind()
    if(is.null(res) || nrow(res) == 0 || ncol(res) == 0) res <- tibble::tibble(.chunk_vide = logical(0))
    tmp <- fichier %+% ".tmp"
    ecrire(res, tmp); if(!file.rename(tmp, fichier)) stop("pmap_chunks : échec du renommage atomique de " %+% tmp, call. = FALSE)
    d <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if(verbose) cat(sprintf("  chunk %s %04d/%04d : %d lignes (%d entrées) en %.1f s — débit %.0f scénarios/s\n", prefixe, i, n_chunks, nrow(res), length(idx), d, if(d > 0) nrow(res) / d else NA))
    n_faits <- n_faits + 1L; t_cum <- t_cum + d; lignes_cum <- lignes_cum + nrow(res)
    if(verbose && n_faits %% 10L == 0L){
      restant <- sum(!file.exists(fichiers[a_traiter]))
      cat(sprintf("  [chunks %s] %d chunk(s) traités dans cette session (%.1f min) ; restant dans la plage : %d ≈ %.1f min au débit moyen (%.0f scénarios/s)\n",
                  prefixe, n_faits, t_cum / 60, restant, if(n_faits > 0) restant * (t_cum / n_faits) / 60 else NA, if(t_cum > 0) lignes_cum / t_cum else NA))
    }
  }
  if(!assembler) return(invisible(NULL))
  manquants <- fichiers[!file.exists(fichiers)]
  if(length(manquants) > 0) stop("pmap_chunks : assemblage impossible, chunks manquants : " %+% paste(basename(manquants), collapse = ", "), call. = FALSE)
  out <- purrr::map(fichiers, function(fi) tibble::as_tibble(lire(fi))) |> purrr::list_rbind()
  if(".chunk_vide" %in% names(out)) out$.chunk_vide <- NULL
  if(!garder_chunks) unlink(fichiers)
  out
}

# Relecture des chunks d'un dossier par lots (finalisation en flux) : applique f(df_lot, i_lot)
lire_chunks_par_lots <- function(dossier, prefixe, taille_lot, f, ext = ".parquet", lire = arrow::read_parquet){
  motif <- "^" %+% prefixe %+% "_chunk_[0-9]{4}" %+% gsub(".", "\\.", ext, fixed = TRUE) %+% "$"
  fichiers <- sort(list.files(dossier, pattern = motif, full.names = TRUE))
  if(length(fichiers) == 0) return(invisible(0L))
  lots <- split(fichiers, ceiling(seq_along(fichiers) / taille_lot))
  for(i in seq_along(lots)){
    d <- purrr::map(lots[[i]], function(fi) tibble::as_tibble(lire(fi))) |> purrr::list_rbind()
    if(".chunk_vide" %in% names(d)) d$.chunk_vide <- NULL
    f(d, i); rm(d); gc()
  }
  invisible(length(lots))
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
# ans_courts / refs_courts (chantier « courts en campagnes » §1bis) : les refs du tirable courts exigent prep_data (et
# prep_das_chronique pour les chroniques) des années ANS_COURTS ; les autres refs, AN_REF seule. La résolution des besoins
# l'assure : annees_a_preparer et annees_das_chronique.
resoudre_besoins <- function(types_etbs, ans, an_ref, fichiers_partiels, fichiers_exports,
                             forcer_refs = FALSE, noms_refs, refs_chroniques, ans_courts = an_ref, refs_courts = character(0)){
  it <- data.frame(etbs = rep(types_etbs, each = length(ans)), an = rep(as.integer(ans), times = length(types_etbs)),
                   stringsAsFactors = FALSE)
  it$fichier <- nom_partiel(it$etbs, it$an)
  it$a_faire <- !(it$fichier %in% basename(fichiers_partiels))
  refs <- data.frame(nom = noms_refs, fichier = nom_ref(noms_refs), stringsAsFactors = FALSE)
  refs$a_faire <- isTRUE(forcer_refs) | !(refs$fichier %in% basename(fichiers_exports))
  courts <- refs$nom %in% refs_courts
  annees <- sort(unique(c(it$an[it$a_faire], if(any(refs$a_faire & !courts)) as.integer(an_ref), if(any(refs$a_faire & courts)) as.integer(ans_courts))))
  chro <- refs$a_faire & refs$nom %in% refs_chroniques
  annees_chro <- sort(unique(c(if(any(chro & !courts)) as.integer(an_ref), if(any(chro & courts)) as.integer(ans_courts))))
  list(iterations = it, refs = refs, annees_a_preparer = as.integer(annees),
       prep_das_chronique = length(annees_chro) > 0, annees_das_chronique = as.integer(annees_chro),
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
  cat("prep_das_chronique            : ", if(plan$prep_das_chronique) paste(plan$annees_das_chronique, collapse = ", ") else "non", "\n")
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
  v <- verifier_magasin("partiels", existant, courant)
  erreur <- if(v$ok) NULL else v$message
  av <- character(0)
  for(ch in CLES_PARTIELS_AVERTISSEMENT) if(!meme(ch)) av <- c(av, "00_partiels/_meta.yaml : " %+% detail(ch))
  list(erreur = erreur, avertissements = av)
}

# Ligne d'instrumentation d'un partiel SEUL (P2 : plus d'accumulateur) : lignes, sum(n)
# (= séjours éligibles), diag2 distincts, diag2 nouveaux par rapport au set déjà vu.
apports_partiel <- function(etbs, an, statut, df_partiel, diag2_vus){
  d <- unique(df_partiel$diag2)
  data.frame(etbs = etbs, an = as.integer(an), statut = statut,
             nb_lignes_partiel = nrow(df_partiel), sum_n_partiel = sum(df_partiel$n),
             nb_diag2_partiel = length(d), nb_diag2_nouveaux = length(setdiff(d, diag2_vus)),
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

# selection/_meta.yaml : la sélection/les chunks ne sont valides que pour ces paramètres.
verifier_meta_tirage <- function(existant, courant, cles){
  if(is.null(existant)) return(NULL)
  diff <- cles[vapply(cles, function(ch) !identical(as.character(unlist(existant[[ch]])), as.character(unlist(courant[[ch]]))), logical(1))]
  if(length(diff) == 0) return(NULL)
  sprintf("selection/_meta.yaml : paramètres différents de la sélection figée (%s). Ouvrez une nouvelle campagne (CAMPAGNE <- \"Cn\") ou videz 40_campagnes/<CAMPAGNE>/ (sélection, chunks, habillé) avant de relancer.",
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
  tibble::tibble(branche = branche, id_scenario = col("id_scenario", NA_character_), cmd = substr(df$ghm2, 1, 2), ghm2 = df$ghm2,
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

## ---- D. Mémoire 15 GiB : collapse vectorisé, agrégation des partiels, recouvrement, mesure ----

# Collapse séjour -> graine (diagnostic_associes) sur la table top-k collectée (colonnes ident,
# pivots, das ; au plus k lignes par ident). k == 2 : chemin vectorisé (arrange + match, aucun
# summarise par groupe) ; sinon repli générique summarise + paste0 (non utilisé en production,
# K_GRAINE_LONGS = 2). Reproduit exactement l'ancien enchaînement group_by(ident, pivots) |>
# arrange(das) |> summarise(paste0(das, collapse = " ")) : ordre C des codes, NA -> "NA".
# Le collapse est PAR SÉJOUR : un ident a une seule cage, donc le morcelage par cage ne coupe
# jamais un séjour (invariant vérifié en test).
collapse_graine <- function(df, pivots, k){
  cols_out <- c(pivots, "diagnostic_associes", "n")
  if(nrow(df) == 0){
    out <- df[0, pivots, drop = FALSE]; out$diagnostic_associes <- character(0); out$n <- integer(0)
    return(tibble::as_tibble(out))
  }
  if(k == 2){
    df <- df |> dplyr::arrange(ident, das)
    premier <- !duplicated(df$ident)
    p <- df[premier, , drop = FALSE]
    second <- df[!premier, c("ident", "das"), drop = FALSE]
    das2 <- second$das[match(p$ident, second$ident)]
    p$diagnostic_associes <- ifelse(is.na(das2), paste(p$das), paste(p$das, das2))
    p |> dplyr::summarise(n = dplyr::n(), .by = dplyr::all_of(c(pivots, "diagnostic_associes")))
  } else {
    df |>
      dplyr::group_by_at(c("ident", pivots)) |>
      dplyr::arrange(das) |>
      dplyr::summarise(diagnostic_associes = paste0(das, collapse = " "), .groups = "drop") |>
      dplyr::ungroup() |>
      dplyr::summarise(n = dplyr::n(), .by = dplyr::all_of(c(pivots, "diagnostic_associes")))
  }
}

# Agrégation de partiels parquet (même schéma) : sum(col_n) par cols, avec semi-jointure
# optionnelle sur filtre_cles (data.frame de clés). Deux chemins : (a) arrow::open_dataset
# (production, mémoire bornée par le moteur arrow) ; (b) pur R incrémental, un partiel à la
# fois, fusion successive (référence sémantique ; utilisé quand arrow est le mock des tests).
arrow_dataset_disponible <- function() requireNamespace("arrow", quietly = TRUE) && "open_dataset" %in% getNamespaceExports("arrow")

agreger_partiels_incremental <- function(fichiers, cols, col_n, filtre_cles = NULL, lire = arrow::read_parquet){
  acc <- NULL
  for(f in fichiers){
    d <- tibble::as_tibble(lire(f))
    if(!is.null(filtre_cles)) d <- dplyr::semi_join(d, filtre_cles, by = names(filtre_cles))
    d <- reagreger(d[, c(cols, col_n), drop = FALSE], cols, col_n)
    acc <- if(is.null(acc)) d else reagreger(dplyr::bind_rows(acc, d), cols, col_n)
    rm(d)
  }
  if(is.null(acc)){
    acc <- tibble::as_tibble(lire(fichiers[1]))[0, c(cols, col_n), drop = FALSE]
  }
  acc
}

agreger_partiels_arrow <- function(fichiers, cols, col_n, filtre_cles = NULL){
  ds <- arrow::open_dataset(fichiers)
  if(!is.null(filtre_cles)) ds <- dplyr::semi_join(ds, filtre_cles, by = names(filtre_cles))
  # symboles explicites : le pronom .data[[ ]] et across(all_of()) ne sont pas traduits par arrow
  res <- ds |> dplyr::group_by(!!!dplyr::syms(cols)) |>
    dplyr::summarise(.n_tmp = sum(!!dplyr::sym(col_n), na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |> tibble::as_tibble()
  res$.n_tmp <- as.integer(res$.n_tmp)
  names(res)[names(res) == ".n_tmp"] <- col_n
  res
}

agreger_partiels <- function(fichiers, cols, col_n, filtre_cles = NULL, chemin = c("auto", "arrow", "incremental")){
  chemin <- match.arg(chemin)
  if(length(fichiers) == 0) stop("agreger_partiels : aucun fichier")
  if(chemin == "auto") chemin <- if(arrow_dataset_disponible()) "arrow" else "incremental"
  if(chemin == "arrow"){
    res <- tryCatch(agreger_partiels_arrow(fichiers, cols, col_n, filtre_cles),
                    error = function(e){ warning("agreger_partiels : chemin arrow en échec (" %+% conditionMessage(e) %+% "), repli incrémental", call. = FALSE); NULL })
    if(!is.null(res)) return(res |> dplyr::arrange(dplyr::across(dplyr::all_of(cols))))
  }
  agreger_partiels_incremental(fichiers, cols, col_n, filtre_cles) |> dplyr::arrange(dplyr::across(dplyr::all_of(cols)))
}

# Clés pivot BRUTES contribuant à une clé pivot retenue (après conversion E669 de diag2) :
# diag2 hors E669 -> identité ; E669 suffixé -> sa cible ; E669 nu -> retenu si AU MOINS une
# de ses cibles de redistribution (cascade sur cage/sexe) est retenue.
cles_brutes_retenues <- function(pivots_bruts, cles_retenues, cols, dist, defaut = "0"){
  if(nrow(pivots_bruts) == 0) return(pivots_bruts[0, cols, drop = FALSE])
  cle <- function(d) do.call(paste, c(lapply(cols, function(p) as.character(d[[p]])), sep = "\r"))
  ret <- unique(cle(cles_retenues[, cols, drop = FALSE]))
  b <- pivots_bruts[, cols, drop = FALSE]
  b_conv <- b; b_conv$diag2 <- convertir_e669(b$diag2)
  garde <- cle(b_conv) %in% ret
  nu <- which(est_e669_nu(b$diag2))
  a_cage <- "cage" %in% cols; a_sexe <- "sexe" %in% cols
  for(i in nu){
    cl <- classes_e660(dist, if(a_cage) as.character(b$cage[i]) else NA, if(a_sexe) as.character(b$sexe[i]) else NA, defaut)
    cibles <- b[rep(i, nrow(cl)), , drop = FALSE]; cibles$diag2 <- cl$code
    garde[i] <- any(cle(cibles) %in% ret)
  }
  dplyr::distinct(b[garde, , drop = FALSE])
}

# Effectifs E669 (suffixé / nu) portés par les graines d'une table de combinaisons
effectif_e669_combos <- function(df, col_combo, col_n){
  if(nrow(df) == 0) return(list(suffixe = 0, nu = 0))
  das <- split_das(df[[col_combo]])
  list(suffixe = sum(df[[col_n]] * vapply(das, function(v) sum(grepl("^E669.", v)), integer(1))),
       nu = sum(df[[col_n]] * vapply(das, function(v) sum(v == "E669"), integer(1))))
}

# Impact de la conversion au niveau des tables PIVOT (petites) : profils > seuil avant / après,
# entrés / sortis (clés avant exprimées avec diag2 converti par suffixe), effectifs E669 diag2.
impact_conversion_pivots <- function(pivots_bruts, pivots_convertis, cols, seuil, col_n = "n"){
  seuil_cles <- function(d){
    s <- reagreger(d[, c(cols, col_n), drop = FALSE], cols, col_n); s <- s[s[[col_n]] > seuil, , drop = FALSE]
    unique(do.call(paste, c(lapply(cols, function(p) if(p == "diag2") convertir_e669(s[[p]]) else as.character(s[[p]])), sep = "\r")))
  }
  k_avant <- seuil_cles(pivots_bruts); k_apres <- seuil_cles(pivots_convertis)
  list(n_total_avant = sum(pivots_bruts[[col_n]]), n_total_apres = sum(pivots_convertis[[col_n]]),
       e669_diag2_suffixe = sum(pivots_bruts[[col_n]][grepl("^E669.", pivots_bruts$diag2)]),
       e669_diag2_nu = sum(pivots_bruts[[col_n]][est_e669_nu(pivots_bruts$diag2)]),
       profils_seuil_avant = length(k_avant), profils_seuil_apres = length(k_apres),
       profils_entres = length(setdiff(k_apres, k_avant)), profils_sortis = length(setdiff(k_avant, k_apres)))
}

# Recouvrement entre deux partiels A (référence) et B (candidat) : combinaisons (pivots ×
# diagnostic_associes), pivots seuls, diag2 nouveaux. Parts en proportion de B.
recouvrement_partiels <- function(A, B, pivots, col_combo = "diagnostic_associes", col_n = "n"){
  cle <- function(d, cols) do.call(paste, c(lapply(cols, function(p) as.character(d[[p]])), sep = "\r"))
  cA <- cle(A, c(pivots, col_combo)); cB <- cle(B, c(pivots, col_combo))
  communes <- cB %in% cA
  pA <- reagreger(A[, c(pivots, col_n)], pivots, col_n); pB <- reagreger(B[, c(pivots, col_n)], pivots, col_n)
  piv_communs <- cle(pB, pivots) %in% cle(pA, pivots)
  data.frame(nb_A = nrow(A), nb_B = nrow(B), nb_communes = sum(communes),
             part_combos_B_vues = if(nrow(B)) sum(communes) / nrow(B) else NA,
             part_sejours_B_vus = if(sum(B[[col_n]])) sum(B[[col_n]][communes]) / sum(B[[col_n]]) else NA,
             nb_pivots_A = nrow(pA), nb_pivots_B = nrow(pB), nb_pivots_communs = sum(piv_communs),
             part_pivots_B_vus = if(nrow(pB)) sum(piv_communs) / nrow(pB) else NA,
             part_sejours_pivots_B_vus = if(sum(pB[[col_n]])) sum(pB[[col_n]][piv_communs]) / sum(pB[[col_n]]) else NA,
             nb_diag2_B_nouveaux = length(setdiff(unique(B$diag2), unique(A$diag2))),
             sejours_B_uniques_nouveaux = sum(B[[col_n]][!communes]))
}

# Instrumentation mémoire : une ligne par mesure (étiquette, horodatage, taille de l'objet,
# mémoire utilisée et pic gc() depuis la mesure précédente, en Go) ; avertissement visible
# si le pic dépasse seuil_alerte_go. Retourne le journal augmenté (data.frame).
mesurer_memoire <- function(etiquette, objet = NULL, journal = NULL, seuil_alerte_go = 10, verbose = TRUE){
  g <- gc(reset = TRUE)   # colonnes : used, (Mb), gc trigger, (Mb), [limit (Mb),] max used, (Mb) -> dernière colonne = pic en Mb
  taille_mo <- if(is.null(objet)) NA_real_ else as.numeric(utils::object.size(objet)) / 1024^2
  utilise_go <- sum(g[, 2]) / 1024
  pic_go <- sum(g[, ncol(g)]) / 1024
  ligne <- data.frame(etiquette = etiquette, horodatage = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      taille_objet_mo = round(taille_mo, 1), memoire_utilisee_go = round(utilise_go, 3),
                      pic_go = round(pic_go, 3), alerte = pic_go > seuil_alerte_go, stringsAsFactors = FALSE)
  if(verbose) cat(sprintf("  [mémoire] %-40s objet = %8s Mo ; utilisé = %6.2f Go ; pic = %6.2f Go%s\n", etiquette,
                          if(is.na(taille_mo)) "-" else format(round(taille_mo, 1)), utilise_go, pic_go,
                          if(pic_go > seuil_alerte_go) "  <<< AVERTISSEMENT : pic > SEUIL_ALERTE_GO" else ""))
  if(pic_go > seuil_alerte_go) warning(sprintf("Pic mémoire %.2f Go > SEUIL_ALERTE_GO (%s) à l'étape « %s »", pic_go, seuil_alerte_go, etiquette), call. = FALSE)
  rbind(journal, ligne)
}

## ---- E. Aval production : typologie, catalogue partitionné, sélection quota_dp_fixe, index, mémoire ----

# --- E1. Typologie DPEC / TPEC ------------------------------------------------------------
charger_typologie <- function(chemin){
  t <- yaml::read_yaml(chemin)
  stopifnot(!is.null(t$version))
  for(k in c("RACINES_GREFFES_CART", "RACINES_TRANSPLANT", "RACINES_IMG_FC", "GHM_ACC_NORMAL", "RACINES_ACC_PATHO",
             "GHM_BB_NORMAL", "RACINES_BB_MED", "RACINES_BB_CHIR", "RACINES_AUTRE_NEONAT")) t[[k]] <- as.character(unlist(t[[k]]))
  t$DPEC_TO_TPEC <- unlist(t$DPEC_TO_TPEC)
  t
}

# Traduction fidèle du STREAM (with_typologie, polars) en case_when ; ORDRE = précédence : séjours
# complexes (CMD 27 CAR-T / transplantations, CMD 22 brûlés) ; obstétrique (IVG 14Z08Z, IMG/FC,
# accouchement normal, ACC_PATHO & sévérité hors A/T) ; néonat (BB_NORMAL, BB_MED, BB_CHIR,
# AUTRE_NEONAT) ; séances CMD 28 (polysomno Z04801, chimio Z511 & adulte, simples) ; médecine M/Z
# (HDJ si HP, puis duree >= 3 / < 3) ; chirurgie C / interventionnel K (même borne) ; sinon "Autre".
# racine = substr(ghm2, 1, 5) (identique à la colonne racine de prep_data). col_age : numérique
# (agean >= 18) ou classe "ge_18"/"lt_18" (pivot age des longs). Catalogue longs : duree_defaut = 3
# (périmètre 3-100 par construction ; classes < 3 nuits / HDJ / séances inaccessibles, attendu) ;
# les courts seront typés avec leur vraie durée.
typologie_sejour <- function(df, typo, col_age = "age", col_duree = "duree", col_mode = "mode_hospit", duree_defaut = NA){
  ghm2 <- as.character(df$ghm2)
  cmd <- substr(ghm2, 1, 2); type_ghm <- substr(ghm2, 3, 3); racine <- substr(ghm2, 1, 5); sev <- substr(ghm2, nchar(ghm2), nchar(ghm2))
  age <- df[[col_age]]
  adulte <- if(is.numeric(age)) !is.na(age) & age >= 18 else !is.na(age) & as.character(age) == "ge_18"
  duree <- if(col_duree %in% names(df)) as.numeric(df[[col_duree]]) else rep(as.numeric(duree_defaut), nrow(df))
  hdj <- if(col_mode %in% names(df)) as.character(df[[col_mode]]) == "HP" else rep(FALSE, nrow(df))
  dp <- as.character(df$diag2)
  dpec <- dplyr::case_when(
    # --- Séjours complexes (CMD 27, 22)
    racine %in% typo$RACINES_GREFFES_CART ~ "Greffes de moelle, CAR-T Cells",
    racine %in% typo$RACINES_TRANSPLANT ~ "Transplantations",
    cmd == "22" ~ "Brûlés",   # critère à confirmer (CMD 22), comme dans STREAM
    # --- Obstétrique
    ghm2 == "14Z08Z" ~ "IVG",
    racine %in% typo$RACINES_IMG_FC ~ "IMG & fausses couches",
    ghm2 %in% typo$GHM_ACC_NORMAL ~ "Accouchement normal mère",
    racine %in% typo$RACINES_ACC_PATHO & !sev %in% c("A", "T") ~ "Accouchement pathologique mère",
    # --- Néonatalogie
    ghm2 %in% typo$GHM_BB_NORMAL ~ "Bébé normal",
    racine %in% typo$RACINES_BB_MED ~ "Bébé néonat med",
    racine %in% typo$RACINES_BB_CHIR ~ "Bébé néonat chir",
    racine %in% typo$RACINES_AUTRE_NEONAT ~ "Autre néonat",
    # --- Médecine : séances (les spécifiques avant le tout-venant CMD 28)
    cmd == "28" & dp == "Z04801" ~ "Séance polysomno",
    cmd == "28" & dp == "Z511" & adulte ~ "Séance chimiothérapie simple adulte",
    cmd == "28" ~ "Séances simples",
    # --- Médecine hors séances (HDJ d'abord, puis durée ; 3 nuits et plus => « > 3 nuits »)
    hdj & type_ghm %in% c("M", "Z") ~ "HDJ médecine adultes",
    !is.na(duree) & duree >= 3 & type_ghm %in% c("M", "Z") ~ "Médecine adultes > 3 nuits",
    !is.na(duree) & duree < 3 & type_ghm %in% c("M", "Z") ~ "Médecine adultes < 3 nuits",
    # --- Chirurgie et interventionnel (même borne à 3)
    !is.na(duree) & duree < 3 & type_ghm == "K" ~ "Interventionnel adultes < 3 nuits",
    !is.na(duree) & duree < 3 & type_ghm == "C" ~ "Chirurgie adultes < 3 nuits",
    !is.na(duree) & duree >= 3 & type_ghm == "K" ~ "Interventionnel adultes > 3 nuits",
    !is.na(duree) & duree >= 3 & type_ghm == "C" ~ "Chirurgie adultes > 3 nuits",
    TRUE ~ "Autre")
  tpec <- unname(typo$DPEC_TO_TPEC[dpec]); tpec[is.na(tpec)] <- "Autre"
  df$DPEC <- dpec; df$TPEC <- tpec
  df
}

# --- E2. Catalogue partitionné par lettre ---------------------------------------------------
# Disposition : <dir>/part_<L>.parquet (colonne `lettre` = substr(diag2, 1, 1) dans chaque part),
# méta <dir>/_meta.yaml (ignoré par arrow::open_dataset : préfixe "_").
lettre_de <- function(diag2) substr(as.character(diag2), 1, 1)
nom_part_lettre <- function(lettre) sprintf("part_%s.parquet", lettre)

# Lecteur UNIQUE du catalogue : dataset arrow filtré (lettres, colonnes) ; mock arrow (sans
# open_dataset) : rbind des parts demandées ; monofichier : lecture dépréciée avec message.
lire_catalogue <- function(dir_dataset, monofichier = NULL, lettres = NULL, colonnes = NULL){
  if(dir.exists(dir_dataset) && length(list.files(dir_dataset, pattern = "^part_.*\\.parquet$")) > 0){
    parts <- list.files(dir_dataset, pattern = "^part_.*\\.parquet$", full.names = TRUE)
    if(!is.null(lettres)) parts <- parts[sub("^part_(.*)\\.parquet$", "\\1", basename(parts)) %in% lettres]
    if(length(parts) == 0) return(NULL)
    if(arrow_dataset_disponible()){
      ds <- arrow::open_dataset(parts)
      if(!is.null(colonnes)) ds <- dplyr::select(ds, dplyr::all_of(unique(c(colonnes))))
      return(tibble::as_tibble(dplyr::collect(ds)))
    }
    out <- purrr::map(parts, function(p){ d <- tibble::as_tibble(arrow::read_parquet(p)); if(!is.null(colonnes)) d[, unique(colonnes), drop = FALSE] else d }) |> purrr::list_rbind()
    return(out)
  }
  if(!is.null(monofichier) && file.exists(monofichier)){
    message("lire_catalogue : lecture du MONOFICHIER ", basename(monofichier), " (déprécié : lancez etape_repartitionner_catalogue() pour un catalogue partitionné par lettre).")
    d <- tibble::as_tibble(arrow::read_parquet(monofichier))
    let <- if("lettre" %in% names(d)) as.character(d$lettre) else lettre_de(d$diag2)
    if(!is.null(lettres)) d <- d[let %in% lettres, , drop = FALSE]
    if(!is.null(colonnes) && "lettre" %in% colonnes && !"lettre" %in% names(d)) d$lettre <- lettre_de(d$diag2)
    if(!is.null(colonnes)) d <- d[, unique(colonnes), drop = FALSE]
    return(d)   # schéma du monofichier inchangé (pas de colonne lettre ajoutée d'office)
  }
  stop(message_catalogue_absent("lire_catalogue", dirname(dir_dataset)), call. = FALSE)
}
lettres_catalogue <- function(dir_dataset, monofichier = NULL){
  if(dir.exists(dir_dataset)){
    p <- list.files(dir_dataset, pattern = "^part_.*\\.parquet$")
    if(length(p) > 0) return(sort(sub("^part_(.*)\\.parquet$", "\\1", p)))
  }
  if(!is.null(monofichier) && file.exists(monofichier)) return(sort(unique(lettre_de(lire_catalogue(dir_dataset, monofichier, colonnes = "diag2")$diag2))))
  character(0)
}

# --- E3. Sélection quota_dp_fixe ----------------------------------------------------------
# Partition des cages : exhaustive et exclusive, sinon stop
verifier_populations <- function(populations, cages){
  toutes <- unlist(populations)
  dup <- unique(toutes[duplicated(toutes)]); manq <- setdiff(cages, toutes)
  if(length(dup) > 0 || length(manq) > 0)
    stop("POPULATIONS : partition des cages invalide", if(length(dup)) " ; en double : " %+% paste(dup, collapse = ", ") else "",
         if(length(manq)) " ; absentes : " %+% paste(manq, collapse = ", ") else "", call. = FALSE)
  invisible(TRUE)
}
population_de <- function(cage, populations){
  m <- stats::setNames(rep(names(populations), lengths(populations)), unlist(populations))
  unname(m[as.character(cage)])
}
# Budget par population au prorata du nb de DP (arrondi, dernier ajusté pour totaliser)
repartir_budget_populations <- function(budget, nb_dp){
  if(sum(nb_dp) == 0) return(stats::setNames(rep(0L, length(nb_dp)), names(nb_dp)))
  b <- round(budget * nb_dp / sum(nb_dp)); b[length(b)] <- budget - sum(b[-length(b)])
  stats::setNames(as.integer(b), names(nb_dp))
}
# Seed stable par (population, lettre)
seed_selection <- function(seed, population, lettre, populations) as.integer(seed + 7e6 + 1e4 * match(population, names(populations)) + utf8ToInt(substr(lettre, 1, 1)))

# Choix de k lignes distinctes d'un groupe, au poids, sans remise ; planchers par type d'unité
# seulement si k >= nb de types présents (sinon désactivés, mention au rapport).
choisir_lignes_dp <- function(d, k, col_poids = "poids", col_unite = "type_unite"){
  k_eff <- min(k, nrow(d))
  if(k_eff <= 0) return(list(lignes = d[0, , drop = FALSE], planchers = FALSE))
  types <- unique(as.character(d[[col_unite]]))
  planchers <- length(types) > 1 && k_eff >= length(types)
  if(planchers){
    idx <- integer(0)
    for(u in types){ cand <- which(d[[col_unite]] == u); idx <- c(idx, if(length(cand) == 1) cand else cand[sample.int(length(cand), 1, prob = d[[col_poids]][cand])]) }
    reste <- setdiff(seq_len(nrow(d)), idx)
    if(k_eff - length(idx) > 0 && length(reste) > 0) idx <- c(idx, if(length(reste) == 1) reste else reste[sample.int(length(reste), k_eff - length(idx), prob = d[[col_poids]][reste])])
  } else {
    idx <- if(nrow(d) == 1) 1L else sample.int(nrow(d), k_eff, prob = d[[col_poids]])
  }
  list(lignes = d[idx, , drop = FALSE], planchers = planchers)
}
# Variantes : n_var = ceiling(X / k_eff) par ligne, dernière ligne tronquée pour totaliser X
variantes_par_ligne <- function(k_eff, X){
  if(k_eff == 0 || X <= 0) return(integer(0))
  n_var <- as.integer(ceiling(X / k_eff)); v <- rep(n_var, k_eff)
  v[k_eff] <- as.integer(X - n_var * (k_eff - 1))
  v
}
# Sélection d'UNE population sur UNE lettre (df déjà filtré) : X par DP (plafonds DPEC par
# (DP × DPEC plafonné), le reste du DP suit X), k lignes distinctes au poids sans remise,
# n_var variantes par ligne. Retourne list(selection, stats).
selection_quota_dp_fixe_lettre <- function(df, X, k, plafonds_dpec = list(), col_dp = "diag2", col_dpec = "DPEC", col_poids = "poids", col_unite = "type_unite"){
  if(nrow(df) == 0) return(list(selection = df[0, ], stats = NULL))
  df$.grp <- ifelse(df[[col_dpec]] %in% names(plafonds_dpec), df[[col_dpec]], ".reste")
  cles <- unique(df[, c(col_dp, ".grp"), drop = FALSE])
  out <- vector("list", nrow(cles)); st <- vector("list", nrow(cles))
  for(i in seq_len(nrow(cles))){
    d <- df[df[[col_dp]] == cles[[col_dp]][i] & df$.grp == cles$.grp[i], , drop = FALSE]
    X_dp <- if(cles$.grp[i] == ".reste") X else min(X, as.integer(plafonds_dpec[[cles$.grp[i]]]))
    ch <- choisir_lignes_dp(d, k, col_poids, col_unite)
    nv <- variantes_par_ligne(nrow(ch$lignes), X_dp)
    l <- ch$lignes; l$n_var <- nv; l <- l[l$n_var > 0, , drop = FALSE]
    out[[i]] <- l
    st[[i]] <- data.frame(dp = cles[[col_dp]][i], groupe = cles$.grp[i], lignes_disponibles = nrow(d), X_dp = X_dp, k_eff = nrow(ch$lignes),
                          variantes = sum(nv), planchers_actifs = ch$planchers, plafonne = cles$.grp[i] != ".reste",
                          manque_a_gagner = max(0L, X_dp - nrow(d)), stringsAsFactors = FALSE)
  }
  sel <- dplyr::bind_rows(out); sel$.grp <- NULL
  list(selection = sel, stats = dplyr::bind_rows(st))
}

# Dédoublonnage souple des variantes d'une même ligne sur le jeu complet de DAS (ordre indifférent)
dedoublonner_variantes <- function(df, col_combo = "diagnostic_associes"){
  if(nrow(df) <= 1) return(df)
  cle <- vapply(split_das(df[[col_combo]]), function(v) paste(sort(unique(v), method = "radix"), collapse = " "), character(1))
  df[!duplicated(cle), , drop = FALSE]
}

# --- E4. Index des tables de référence (accès direct au lieu du filtre) ------------------
cle_strate <- function(...) do.call(paste, c(lapply(list(...), as.character), sep = "\r"))
indexer_ref_das <- function(ref){
  cle <- cle_strate(ref$diag2, ref$mode_hospit, ref$sexe, ref$cage, ref$ghm2)
  idx <- split(ref, cle)
  structure(idx, class = c("index_ref_das", "list"))
}
indexer_ref_chronique <- function(ref_chro){
  list(strate = structure(split(ref_chro$strate, cle_strate(ref_chro$strate$diag2, ref_chro$strate$sexe, ref_chro$strate$cage)), class = c("index_ref_chro", "list")),
       repli  = structure(split(ref_chro$repli, cle_strate(ref_chro$repli$sexe, ref_chro$repli$cage)), class = c("index_ref_chro", "list")),
       colonnes_strate = names(ref_chro$strate), colonnes_repli = names(ref_chro$repli))
}
vide_comme <- function(cols) tibble::as_tibble(stats::setNames(replicate(length(cols), character(0), simplify = FALSE), cols))

# --- E5. Mémoire de session -------------------------------------------------------------
memoire_session <- function(envs = list(globalenv = globalenv(), ETAPES_ENV = if(exists("ETAPES_ENV")) ETAPES_ENV else NULL, CACHE_E669 = if(exists("CACHE_E669")) CACHE_E669 else NULL), n_max = 30){
  rows <- list()
  for(nom_env in names(envs)){
    e <- envs[[nom_env]]; if(is.null(e)) next
    for(o in ls(e, all.names = TRUE)){
      obj <- get(o, envir = e)
      rows[[length(rows) + 1]] <- data.frame(env = nom_env, objet = o, classe = class(obj)[1], taille_mo = round(as.numeric(utils::object.size(obj)) / 1024^2, 2), stringsAsFactors = FALSE)
    }
  }
  if(length(rows) == 0) return(data.frame(env = character(0), objet = character(0), classe = character(0), taille_mo = numeric(0)))
  d <- do.call(rbind, rows); d <- d[order(-d$taille_mo), ]; rownames(d) <- NULL
  print(utils::head(d, n_max), row.names = FALSE)
  g <- gc(); cat(sprintf("gc() : utilisé %.2f Go ; pic %.2f Go (le RSS de R ne redescend pas toujours après gc() : Restart R avant une étape lourde)\n", sum(g[, 2]) / 1024, sum(g[, ncol(g)]) / 1024))
  invisible(d)
}

# --- E6. Statistiques accumulées par lot (finalisation en flux) --------------------------
acc_stats_init <- function() list(n = 0L, pivots = NULL, nb_das = NULL, top_das = NULL, e660 = NULL, imprecis_num = 0L, imprecis_den = 0L,
                                  controles = c(doublons_categorie = 0L, diabete_hors_flag = 0L, i10_avec_hta_autres = 0L, poids_sous_seuil = 0L), e669_residuels = 0L)
acc_stats_ajouter <- function(acc, df, pivots, codes_imprecis, hta_autres, seuil_pivot, cols_e669){
  if(nrow(df) == 0) return(acc)
  acc$n <- acc$n + nrow(df)
  acc$pivots <- dplyr::distinct(dplyr::bind_rows(acc$pivots, dplyr::distinct(df[, intersect(pivots, names(df))])))
  das <- split_das(df$diagnostic_associes)
  acc$nb_das <- dplyr::bind_rows(acc$nb_das, tibble::tibble(cage = as.character(df$cage), nb_das = lengths(das)) |> dplyr::count(cage, nb_das, name = "n")) |>
    dplyr::summarise(n = sum(n), .by = c(cage, nb_das))
  acc$top_das <- dplyr::bind_rows(acc$top_das, tibble::tibble(cmd = rep(substr(df$ghm2, 1, 2), lengths(das)), das = unname(unlist(das))) |> dplyr::count(cmd, das, name = "n")) |>
    dplyr::summarise(n = sum(n), .by = c(cmd, das))
  acc$e660 <- dplyr::bind_rows(acc$e660, effectifs_e660(df, c("diag2", "diagnostic_associes"))) |> dplyr::summarise(n = sum(n), .by = code)
  v <- unlist(das); acc$imprecis_num <- acc$imprecis_num + sum(v %in% codes_imprecis); acc$imprecis_den <- acc$imprecis_den + length(v)
  cc <- controler_scenarios(df, hta_autres, seuil_pivot)
  for(k in names(acc$controles)) acc$controles[[k]] <- acc$controles[[k]] + (if(is.na(cc[[k]])) 0L else cc[[k]])
  acc$e669_residuels <- acc$e669_residuels + compter_e669(df, cols_e669)
  acc
}
acc_stats_final <- function(acc, n_top = 30){
  if(acc$n == 0) return(list(n = 0L, pivots = 0L, distribution = NULL, top_das = NULL, taux_imprecis = NA_real_, controles = as.list(acc$controles), e669_residuels = 0L, e660 = NULL))
  nb <- acc$nb_das
  distribution <- nb |> dplyr::summarise(n = sum(n), moy = round(sum(nb_das * n) / sum(n), 2), min = min(nb_das), q50 = { o <- order(nb_das); cs <- cumsum(n[o]); nb_das[o][which(cs >= sum(n) / 2)[1]] }, max = max(nb_das), .by = cage) |> dplyr::arrange(cage)
  top <- acc$top_das |> dplyr::summarise(n = sum(n), .by = c(cmd, das)) |> dplyr::arrange(cmd, dplyr::desc(n), das) |> dplyr::mutate(rang = dplyr::row_number(), .by = cmd) |> dplyr::filter(rang <= n_top)
  list(n = acc$n, pivots = nrow(acc$pivots), distribution = distribution, top_das = top,
       taux_imprecis = if(acc$imprecis_den > 0) round(acc$imprecis_num / acc$imprecis_den, 4) else NA_real_,
       controles = as.list(acc$controles), e669_residuels = acc$e669_residuels, e660 = acc$e660 |> dplyr::arrange(code))
}

## ---- F. Finitions exploitation ----
# (La migration inter-profils — localiser_catalogue, condition_q13, fichiers_migration_catalogue — est RETIRÉE au
#  chantier « livrable unique + arborescence » : les magasins sont partagés entre profils et gardés par verifier_magasin.)
# Message quand le catalogue longs est absent du magasin partagé 20_catalogue/.
message_catalogue_absent <- function(etape, dir_catalogue){
  sprintf(paste0("%s : catalogue absent du magasin partagé %s (ni catalogue_longs_seuil.parquet, ni catalogue_longs_seuil/ partitionné). ",
                 "Lancez etape_catalogue() (extraction, coûteux ; partiels réutilisés s'ils existent) puis etape_repartitionner_catalogue()."), etape, sub("/$", "", dir_catalogue))
}

## ---- G. Campagnes : identifiants stables, registre des tirages, plafonds de classe, sélection sous registre ----

# --- G1. Identifiants stables --------------------------------------------------------------
# RECETTE FIGÉE (version RECETTE_ID) : sha256 de la concaténation, séparateur "\r", des valeurs
# as.character (NA -> "") des colonnes, DANS CET ORDRE : PIVOTS_LONGS = mode_hospit, sexe, age, cage,
# racine, ghm2, diabete, hta, diag2, nbda, type_unite, prep_sc, puis diagnostic_associes ; hex tronqué
# à 16 caractères. Déterministe, indépendant de l'ordre des lignes, recalculable sur tout fichier.
# La changer invaliderait le registre (MODIFICATIONS_V8.md section 18).
RECETTE_ID <- "id_v1"
COLONNES_RECETTE_ID <- c("mode_hospit", "sexe", "age", "cage", "racine", "ghm2", "diabete", "hta", "diag2", "nbda", "type_unite", "prep_sc", "diagnostic_associes")
sha256_vec <- function(x){
  x <- as.character(x)
  if(requireNamespace("openssl", quietly = TRUE)) return(as.character(openssl::sha256(x)))
  if(requireNamespace("digest", quietly = TRUE)) return(unname(digest::getVDigest(algo = "sha256")(x, serialize = FALSE)))
  stop("id_profil : ni openssl ni digest disponible (sha256). Installer l'un des deux (MODIFICATIONS_V8.md, Q37).", call. = FALSE)
}
norm_val <- function(v){ v <- as.character(v); v[is.na(v)] <- ""; v }
id_profil_de <- function(df, colonnes = COLONNES_RECETTE_ID){
  manq <- setdiff(colonnes, names(df)); if(length(manq)) stop("id_profil_de : colonnes manquantes : " %+% paste(manq, collapse = ", "), call. = FALSE)
  if(nrow(df) == 0) return(character(0))
  cle <- do.call(paste, c(lapply(colonnes, function(cc) norm_val(df[[cc]])), sep = "\r"))
  substr(sha256_vec(cle), 1, 16)
}
id_scenario_de <- function(id_profil, variante) paste0(id_profil, "-", sprintf("%03d", as.integer(variante)))
# Empreinte du jeu complet de DAS réellement tiré (graine + complétion + doctrine), ordre indifférent
hash_das_de <- function(diagnostic_associes){
  cle <- vapply(split_das(diagnostic_associes), function(v) paste(sort(unique(v), method = "radix"), collapse = " "), character(1))
  substr(sha256_vec(cle), 1, 16)
}
seed_campagne <- function(seed, campagne) as.integer(seed + 1000L * (sum(utf8ToInt(as.character(campagne))) %% 100000L))
# Identifiants des SÉJOURS COURTS — recette FIGÉE id_courts_v1 (lot « notebook campagnes », §7) : même
# mécanique que id_v1 (sha256, séparateur "\r", NA -> "") sur les PIVOTS_COURTS DANS CET ORDRE :
# mode_hospit, sexe, cage, ghm2, diag2, duree ; id_profil = "k" + 15 hex (préfixe de domaine HORS alphabet
# hexadécimal : l'identifiant dit sa branche à lui seul ; les longs restent 16 hex sans préfixe — Q63 résolue,
# corrigé avant toute circulation). id_scenario = id_profil-variante et hash_das : fonctions communes.
# Chantier « courts en campagnes » : les courts s'inscrivent au registre comme les longs (branche = "court"), tirage par
# campagne à variantes nouvelles ; l'id d'un pivot est un hash de contenu, stable sous extension du tirable (ANS_COURTS).
RECETTE_ID_COURTS <- "id_courts_v1"
COLONNES_RECETTE_ID_COURTS <- c("mode_hospit", "sexe", "cage", "ghm2", "diag2", "duree")
id_profil_courts_de <- function(df, colonnes = COLONNES_RECETTE_ID_COURTS){
  manq <- setdiff(colonnes, names(df)); if(length(manq)) stop("id_profil_courts_de : colonnes manquantes : " %+% paste(manq, collapse = ", "), call. = FALSE)
  if(nrow(df) == 0) return(character(0))
  cle <- do.call(paste, c(lapply(colonnes, function(cc) norm_val(df[[cc]])), sep = "\r"))
  paste0("k", substr(sha256_vec(cle), 1, 15))
}

# --- G2. Registre des tirages (append-only) ------------------------------------------------
# Colonne `branche` AJOUTÉE (chantier « courts en campagnes ») : "long" / "court" ; les registres écrits avant portent
# branche = "long" implicite à la relecture. Un seul fichier registre_<C>.parquet par campagne, les deux branches.
COLONNES_REGISTRE <- c("id_profil", "variante", "id_scenario", "hash_das", "campagne", "population", "diag2", "DPEC", "date", "branche")
nom_registre <- function(campagne) sprintf("registre_%s.parquet", campagne)
registre_vide <- function(){ v <- tibble::as_tibble(stats::setNames(replicate(length(COLONNES_REGISTRE), character(0), simplify = FALSE), COLONNES_REGISTRE)); v$variante <- integer(0); v }
normaliser_registre <- function(df){
  df <- tibble::as_tibble(df)
  if(!"branche" %in% names(df)) df$branche <- rep("long", nrow(df)) else { df$branche <- as.character(df$branche); df$branche[is.na(df$branche)] <- "long" }
  df$variante <- as.integer(df$variante)
  for(cc in setdiff(COLONNES_REGISTRE, "variante")) df[[cc]] <- as.character(df[[cc]])
  df[, COLONNES_REGISTRE] |> dplyr::arrange(branche, id_profil, variante)
}
# Écrit registre_<campagne>.parquet ; APPEND-ONLY : idempotent si le contenu est identique (hors date) ; EXTENSION acceptée
# seulement si toutes les lignes existantes sont conservées à l'identique et que les ajouts sont d'une branche absente du
# fichier (ex. courts adoptés après une rétro-inscription des longs) ; toute autre différence -> stop, rien réécrit.
ecrire_registre_campagne <- function(df, campagne, dir_registre, ecrire = arrow::write_parquet, lire = arrow::read_parquet){
  manq <- setdiff(setdiff(COLONNES_REGISTRE, "branche"), names(df)); if(length(manq)) stop("registre : colonnes manquantes : " %+% paste(manq, collapse = ", "), call. = FALSE)
  df <- normaliser_registre(df)
  if(!dir.exists(dir_registre)) dir.create(dir_registre, recursive = TRUE)
  f <- file.path(dir_registre, nom_registre(campagne))
  if(file.exists(f)){
    ex <- normaliser_registre(lire(f)); cols <- setdiff(COLONNES_REGISTRE, "date")
    cle <- function(x) do.call(paste, c(lapply(cols, function(cc) norm_val(x[[cc]])), sep = "\r"))
    br_ex <- unique(ex$branche); df_vues <- df[df$branche %in% br_ex, , drop = FALSE]; nouvelles <- df[!df$branche %in% br_ex, , drop = FALSE]
    # branches déjà présentes : le contenu fourni pour ces branches doit être EXACTEMENT celui du fichier ; branches absentes : ajoutées
    if(nrow(df_vues) > 0 && !identical(sort(cle(df_vues)), sort(cle(ex[ex$branche %in% unique(df_vues$branche), , drop = FALSE]))))
      stop("registre : " %+% basename(f) %+% " existe déjà avec un contenu DIFFÉRENT (registre append-only, jamais réécrit). Choisissez un autre identifiant de campagne.", call. = FALSE)
    if(nrow(nouvelles) == 0) return(invisible(f))
    ecrire(normaliser_registre(dplyr::bind_rows(ex, nouvelles)), f)
    cat("registre ", basename(f), " : étendu (append-only) — branche ", paste(unique(nouvelles$branche), collapse = ","), " ajoutée : ", nrow(nouvelles), " scénarios ; lignes existantes intactes\n", sep = "")
    return(invisible(f))
  }
  ecrire(df, f); invisible(f)
}
# Lecteur unique du registre : lignes + agrégats (par id_profil : variante_max, nb, hash_das ; par diag2, par DPEC,
# par branche, par campagne × branche). Registre vide -> tables vides. Fichier par fichier (schémas anciens sans
# `branche` relus avec branche = "long" implicite).
lire_registre <- function(dir_registre, lire = arrow::read_parquet){
  fichiers <- if(dir.exists(dir_registre)) sort(list.files(dir_registre, pattern = "^registre_.*\\.parquet$", full.names = TRUE)) else character(0)
  lignes <- if(length(fichiers) == 0) registre_vide() else purrr::map(fichiers, function(f) normaliser_registre(lire(f))) |> purrr::list_rbind()
  par_profil <- if(nrow(lignes) == 0) tibble::tibble(id_profil = character(0), variante_max = integer(0), nb_scenarios = integer(0), hash_das = list()) else
    lignes |> dplyr::summarise(variante_max = max(variante), nb_scenarios = dplyr::n(), hash_das = list(unique(hash_das)), .by = id_profil)
  list(lignes = lignes, fichiers = fichiers, nb_campagnes = length(fichiers), nb_scenarios = nrow(lignes),
       par_profil = par_profil,
       par_diag2 = lignes |> dplyr::summarise(nb_scenarios = dplyr::n(), nb_profils = dplyr::n_distinct(id_profil), .by = diag2),
       par_dpec = lignes |> dplyr::summarise(nb_scenarios = dplyr::n(), nb_profils = dplyr::n_distinct(id_profil), .by = DPEC),
       par_branche = lignes |> dplyr::summarise(nb_scenarios = dplyr::n(), nb_profils = dplyr::n_distinct(id_profil), .by = branche),
       par_campagne = lignes |> dplyr::summarise(nb_scenarios = dplyr::n(), nb_longs = sum(branche == "long"), nb_courts = sum(branche == "court"), .by = campagne))
}

# --- G3. Plafonds de CLASSE DPEC (amendement Q33) -------------------------------------------
# Plafond = total de la classe DPEC, par population. Chaque DP de la classe reçoit d'abord 1 ligne ×
# 1 variante (le représentant prime : si nb_dp > plafond, total = nb_dp, dépassement consigné) ; le
# surplus (plafond - nb_dp) est réparti au poids (plus forts restes) entre les DP de la classe.
# Entrée : lignes de la classe (diag2, poids) ; sortie : tibble(diag2, quota) + résumé.
allocation_classe_plafonnee <- function(df_classe, plafond, col_dp = "diag2", col_poids = "poids"){
  if(nrow(df_classe) == 0) return(list(quotas = tibble::tibble(diag2 = character(0), quota = integer(0)), nb_dp = 0L, plafond = as.integer(plafond), total = 0L, depassement = 0L))
  p <- df_classe |> dplyr::summarise(poids = sum(.data[[col_poids]]), .by = dplyr::all_of(col_dp))
  names(p)[1] <- "diag2"
  nb_dp <- nrow(p); surplus <- max(0L, as.integer(plafond) - nb_dp)
  quota <- rep(1L, nb_dp) + if(surplus > 0) as.integer(repartir_proportionnel(surplus, p$poids)) else 0L
  list(quotas = tibble::tibble(diag2 = p$diag2, quota = quota), nb_dp = nb_dp, plafond = as.integer(plafond), total = sum(quota), depassement = max(0L, nb_dp - as.integer(plafond)))
}

# --- G4. Sélection d'un DP sous registre (fraîcheur d'abord, recyclage à variantes nouvelles) ----
# d : lignes du DP (avec id_profil) ; registre_profil : tibble(id_profil, variante_max, hash_das (list))
# ou NULL (registre inactif). Retourne les k lignes + colonnes origine_profil, variante_debut, hash_exclus.
choisir_lignes_dp_registre <- function(d, k, registre_profil = NULL, col_poids = "poids", col_unite = "type_unite"){
  if(is.null(registre_profil) || nrow(registre_profil) == 0 || !"id_profil" %in% names(d)){
    ch <- choisir_lignes_dp(d, k, col_poids, col_unite)
    l <- ch$lignes; l$origine_profil <- "vierge"; l$variante_debut <- 1L; l$hash_exclus <- ""
    return(list(lignes = l, planchers = ch$planchers, nb_vierges = nrow(d), nb_recycles = 0L))
  }
  vierge <- !d$id_profil %in% registre_profil$id_profil
  dv <- d[vierge, , drop = FALSE]; du <- d[!vierge, , drop = FALSE]
  k_eff <- min(k, nrow(d))
  ch <- choisir_lignes_dp(dv, min(k_eff, nrow(dv)), col_poids, col_unite)
  l <- ch$lignes
  if(nrow(l) > 0){ l$origine_profil <- "vierge"; l$variante_debut <- 1L; l$hash_exclus <- "" }
  reste <- k_eff - nrow(l)
  if(reste > 0 && nrow(du) > 0){
    idx <- if(nrow(du) == 1) 1L else sample.int(nrow(du), min(reste, nrow(du)), prob = du[[col_poids]])
    r <- du[idx, , drop = FALSE]
    m <- match(r$id_profil, registre_profil$id_profil)
    r$origine_profil <- "recycle"; r$variante_debut <- as.integer(registre_profil$variante_max[m]) + 1L
    r$hash_exclus <- vapply(registre_profil$hash_das[m], function(h) paste(unique(h), collapse = " "), character(1))
    l <- dplyr::bind_rows(l, r)
  }
  list(lignes = l, planchers = ch$planchers, nb_vierges = nrow(dv), nb_recycles = sum(l$origine_profil == "recycle"))
}

# Sélection d'UNE population sur UNE lettre, sous registre et plafonds de classe (remplace
# selection_quota_dp_fixe_lettre en mode campagne). quotas_classe : tibble(diag2, DPEC, quota) issu
# d'allocation_classe_plafonnee (toutes lettres) ; NULL = pas de classe plafonnée.
selection_campagne_lettre <- function(df, X, k, quotas_classe = NULL, registre_profil = NULL, col_dp = "diag2", col_dpec = "DPEC", col_poids = "poids", col_unite = "type_unite"){
  if(nrow(df) == 0) return(list(selection = df[0, ], stats = NULL))
  classes <- if(is.null(quotas_classe)) character(0) else unique(quotas_classe$DPEC)
  df$.grp <- ifelse(df[[col_dpec]] %in% classes, df[[col_dpec]], ".reste")
  cles <- unique(df[, c(col_dp, ".grp"), drop = FALSE])
  out <- vector("list", nrow(cles)); st <- vector("list", nrow(cles))
  for(i in seq_len(nrow(cles))){
    d <- df[df[[col_dp]] == cles[[col_dp]][i] & df$.grp == cles$.grp[i], , drop = FALSE]
    if(cles$.grp[i] == ".reste"){ X_dp <- X; k_grp <- k } else {
      q <- quotas_classe$quota[quotas_classe$diag2 == cles[[col_dp]][i] & quotas_classe$DPEC == cles$.grp[i]]
      X_dp <- if(length(q)) as.integer(q[1]) else 1L; k_grp <- 1L   # classe plafonnée : 1 ligne par DP, variantes = quota
    }
    ch <- choisir_lignes_dp_registre(d, k_grp, registre_profil, col_poids, col_unite)
    nv <- variantes_par_ligne(nrow(ch$lignes), X_dp)
    l <- ch$lignes; l$n_var <- nv; l <- l[l$n_var > 0, , drop = FALSE]
    out[[i]] <- l
    st[[i]] <- data.frame(dp = cles[[col_dp]][i], groupe = cles$.grp[i], lignes_disponibles = nrow(d), X_dp = X_dp, k_eff = nrow(ch$lignes),
                          variantes = sum(nv), planchers_actifs = ch$planchers, plafonne = cles$.grp[i] != ".reste",
                          manque_a_gagner = max(0L, X_dp - nrow(d)), nb_vierges = ch$nb_vierges, nb_recycles = ch$nb_recycles,
                          vierges_restantes = max(0L, ch$nb_vierges - sum(ch$lignes$origine_profil == "vierge")), stringsAsFactors = FALSE)
  }
  sel <- dplyr::bind_rows(out); sel$.grp <- NULL
  list(selection = sel, stats = dplyr::bind_rows(st))
}

# --- G5. Rétro-inscription : registre d'une campagne tirée avant ce chantier -----------------
# Reconstruit les lignes de registre depuis les chunks tirés (pivots + graine -> id_profil ; DAS
# réellement tirés -> hash_das ; variante telle que tirée) et la sélection (population, DPEC).
registre_depuis_chunks <- function(df_chunks, campagne, population = NA_character_, dpec_par_profil = NULL, typo = NULL){
  if(nrow(df_chunks) == 0) return(tibble::as_tibble(stats::setNames(replicate(length(COLONNES_REGISTRE), character(0), simplify = FALSE), COLONNES_REGISTRE)))
  d <- df_chunks
  d$id_profil <- id_profil_de(dplyr::mutate(d, diagnostic_associes = graine))
  d$hash_das <- hash_das_de(d$diagnostic_associes)
  d$id_scenario <- id_scenario_de(d$id_profil, d$variante)
  d$campagne <- as.character(campagne); d$population <- if("population" %in% names(d)) as.character(d$population) else as.character(population)
  d$DPEC <- if("DPEC" %in% names(d)) as.character(d$DPEC) else if(!is.null(dpec_par_profil)) unname(dpec_par_profil[d$id_profil]) else if(!is.null(typo)) typologie_sejour(d, typo, col_age = "age", duree_defaut = 3)$DPEC else NA_character_
  d$date <- as.character(Sys.Date()); d$branche <- "long"
  tibble::as_tibble(d[, COLONNES_REGISTRE])
}
# Lignes de registre des SÉJOURS COURTS depuis un corpus ou des chunks courts : id_profil recalculé (recette id_courts_v1 sur
# les pivots), hash_das sur les DAS tirés, variante telle que tirée, UN scénario par id_scenario (les variantes d'habillage
# admin d'un même scénario sont repliées), population par cage, DPEC par typologie (vraie durée, âge tiré) si absent.
registre_depuis_courts <- function(df, campagne, typo = NULL, populations = POPULATIONS){
  if(nrow(df) == 0) return(registre_vide())
  d <- tibble::as_tibble(df)
  d$id_profil <- id_profil_courts_de(d); d$hash_das <- hash_das_de(d$diagnostic_associes)
  d$variante <- as.integer(d$variante); d$id_scenario <- id_scenario_de(d$id_profil, d$variante)
  d <- d[!duplicated(d$id_scenario), , drop = FALSE]
  d$campagne <- as.character(campagne); d$population <- population_de(as.character(d$cage), populations)
  if(!"DPEC" %in% names(d)) d$DPEC <- if(!is.null(typo)) typologie_sejour(d, typo, col_age = "age", col_duree = "duree")$DPEC else NA_character_
  d$DPEC <- as.character(d$DPEC); d$date <- as.character(Sys.Date()); d$branche <- "court"
  tibble::as_tibble(d[, COLONNES_REGISTRE])
}

## ---- H. Lot « notebook campagnes + config locale » : nommage par campagne, fichiers datés, garde-fous ----
if(!exists("%||%")) `%||%` <- function(a, b) if(is.null(a)) b else a

# --- H1. (résolution des fichiers datés RETIRÉE : règle « nom stable, date dans le méta », section 22) ----
# Message quand les scénarios courts DE LA CAMPAGNE sont introuvables (chantier « courts en campagnes » : étape de campagne).
message_courts_absent <- function(dir_courts_campagne){
  paste0("etape_finalisation : scénarios courts de la campagne absents (", sub("/$", "", dir_courts_campagne), "). ",
         "Lancez etape_tirage_courts() — étape DE CAMPAGNE, sans base, après etape_selection_longs() (budget = NB_CRH_CIBLE_COURTS, ou RATIO_COURTS × volume longs attendu) ; ",
         "le tirable (30_courts/ref_pivots_courts.parquet, magasin partagé) et les refs viennent d'etape_refs() (extraction, 01_preparation_donnees.Rmd).")
}

# --- H2. Corpus final nommé par CAMPAGNE : garde-fou sur le dossier existant --------------------
# `meta_existant` : NULL (dossier absent) ; list() (dossier présent sans _meta.yaml, antérieur au lot) ;
# sinon le contenu de _meta.yaml. Retourne list(action = "creer" | "reprise" | "stop", message).
verifier_dossier_final <- function(meta_existant, campagne, dossier = ""){
  if(is.null(meta_existant)) return(list(action = "creer", message = "corpus " %+% campagne %+% " : dossier créé (" %+% dossier %+% ")"))
  autre <- meta_existant$campagne
  if(is.null(autre)) return(list(action = "reprise", message = "corpus " %+% campagne %+% " : dossier présent SANS _meta.yaml (antérieur au nommage par campagne) : repris, parts réécrites"))
  if(!identical(as.character(autre), as.character(campagne)))
    return(list(action = "stop", message = "etape_finalisation : le dossier " %+% dossier %+% " porte un _meta.yaml d'une AUTRE campagne (" %+% autre %+% ", du " %+% (meta_existant$date %||% "?") %+%
                  "). Rien n'est écrasé. Choisissez un autre identifiant (CAMPAGNE <- \"Cn\", Restart R) ou déplacez ce corpus."))
  list(action = "reprise", message = "corpus " %+% campagne %+% " : dossier présent (même campagne, du " %+% (meta_existant$date %||% "?") %+% ") : repris, parts réécrites")
}

# --- H3. Statut d'une campagne au registre (affichage session, stop précoce de la sélection) ------
# branche = NULL : les deux branches ; "long" / "court" : statut de cette seule branche (les courts s'inscrivent aussi).
statut_campagne_registre <- function(campagne, registre, branche = NULL){
  l <- if(is.null(registre) || is.null(registre$lignes)) NULL else registre$lignes[registre$lignes$campagne == campagne, , drop = FALSE]
  if(!is.null(l) && !is.null(branche)) l <- l[(if("branche" %in% names(l)) l$branche else rep("long", nrow(l))) %in% branche, , drop = FALSE]
  if(is.null(l) || nrow(l) == 0) return(list(inscrite = FALSE, nb = 0L, nb_longs = 0L, nb_courts = 0L, texte = "jamais inscrite au registre" %+% (if(is.null(branche)) "" else " (branche " %+% branche %+% ")")))
  br <- if("branche" %in% names(l)) l$branche else rep("long", nrow(l)); nl <- sum(br == "long"); nc <- sum(br == "court")
  list(inscrite = TRUE, nb = nrow(l), nb_longs = nl, nb_courts = nc, date = max(as.character(l$date)),
       texte = sprintf("déjà inscrite au registre : %d scénarios (%d longs, %d courts) le %s — changez d'identifiant", nrow(l), nl, nc, max(as.character(l$date))))
}

# --- H4. Lecteurs à repli ------------------------------------------------------------------
# Lecteur UNIQUE du livrable d'une campagne (<profil>/60_export_final/scenarios_<campagne>.parquet, ou le repli en parts
# scenarios_<campagne>/part_*.parquet au-delà de SEUIL_MONOFICHIER) : dataset arrow si arrow réel, sinon rbind des parts.
# Filtres optionnels : populations (colonne population des longs), branche ("long" / "court"), colonnes. NULL si absent.
lire_corpus_final <- function(campagne = CAMPAGNE, populations = NULL, colonnes = NULL, branche = NULL, dir_export = DIR_EXPORT_FINAL){
  dir_export <- sub("/+$", "", dir_export)
  f_mono <- file.path(dir_export, "scenarios_" %+% campagne %+% ".parquet"); d_parts <- file.path(dir_export, "scenarios_" %+% campagne)
  cols <- if(is.null(colonnes)) NULL else unique(c(colonnes, if(!is.null(populations)) "population", if(!is.null(branche)) "branche"))
  if(file.exists(f_mono)){
    d <- tibble::as_tibble(arrow::read_parquet(f_mono)); if(!is.null(cols)) d <- d[, intersect(cols, names(d)), drop = FALSE]
  } else {
    parts <- if(dir.exists(d_parts)) list.files(d_parts, pattern = "^part_[0-9]{4}\\.parquet$", full.names = TRUE) else character(0)
    if(length(parts) == 0) return(NULL)
    d <- if(arrow_dataset_disponible()){
      ds <- arrow::open_dataset(parts); if(!is.null(cols)) ds <- dplyr::select(ds, dplyr::all_of(cols)); tibble::as_tibble(dplyr::collect(ds))
    } else purrr::map(parts, function(p){ x <- tibble::as_tibble(arrow::read_parquet(p)); if(!is.null(cols)) x[, intersect(cols, names(x)), drop = FALSE] else x }) |> purrr::list_rbind()
  }
  if(!is.null(branche) && "branche" %in% names(d)) d <- d[d$branche %in% branche, , drop = FALSE]
  if(!is.null(populations) && "population" %in% names(d)) d <- d[is.na(d$population) & is.null(branche) | d$population %in% populations, , drop = FALSE]
  if(!is.null(colonnes)) d <- d[, intersect(unique(colonnes), names(d)), drop = FALSE]
  d
}
# Dernier fichier (tri lexical = chronologique sur AAAAMMJJ) d'un dossier au motif ; NA si aucun.
dernier_fichier <- function(dossier, motif){
  f <- if(!is.na(dossier) && dir.exists(dossier)) sort(list.files(dossier, pattern = motif)) else character(0)
  if(length(f)) file.path(dossier, f[length(f)]) else NA_character_
}
# Lecture robuste d'un produit d'étape : fichier absent -> message actionnable (pas d'erreur R brute), NULL invisible ;
# présent -> lecture selon l'extension (txt : lignes ; csv : read.csv, ou read.csv2 si ';' en tête ; parquet ; yaml),
# `mode` pour forcer ("lignes", "csv", "csv2", "parquet", "yaml") ou "chemin" pour rendre le chemin résolu.
lire_si_present <- function(chemin, produit_par = "l'étape amont", mode = "auto", nom = NULL){
  if(length(chemin) != 1 || is.na(chemin) || !file.exists(chemin)){
    cat(if(!is.null(nom)) nom else if(length(chemin) == 1 && !is.na(chemin)) basename(chemin) else "<fichier>",
        " absent — produit par ", produit_par, ", pas encore exécutée dans ce profil (voir etat_pipeline()).\n", sep = "")
    return(invisible(NULL))
  }
  if(mode == "chemin") return(chemin)
  if(mode == "auto"){
    ext <- tolower(tools::file_ext(chemin))
    mode <- if(ext == "csv"){ if(grepl(";", readLines(chemin, n = 1, warn = FALSE)[1], fixed = TRUE)) "csv2" else "csv" }
            else switch(ext, parquet = "parquet", yaml = "yaml", yml = "yaml", "lignes")
  }
  switch(mode, lignes = readLines(chemin, warn = FALSE), csv = utils::read.csv(chemin), csv2 = utils::read.csv2(chemin),
         parquet = tibble::as_tibble(arrow::read_parquet(chemin)), yaml = yaml::read_yaml(chemin),
         stop("lire_si_present : mode inconnu : " %+% mode, call. = FALSE))
}

## ---- I. Chantier « livrable unique + nommage + arborescence » : magasins partagés, livrable, réorganisation ----

# --- I1. Gardes des magasins partagés (généralisation de l'ancienne condition Q13) ------------
# Chaque magasin partagé porte un _meta.yaml avec LES PARAMÈTRES QUI LE DÉFINISSENT ; à chaque chargement la config
# courante lui est comparée : divergence -> stop nommant les clés en écart et les issues. Les partiels gardent leurs
# clés bloquantes historiques ; les références les 6 clés de l'ancienne condition Q13.
CLES_MAGASINS <- list(
  partiels   = c("K_GRAINE_LONGS", "NBDA_MAX", "DUREE_LONGS", "PIVOTS_LONGS"),
  references = c("AN_REF", "ANS_COURTS", "SEUIL_REF_DAS", "SEUIL_REF_IMPRECIS", "SEUIL_REF_PAIRES", "CONVERSION_E669", "BARE_E669_DEFAUT", "CLES_ADMIN_LONGS", "DUREE_LONGS", "DUREE_COURTS"),
  catalogue  = c("ANS_HISTORIQUE", "TYPES_ETBS_LONGS", "SEUIL_PIVOT", "CONVERSION_E669", "BARE_E669_DEFAUT", "K_GRAINE_LONGS", "NBDA_MAX", "DUREE_LONGS", "PIVOTS_LONGS"),
  courts     = c("ANS_COURTS", "SEUIL_PIVOT", "DUREE_COURTS", "PIVOTS_COURTS", "CONVERSION_E669", "BARE_E669_DEFAUT"))   # 30_courts = le TIRABLE (pivots), plus le tirage
DRAPEAUX_MAGASINS <- c(partiels = "FORCER_PARTIELS", references = "FORCER_REFS", catalogue = "FORCER_CATALOGUE", courts = "FORCER_COURTS")
DOSSIERS_MAGASINS <- c(partiels = "00_partiels", references = "10_references", catalogue = "20_catalogue", courts = "30_courts")
# Méta d'un magasin : les clés qui le définissent, prises dans `config` (valeurs effectives), + date (+ champs libres).
meta_magasin <- function(magasin, config, ...){
  cles <- CLES_MAGASINS[[magasin]]
  m <- lapply(cles, function(k){ v <- config[[k]]; if(is.numeric(v) && length(v) > 1) as.integer(v) else v }); names(m) <- cles
  c(list(magasin = magasin), m, list(date = as.character(Sys.Date())), list(...))
}
# verifier_magasin(magasin, meta_existant, config_courante) -> list(ok, differences, message). NULL existant -> ok.
verifier_magasin <- function(magasin, meta_existant, config_courante, cles = CLES_MAGASINS[[magasin]], dossier = DOSSIERS_MAGASINS[[magasin]]){
  if(is.null(meta_existant)) return(list(ok = TRUE, differences = character(0), message = NULL))
  norm <- function(v){ v <- as.character(unlist(v)); if(length(v) > 1 && !is.null(v)) v else v }
  diff <- cles[vapply(cles, function(k) !identical(norm(meta_existant[[k]]), norm(config_courante[[k]])), logical(1))]
  if(length(diff) == 0) return(list(ok = TRUE, differences = character(0), message = NULL))
  det <- vapply(diff, function(k) sprintf("%s : magasin = %s ; courant = %s", k, paste(unlist(meta_existant[[k]]), collapse = ","), paste(unlist(config_courante[[k]]), collapse = ",")), character(1))
  list(ok = FALSE, differences = diff,
       message = sprintf("magasin partagé %s (%s/_meta.yaml) : paramètres en écart avec la config courante — %s. Issues : (1) régénérer le magasin avec %s <- TRUE — ATTENTION, il sert TOUS les profils ; (2) surcharger son chemin pour ce seul profil (CHEMINS_SURCHARGES$%s en config)%s.",
                         magasin, dossier, paste(det, collapse = " ; "), DRAPEAUX_MAGASINS[[magasin]], magasin,
                         if(magasin == "catalogue" && any(diff %in% c("ANS_HISTORIQUE", "TYPES_ETBS_LONGS"))) " ; (3) aligner ANS_HISTORIQUE / TYPES_ETBS_LONGS de la config sur la décision de périmètre du catalogue" else ""))
}

# --- I2. Livrable unique par campagne : union de schémas, familles de colonnes, revue ----------
# Modèle de schéma = union (bind_rows) des tibbles vides ; harmoniser() aligne un tibble sur le modèle (colonnes
# manquantes = NA typés, ordre du modèle, colonne branche en tête).
# Type commun d'une colonne présente dans plusieurs branches : identique -> conservé ; integer/numeric -> numeric ;
# sinon character (ex. `age` : entier chez les courts, classe lt_18/ge_18 chez les longs -> texte, documenté au méta).
type_commun <- function(classes){
  cl <- unique(classes)
  if(length(cl) == 1) return(cl)
  if(all(cl %in% c("integer", "numeric"))) return("numeric")
  "character"
}
coercer <- function(x, type) switch(type, character = as.character(x), numeric = as.numeric(x), integer = as.integer(x), logical = as.logical(x), x)
modele_schema <- function(...){
  frames <- list(...); noms <- unique(unlist(lapply(frames, names)))
  types <- vapply(noms, function(n) type_commun(unlist(lapply(frames, function(d) if(n %in% names(d)) class(d[[n]])[1]))), character(1))
  m <- tibble::as_tibble(stats::setNames(lapply(noms, function(n){ d <- frames[[which(vapply(frames, function(x) n %in% names(x), logical(1)))[1]]]; coercer(d[[n]][0], types[[n]]) }), noms))
  m[, c("branche", setdiff(names(m), "branche")), drop = FALSE]
}
harmoniser <- function(d, modele){
  for(n in intersect(names(d), names(modele))){ t_m <- class(modele[[n]])[1]; if(class(d[[n]])[1] != t_m) d[[n]] <- coercer(d[[n]], t_m) }
  h <- dplyr::bind_rows(modele, d); h[, names(modele), drop = FALSE]
}
# poids : la colonne de RÉ-ÉCHANTILLONNAGE du livrable (micro-lot « Q74 + poids documenté », journal §24.12) — même note au méta,
# dans VISITE_GUIDEE.md et au README ; le curseur alpha est une décision de l'équipe apprentissage (question ouverte du consortium).
NOTE_POIDS <- paste0("poids = effectif réel du profil dans la base sur le périmètre du catalogue (longs : profil du catalogue ; courts : n du pivot) — ",
                     "la colonne de ré-échantillonnage : le corpus est construit à couverture équitable (quota par DP), l'entraînement peut restituer la ",
                     "distribution réelle en échantillonnant proportionnellement à poids (ou poids^alpha, curseur réalisme / couverture — décision équipe apprentissage) ; ",
                     "numérique, jamais NA sur les deux branches")
FAMILLES_COLONNES <- list(
  identite_livrable = c("branche", "population", "campagne"),
  profil_clinique   = c("sexe", "age", "cage", "cage2"),
  contexte_sejour   = c("mode_hospit", "ghm2", "racine", "duree", "nbda", "type_unite", "prep_sc"),
  diagnostics       = c("diag2", "graine", "diagnostic_associes", "nb_das", "diabete", "diabete_scenario", "hta", "hta_scenario"),
  habillage_admin   = c("mode_entree", "mode_sortie", "mdp", "repli_admin"),
  typologie         = c("lettre", "DPEC", "TPEC"),
  tracabilite       = c("id_profil", "id_scenario", "hash_das", "variante", "origine_profil", "id_selection", "variante_debut"),
  audit             = c("poids", "source_ref", "nb_cible", "nb_variantes_demandees", "hash_exclus"))
familles_colonnes <- function(noms){
  f <- lapply(FAMILLES_COLONNES, function(cols) cols[cols %in% noms]); f <- f[lengths(f) > 0]
  reste <- setdiff(noms, unlist(FAMILLES_COLONNES)); if(length(reste)) f$autres <- reste
  f
}
# Échantillon de revue tiré du livrable unifié : n_courts = round(n × part_courts) lignes de branche "court",
# le reste de branche "long" ; round-robin par CMD dans chaque branche (echantillonner_revue).
echantillonner_livrable <- function(df, n, part_courts, seed){
  n_c <- as.integer(round(n * part_courts)); n_l <- as.integer(n) - n_c
  set.seed(seed);     ec <- echantillonner_revue(df[df$branche == "court", , drop = FALSE] |> dplyr::mutate(cmd = substr(ghm2, 1, 2)), n_c)
  set.seed(seed + 1); el <- echantillonner_revue(df[df$branche == "long",  , drop = FALSE] |> dplyr::mutate(cmd = substr(ghm2, 1, 2)), n_l)
  dplyr::bind_rows(ec, el)
}

# --- I3. Réorganisation SUR PLACE de l'ancien results/ (plan pur) ----------------------------------
# inventaire : data.frame(chemin = chemin relatif à _a_reorganiser/, mtime, taille). Retourne un data.frame
# (source, categorie ∈ reconnu / ignore / inconnu, destination (relative à PATH_RESULTS), action, motif) ; RIEN n'est
# déplacé ici. Anciens noms de refs -> ref_* ; doublons (datés, ou inter-profils exports/exports_diagnostic) : le plus
# récent retenu, les autres « ignoré (doublon) » ; registre -> production/50_registre/ seulement si migrer_registre.
ANCIENS_NOMS_REFS <- c(ref_das_chronique = "ref_das_chronique", distribution_e660 = "ref_distribution_e660", ref_das_aigu = "ref_das_aigu",
                       ref_nb_chroniques = "ref_nb_chroniques", ref_comp_diabete = "ref_comp_diabete", pivots_courts = "ref_pivots_courts",
                       v_admin_courts = "ref_v_admin_courts", v_admin_longs = "ref_v_admin_longs",
                       referentiel_substitution_imprecis = "ref_substitution_imprecis", referentiel_paires_chroniques = "ref_paires_chroniques")
planifier_reorganisation <- function(inventaire, migrer_registre = FALSE){
  stopifnot(all(c("chemin", "mtime") %in% names(inventaire)))
  inv <- inventaire[order(inventaire$chemin, method = "radix"), , drop = FALSE]   # ordre C (déterministe, indépendant de la locale)
  n <- nrow(inv); cat_ <- rep("inconnu", n); dest <- rep(NA_character_, n); action <- rep("arbitrage humain", n); motif <- rep("non reconnu : à arbitrer, jamais déplacé", n)
  ch <- inv$chemin; base <- basename(ch); dir1 <- sub("/.*$", "", ch)
  profil_de <- function(d) ifelse(d == "exports", "production", ifelse(d == "exports_diagnostic", "diagnostic", NA_character_))
  est_export <- grepl("^exports(_diagnostic)?/", ch)
  reste <- ifelse(est_export, sub("^exports(_diagnostic)?/", "", ch), NA_character_)
  poser <- function(i, categorie, destination, act, mot){ cat_[i] <<- categorie; dest[i] <<- destination; action[i] <<- act; motif[i] <<- mot }
  for(i in seq_len(n)){
    if(grepl("^partiels/catalogue_partiel_.*\\.parquet$", ch[i])){ poser(i, "reconnu", "00_partiels/" %+% base[i], "copier", "partiel"); next }
    if(ch[i] == "partiels/partiels_meta.yaml"){ poser(i, "reconnu", "00_partiels/_meta.yaml", "convertir méta", "méta des partiels (clés bloquantes reprises)"); next }
    if(grepl("\\.tmp$", ch[i])){ poser(i, "ignore", NA, "ignorer", "fichier temporaire"); next }
    if(!est_export[i]) next
    r <- reste[i]; b <- base[i]; nom_sans_ext <- sub("\\.parquet$", "", b)
    if(!grepl("/", r) && grepl("\\.parquet$", b) && nom_sans_ext %in% names(ANCIENS_NOMS_REFS)){
      nv <- ANCIENS_NOMS_REFS[[nom_sans_ext]]
      if(nv == "ref_pivots_courts") poser(i, "reconnu", "30_courts/ref_pivots_courts.parquet", "copier + renommer", "le TIRABLE courts (pivots = catalogue des courts, magasin 30_courts)")
      else poser(i, "reconnu", "10_references/" %+% nv %+% ".parquet", "copier + renommer", "référence (ancien nom -> ref_*)")
      next }
    if(grepl("^catalogue_longs_seuil/part_[A-Z]\\.parquet$", r)){ poser(i, "reconnu", "20_catalogue/catalogue_longs_seuil/" %+% b, "copier", "part du catalogue"); next }
    if(r == "catalogue_longs_seuil/_sidecar.yaml"){ poser(i, "reconnu", "20_catalogue/catalogue_longs_seuil/_meta.yaml", "convertir méta", "sidecar du catalogue -> _meta.yaml (+ clés du magasin depuis catalogue_longs_seuil_meta.yaml)"); next }
    if(r == "catalogue_longs_seuil_meta.yaml"){ poser(i, "reconnu", "20_catalogue/catalogue_longs_seuil_meta.yaml", "copier", "méta du catalogue"); next }
    if(r == "catalogue_longs_seuil.parquet"){ poser(i, "reconnu", "20_catalogue/catalogue_longs_seuil.parquet", "copier", "catalogue monofichier (non partitionné)"); next }
    if(r == "catalogue_longs_seuil.parquet.ancien"){ poser(i, "ignore", NA, "ignorer", "monofichier .ancien (les parts font foi)"); next }
    if(grepl("^chunks/courts_chunk_[0-9]{4}\\.parquet$", r) || r == "chunks/courts_chunks_meta.yaml"){ poser(i, "ignore", NA, "ignorer", "chunks courts de l'ancienne génération (corpus courts FIXE : adopté depuis scenarios_courts_v8_<date>, jamais re-tiré)"); next }
    if(grepl("^scenarios_courts_v8_[0-9]{8}\\.parquet$", r)){ poser(i, "reconnu", "30_courts/scenarios_courts.parquet", "copier + dé-dater", "corpus courts HISTORIQUE (méta scenarios_courts_meta.yaml avec la date d'origine ; adopté en C1 par etape_adopter_campagne)"); next }
    if(r %in% c("diagnostic_apports.csv", "recouvrement.csv")){ poser(i, "reconnu", "90_diagnostics/" %+% b, "copier", "diagnostic partagé"); next }
    if(r == "diagnostic_memoire.csv"){ poser(i, "reconnu", "90_diagnostics/diagnostic_memoire_" %+% profil_de(dir1[i]) %+% ".csv", "copier + renommer", "diagnostic mémoire par profil"); next }
    if(grepl("^registre_tirages/registre_.*\\.parquet$", r)){
      if(dir1[i] != "exports") poser(i, "ignore", NA, "ignorer", "registre hors production (exports_diagnostic)")
      else if(migrer_registre) poser(i, "reconnu", "production/50_registre/registre_tirages/" %+% b, "copier", "registre (migrer_registre = TRUE : les campagnes passées comptent)")
      else poser(i, "ignore", NA, "ignorer", "registre non migré (migrer_registre = FALSE : registre vierge)")
      next
    }
    if(grepl("^(selection_longs|chunks/longs_chunk_|chunks/[^/]+/|habille/|scenarios_longs_tirage_v8_|rapport_v8_|rapport_extraction_v8_|echantillon_revue|top30_das_par_cmd|meta_tirage\\.yaml|selection_longs_effectifs|chunks/longs_chunks_meta)", r)){
      poser(i, "ignore", NA, "ignorer", "transitoire ou ancienne génération (sélection, chunks longs, habillé, corpus, rapports, annexes)"); next
    }
  }
  plan <- data.frame(source = ch, categorie = cat_, destination = dest, action = action, motif = motif, mtime = inv$mtime, stringsAsFactors = FALSE)
  # doublons de destination (datés ou inter-profils) : le plus récent retenu, les autres ignorés
  rec <- which(plan$categorie == "reconnu")
  for(d in unique(plan$destination[rec])){
    idx <- rec[plan$destination[rec] == d]
    if(length(idx) > 1){
      # le plus récent retenu ; à égalité de date, exports/ (production) prime sur exports_diagnostic/, puis l'ordre des noms
      prio <- order(-as.numeric(plan$mtime[idx]), !grepl("^exports/", plan$source[idx]), plan$source[idx], method = "radix")
      garde <- idx[prio[1]]
      for(k in setdiff(idx, garde)){ plan$categorie[k] <- "ignore"; plan$action[k] <- "ignorer"; plan$motif[k] <- "doublon : plus ancien que " %+% plan$source[garde] %+% " (le plus récent est retenu)"; plan$destination[k] <- NA }
    }
  }
  plan
}
imprimer_plan_reorganisation <- function(plan){
  for(cat_ in c("reconnu", "ignore", "inconnu")){
    p <- plan[plan$categorie == cat_, , drop = FALSE]
    cat(sprintf("\n== %s (%d) ==\n", c(reconnu = "1. RECONNUS -> destination", ignore = "2. IGNORÉS volontairement", inconnu = "3. NON RECONNUS (arbitrage humain)")[[cat_]], nrow(p)))
    if(nrow(p) == 0){ cat("   (aucun)\n"); next }
    for(i in seq_len(nrow(p))) cat(sprintf("   %-60s %s%s\n", p$source[i], if(cat_ == "reconnu") "-> " %+% p$destination[i] %+% "  [" %+% p$action[i] %+% "]" else p$motif[i], if(cat_ == "reconnu") "" else ""))
  }
  invisible(plan)
}

## ---- J. Trois niveaux de paramètres : doctrine (config.R) / poste (config_locale.R) / campagne (campagne.R) ----
# La décision d'exploitation d'une campagne ne s'édite plus dans config.R : elle s'écrit dans campagne.R (gitignoré),
# depuis le notebook 02_campagne.Rmd (chunk ouvrir_campagne), et s'active par SCENARIOS_PMSI_SURCHARGE — même mécanique que palier.R.
# Les deux surcharges sont EXCLUSIVES ; la surcharge démo (troisième cas légitime) est hors de cette exclusivité.
PARAMETRES_CAMPAGNE <- c("CAMPAGNE", "NB_CRH_CIBLE", "NB_LIGNES_PAR_DP", "REGISTRE_ACTIF", "PLAFONDS_DPEC", "NB_CRH_CIBLE_COURTS", "RATIO_COURTS", "NB_VARIANTES_ADMIN_LONGS", "NB_VARIANTES_ADMIN_COURTS")
MARQUEUR_CAMPAGNE <- "SURCHARGE_CAMPAGNE_ACTIVE <- TRUE"
MARQUEUR_PALIER   <- "PALIER_ACTIF <- TRUE"
entier_R <- function(x, nom){ if(length(x) != 1 || is.na(x) || x != round(x) || x < 1) stop(nom %+% " : entier >= 1 attendu", call. = FALSE); sprintf("%dL", as.integer(x)) }
contenu_surcharge_campagne <- function(campagne, nb_crh_cible, nb_lignes_par_dp = 1L, registre_actif = TRUE, plafonds_dpec = NULL, nb_crh_cible_courts = NULL, ratio_courts = NULL, nb_variantes_admin = NULL, nb_variantes_admin_courts = NULL){
  if(!is.null(nb_variantes_admin) && length(nb_variantes_admin) != 1) stop("NB_VARIANTES_ADMIN_LONGS : entier >= 1 ou NA attendu", call. = FALSE)
  if(!is.character(campagne) || length(campagne) != 1 || !nzchar(campagne) || grepl("[^A-Za-z0-9_-]", campagne)) stop("CAMPAGNE : identifiant court obligatoire ([A-Za-z0-9_-])", call. = FALSE)
  if(!is.null(ratio_courts) && (!is.numeric(ratio_courts) || length(ratio_courts) != 1 || is.na(ratio_courts) || ratio_courts <= 0)) stop("RATIO_COURTS : nombre > 0 attendu", call. = FALSE)
  c("# campagne.R — DÉCISION D'EXPLOITATION de la campagne (niveau campagne ; gitignoré ; écrit par le chunk ouvrir_campagne de 02_campagne.Rmd).",
    "# Activé par SCENARIOS_PMSI_SURCHARGE ; exclusif du palier (palier.R). Les défauts vivent dans config.R, le poste dans config_locale.R.",
    MARQUEUR_CAMPAGNE,
    "CAMPAGNE <- \"" %+% campagne %+% "\"",
    "NB_CRH_CIBLE <- " %+% entier_R(nb_crh_cible, "NB_CRH_CIBLE"),
    "NB_LIGNES_PAR_DP <- " %+% entier_R(nb_lignes_par_dp, "NB_LIGNES_PAR_DP"),
    "REGISTRE_ACTIF <- " %+% (if(isTRUE(registre_actif)) "TRUE" else "FALSE"),
    if(!is.null(plafonds_dpec)) "PLAFONDS_DPEC <- " %+% paste(deparse(plafonds_dpec), collapse = ""),
    if(!is.null(nb_crh_cible_courts)) "NB_CRH_CIBLE_COURTS <- " %+% entier_R(nb_crh_cible_courts, "NB_CRH_CIBLE_COURTS") %+% "   # budget courts ABSOLU (sinon RATIO_COURTS × volume longs)",
    if(!is.null(ratio_courts)) "RATIO_COURTS <- " %+% format(as.numeric(ratio_courts)) %+% "   # provisoire — à calibrer avec l'équipe apprentissage",
    if(!is.null(nb_variantes_admin)) "NB_VARIANTES_ADMIN_LONGS <- " %+% (if(is.na(nb_variantes_admin)) "NA   # toutes les combinaisons (v7.2)" else entier_R(nb_variantes_admin, "NB_VARIANTES_ADMIN_LONGS") %+% "   # tenues admin par scénario long (N > 1 : id_scenario suffixé -aN)"),
    if(!is.null(nb_variantes_admin_courts)) "NB_VARIANTES_ADMIN_COURTS <- " %+% entier_R(nb_variantes_admin_courts, "NB_VARIANTES_ADMIN_COURTS") %+% "   # tenues admin par scénario court (N > 1 : id_scenario suffixé -aN ; 2 = v7.1.2)")
}
contenu_surcharge_palier <- function(nb_crh_cible = 100000L){
  c("# palier.R — PALIER DE MESURE (budget réduit, hors registre) ; gitignoré ; écrit par le chunk palier_surcharge de 03_outils_maintenance.Rmd.",
    "NB_CRH_CIBLE <- " %+% entier_R(nb_crh_cible, "NB_CRH_CIBLE"), MARQUEUR_PALIER, "REGISTRE_ACTIF <- FALSE   # imposé : un palier n'écrit jamais au registre")
}
# Type d'une surcharge d'après son contenu : "palier", "campagne", "autre" (ex. démo), "aucune" (vide / absente).
type_surcharge <- function(lignes){
  if(is.null(lignes) || length(lignes) == 0) return("aucune")
  l <- sub("#.*$", "", lignes)
  if(any(grepl("^\\s*PALIER_ACTIF\\s*<-\\s*TRUE", l))) return("palier")
  if(any(grepl("^\\s*SURCHARGE_CAMPAGNE_ACTIVE\\s*<-\\s*TRUE", l))) return("campagne")
  "autre"
}
# Exclusivité palier / campagne : écrire l'une alors que l'autre est active -> refus, message disant laquelle retirer et comment.
verifier_exclusivite_surcharges <- function(type_demande, chemin_actif, lignes_actives = NULL){
  type_actif <- type_surcharge(lignes_actives)
  if(type_actif %in% c("aucune", "autre") || type_actif == type_demande) return(list(ok = TRUE, message = NULL))
  autre <- if(type_actif == "palier") "le PALIER (" %+% chemin_actif %+% ")" else "la CAMPAGNE (" %+% chemin_actif %+% ")"
  list(ok = FALSE, message = "surcharge " %+% type_demande %+% " refusée : " %+% autre %+% " est actif. Retirez-le d'abord : " %+%
         (if(type_actif == "palier") "chunk vider_palier (JE_CONFIRME_VIDAGE_PALIER) de 03_outils_maintenance.Rmd, ou Sys.setenv(SCENARIOS_PMSI_SURCHARGE = \"\") puis Restart R"
          else "Sys.setenv(SCENARIOS_PMSI_SURCHARGE = \"\") puis Restart R (campagne.R peut rester : il n'est actif que par SCENARIOS_PMSI_SURCHARGE)") %+% ", puis relancez ce chunk.")
}
# Source de chaque paramètre de campagne : "défaut config" ou "surcharge <type> (<fichier>)" si la surcharge active le définit.
sources_parametres <- function(noms, lignes_surcharge = NULL, chemin = ""){
  type <- type_surcharge(lignes_surcharge); l <- if(is.null(lignes_surcharge)) character(0) else sub("#.*$", "", lignes_surcharge)
  vapply(noms, function(n) if(type != "aucune" && any(grepl("^\\s*" %+% n %+% "\\s*<-", l))) "surcharge " %+% type %+% " (" %+% basename(chemin) %+% ")" else "défaut config", character(1))
}

## ---- K. Chantier « courts en campagnes + habillage robuste » ----
# Décisions actées : les séjours courts ont le MÊME STATUT que les longs dans le corpus (leur traitement diffère — saturation
# des DAS sous-codés en routine — pas leur rôle) ; ils entrent dans l'économie des campagnes (tirage par campagne à
# variantes nouvelles, registre commun, composition pilotée par RATIO_COURTS provisoire) ; les pivots courts SONT le
# catalogue des courts (magasin 30_courts = le tirable) ; habillage admin des longs robuste (nbda retiré des clés, repli
# hiérarchique, jamais de NA silencieux) — défaut trouvé en REVUE CLINIQUE.

# --- K1. Budget courts d'une campagne et répartition sur les pivots -----------------------------
# budget_courts : NB_CRH_CIBLE_COURTS absolu si posé, sinon RATIO_COURTS × volume longs attendu (sélection de la campagne).
budget_courts <- function(nb_cible_courts = NULL, ratio = 1, volume_longs = NULL){
  if(!is.null(nb_cible_courts)) return(list(budget = as.integer(nb_cible_courts), source = "absolu", detail = "NB_CRH_CIBLE_COURTS = " %+% as.integer(nb_cible_courts)))
  if(is.null(volume_longs) || is.na(volume_longs)) stop("budget courts : NB_CRH_CIBLE_COURTS est NULL et le volume longs attendu de la campagne est inconnu (lancez etape_selection_longs() d'abord, ou posez NB_CRH_CIBLE_COURTS dans campagne.R).", call. = FALSE)
  b <- as.integer(round(as.numeric(ratio) * as.numeric(volume_longs)))
  list(budget = b, source = "ratio", detail = sprintf("RATIO_COURTS %s × volume longs attendu %d = %d", format(ratio), as.integer(volume_longs), b))
}
# Répartition du budget sur les pivots AU POIDS (n), plus forts restes ; remise au niveau pivot autorisée (un pivot reçoit
# nb_tirages variantes). Somme == budget ; les pivots à 0 ne sont pas tirés.
repartir_budget_pivots <- function(budget, poids){
  if(length(poids) == 0 || is.na(budget) || budget < 1) return(integer(length(poids)))
  as.integer(repartir_proportionnel(as.integer(budget), as.numeric(poids)))
}
# Colonnes de tirage d'un pivot sous registre : variante_debut (variante_max + 1) et hash_exclus du pivot (id k…), sinon vierge.
pivots_sous_registre <- function(pivots, registre_profil = NULL){
  p <- tibble::as_tibble(pivots); p$id_profil <- id_profil_courts_de(p)
  p$origine_profil <- "vierge"; p$variante_debut <- 1L; p$hash_exclus <- ""
  if(!is.null(registre_profil) && nrow(registre_profil) > 0){
    m <- match(p$id_profil, registre_profil$id_profil); vu <- !is.na(m)
    p$origine_profil[vu] <- "recycle"; p$variante_debut[vu] <- as.integer(registre_profil$variante_max[m[vu]]) + 1L
    p$hash_exclus[vu] <- vapply(registre_profil$hash_das[m[vu]], function(h) paste(unique(h), collapse = " "), character(1))
  }
  p
}

# --- K2. Habillage admin robuste (défaut trouvé en revue clinique : NA durée + modes sur les longs) --------------
# Cause : jointure naturelle sur 7 clés dont l'âge EXACT et nbda, v_admin photographié sur AN_REF seule -> profils du
# catalogue multi-années sans candidat -> NA sur les 4 colonnes apportées, ensemble. Décision : nbda SORT des clés
# (fabrique_v_admin_longs sans nbda) ; repli hiérarchique : (0) strate fine 6 clés -> (1) cage au lieu de l'âge exact ->
# (2) mode_hospit × cage × racine ; tirage au premier niveau non vide, PONDÉRÉ par les effectifs n de la photographie
# (Q70 actée : l'uniforme entre combinaisons distinctes sur-représente les issues rares ; aux replis, n sommés sur les
# strates fusionnées) ; colonne repli_admin (0/1/2) tracée jusqu'au corpus ; tous niveaux vides -> stop NOMINATIF, jamais
# de NA silencieux. La photographie est filtrée sur le périmètre de durée de sa branche (Q72 actée).
# CLES_ADMIN_LONGS (config, doctrine) = niveau 0 ; niveaux de repli dérivés.
NIVEAUX_REPLI_ADMIN <- list(c("mode_hospit", "sexe", "age", "cage", "ghm2", "diag2"), c("mode_hospit", "sexe", "cage", "ghm2", "diag2"), c("mode_hospit", "cage", "racine"))
# d : scénarios ; v_admin : photographie admin avec effectifs n (racine dérivée de ghm2 si absente) ; nb_variantes : N tenues
# admin par scénario tirées SANS remise au poids n parmi les combinaisons de la strate (toutes si moins ; NA = toutes, v7.2) ;
# nb_repli : idem aux niveaux de repli (NULL = aligné sur nb_variantes, 1 si NA ; n sommés sur les strates fusionnées) ;
# cols_apport : colonnes apportées. suffixer_id : pour N > 1, id_scenario suffixé -a2..-aN à partir de la 2e tenue (unicité ;
# aucun suffixe à N = 1 — le registre compte les jeux de DAS, les tenues admin sont un raffinement en dessous).
# Photographie sans colonne n -> stop (magasin à régénérer).
habiller_admin <- function(d, v_admin, niveaux = NIVEAUX_REPLI_ADMIN, cols_apport = c(COLS_ADMIN, "duree"), nb_variantes = NA, nb_repli = NULL, etiquette = "longs", suffixer_id = FALSE){
  d <- tibble::as_tibble(d); v <- tibble::as_tibble(v_admin)
  if(is.null(nb_repli)) nb_repli <- if(is.na(nb_variantes)) 1L else as.integer(nb_variantes)
  if(!"n" %in% names(v)) stop("habiller_admin (" %+% etiquette %+% ") : la photographie admin ne porte pas d'effectifs (colonne n) : magasin 10_references antérieur au micro-lot « v_admin : périmètre de durée + pondération » — régénérer avec FORCER_REFS <- TRUE.", call. = FALSE)
  if(!"racine" %in% names(v) && "ghm2" %in% names(v)) v$racine <- substr(as.character(v$ghm2), 1, 5)
  if(any(vapply(niveaux, function(k) "racine" %in% k, logical(1))) && !"racine" %in% names(d) && "ghm2" %in% names(d)) d$racine <- substr(as.character(d$ghm2), 1, 5)
  manq <- setdiff(cols_apport, names(v)); if(length(manq)) stop("habiller_admin : colonnes apportées absentes de v_admin : " %+% paste(manq, collapse = ", "), call. = FALSE)
  if(nrow(d) == 0){ d$repli_admin <- integer(0); for(cc in setdiff(cols_apport, names(d))) d[[cc]] <- v[[cc]][0]; return(d) }
  d$.rid <- seq_len(nrow(d)); restants <- d[, setdiff(names(d), cols_apport), drop = FALSE]; out <- list()
  for(j in seq_along(niveaux)){
    cles <- niveaux[[j]]
    manq <- setdiff(cles, c(names(v), names(restants))); if(length(manq)) stop("habiller_admin : clés absentes au niveau " %+% (j - 1) %+% " : " %+% paste(manq, collapse = ", "), call. = FALSE)
    cand <- v[, c(cles, cols_apport, "n"), drop = FALSE] |> dplyr::summarise(.n_admin = sum(n), .by = dplyr::all_of(c(cles, cols_apport)))   # n sommés sur les strates fusionnées
    h <- dplyr::inner_join(restants, cand, by = cles, relationship = "many-to-many")
    n_var <- if(j == 1) nb_variantes else nb_repli
    if(!is.na(n_var)) h <- h |> dplyr::group_by(.rid) |> dplyr::slice_sample(n = as.integer(n_var), weight_by = .n_admin) |> dplyr::ungroup()
    h$.n_admin <- NULL
    h$repli_admin <- as.integer(j - 1L); out[[j]] <- h
    restants <- restants[!restants$.rid %in% h$.rid, , drop = FALSE]
    if(nrow(restants) == 0) break
  }
  if(nrow(restants) > 0){
    cles <- unique(c(niveaux[[length(niveaux)]], "diag2")); ex <- utils::head(unique(restants[, intersect(cles, names(restants)), drop = FALSE]), 10)
    stop("habillage " %+% etiquette %+% " : aucun candidat admin à AUCUN niveau de repli pour " %+% nrow(restants) %+% " scénario(s) — profils (" %+% paste(names(ex), collapse = ", ") %+% ") : " %+%
           paste(apply(as.data.frame(ex), 1, paste, collapse = "/"), collapse = " ; ") %+% ". Jamais de NA silencieux : élargir la photographie v_admin (années) ou les niveaux de repli.", call. = FALSE)
  }
  res <- dplyr::bind_rows(out); res <- res[order(res$.rid), , drop = FALSE]
  if(isTRUE(suffixer_id) && !is.na(nb_variantes) && nb_variantes > 1 && "id_scenario" %in% names(res)){
    k <- stats::ave(seq_len(nrow(res)), res$.rid, FUN = seq_along)
    res$id_scenario <- ifelse(k > 1, paste0(res$id_scenario, "-a", k), res$id_scenario)
  }
  res$.rid <- NULL
  res
}
# id_scenario sans le suffixe de tenue admin (-aN) : le jeu de DAS, tel qu'inscrit au registre
id_scenario_base <- function(x) sub("-a[0-9]+$", "", as.character(x))
# Contrôle « zéro NA d'habillage » (contrôles §8.2) : nb de lignes avec au moins un NA sur les colonnes apportées, lignes dont la
# durée sort du périmètre de la branche (duree_perimetre, Q72 : plus aucune ligne longue à durée < 3), distribution du repli,
# et multiplication admin (N tenues par scénario : lignes attendues = scénarios × N ; lignes EN TROP = anomalie, strates à
# moins de N combinaisons = information ; NA = toutes les combinaisons, non contrôlé).
controle_habillage <- function(df, cols_apport = c(COLS_ADMIN, "duree"), duree_perimetre = NULL, N = NA){
  cols <- intersect(cols_apport, names(df))
  na_lignes <- if(length(cols) == 0 || nrow(df) == 0) 0L else sum(rowSums(is.na(df[, cols, drop = FALSE])) > 0)
  hors <- if(is.null(duree_perimetre) || !"duree" %in% names(df) || nrow(df) == 0) 0L else sum(!is.na(df$duree) & !(as.numeric(df$duree) %in% as.numeric(duree_perimetre)))
  repli <- if("repli_admin" %in% names(df) && nrow(df) > 0) as.data.frame(table(niveau = df$repli_admin), responseName = "n") else data.frame(niveau = character(0), n = integer(0))
  scen <- if("id_scenario" %in% names(df)) dplyr::n_distinct(id_scenario_base(df$id_scenario)) else NA_integer_
  att <- if(is.na(N) || is.na(scen)) NA_integer_ else as.integer(scen * N)
  list(na_habillage = as.integer(na_lignes), duree_hors_perimetre = as.integer(hors), colonnes = cols, repli = repli, N = N, scenarios = scen, lignes = nrow(df), lignes_attendues = att,
       lignes_hors_multiplication = if(is.na(att)) 0L else as.integer(max(0L, nrow(df) - att)), lignes_manquantes = if(is.na(att)) 0L else as.integer(max(0L, att - nrow(df))))
}
# Unicité de id_scenario dans un livrable (toutes branches, campagnes NOUVELLES : les tenues admin sont suffixées -aN) ;
# les livrables ADOPTÉS (corpus historiques : 2 tenues par scénario court sans suffixe, convention v7.1.2) ne sont pas soumis à ce contrôle.
controle_unicite_ids <- function(ids){ ids <- ids[!is.na(ids)]; as.integer(sum(duplicated(ids))) }   # NA (modes historiques sans identifiant) ignorés

# --- K3. Adoption de C1 (longs + courts historiques, SANS re-tirage) ---------------------------------------------
# Candidats : dossiers ou fichiers scenarios_longs_tirage_v8_<AAAAMMJJ> dans un dossier de recherche ; le choix est
# toujours explicite quand il y a plusieurs candidats (jamais de choix silencieux).
candidats_corpus_longs <- function(dossier){
  if(is.null(dossier) || !dir.exists(dossier)) return(character(0))
  sort(list.files(dossier, pattern = "^scenarios_longs_tirage_v8_[0-9]{8}(\\.parquet)?$", full.names = TRUE))
}
choisir_source_longs <- function(explicite = NULL, candidats = character(0)){
  if(!is.null(explicite)){ if(!file.exists(explicite)) stop("adoption : source longs introuvable : " %+% explicite, call. = FALSE); return(list(source = explicite, message = "source longs (argument explicite) : " %+% explicite)) }
  if(length(candidats) == 0) stop("adoption : aucun corpus longs daté (scenarios_longs_tirage_v8_<AAAAMMJJ>) trouvé ; passez source_longs = \"<chemin>\" explicitement.", call. = FALSE)
  if(length(candidats) > 1) stop("adoption : plusieurs corpus longs datés trouvés, jamais de choix silencieux — passez source_longs = l'un de : " %+% paste(candidats, collapse = " ; "), call. = FALSE)
  list(source = candidats[1], message = "source longs (candidat unique) : " %+% candidats[1])
}
# Longs adoptés : population reconstituée (cage) si absente, DPEC/TPEC si absents (duree = 3 comme le catalogue), id_profil
# (recette id_v1 sur pivots + graine) et hash_das recalculés s'ils manquent, id_scenario = id_profil-variante ; branche "long".
preparer_longs_adoptes <- function(df, campagne, typo = NULL, populations = POPULATIONS){
  d <- tibble::as_tibble(df)
  manq <- setdiff(c(PIVOTS_LONGS, "graine", "variante", "diagnostic_associes"), names(d)); if(length(manq)) stop("adoption longs : colonnes manquantes : " %+% paste(manq, collapse = ", "), call. = FALSE)
  if(!"population" %in% names(d) || all(is.na(d$population))) d$population <- population_de(as.character(d$cage), populations)
  if(!all(c("DPEC", "TPEC") %in% names(d))){ if(is.null(typo)) stop("adoption longs : typologie requise pour DPEC/TPEC", call. = FALSE); d <- typologie_sejour(d, typo, col_age = "age", col_duree = ".aucune", duree_defaut = 3) }
  d$id_profil <- id_profil_de(dplyr::mutate(d, diagnostic_associes = graine)); d$hash_das <- hash_das_de(d$diagnostic_associes)
  d$variante <- as.integer(d$variante); d$id_scenario <- id_scenario_de(d$id_profil, d$variante)
  d$branche <- "long"; d$campagne <- as.character(campagne)
  reg <- registre_depuis_chunks(d[!duplicated(d$id_scenario), , drop = FALSE], campagne, typo = typo)
  list(df = d, registre = reg)
}
# Courts adoptés : ids (recette id_courts_v1), hash, population, DPEC/TPEC (vraie durée, âge tiré) ; branche "court".
preparer_courts_adoptes <- function(df, campagne, typo = NULL, populations = POPULATIONS){
  d <- tibble::as_tibble(df)
  manq <- setdiff(c(PIVOTS_COURTS, "variante", "diagnostic_associes"), names(d)); if(length(manq)) stop("adoption courts : colonnes manquantes : " %+% paste(manq, collapse = ", "), call. = FALSE)
  d$id_profil <- id_profil_courts_de(d); d$hash_das <- hash_das_de(d$diagnostic_associes)
  d$variante <- as.integer(d$variante); d$id_scenario <- id_scenario_de(d$id_profil, d$variante)
  d$population <- population_de(as.character(d$cage), populations)
  if(!all(c("DPEC", "TPEC") %in% names(d)) && !is.null(typo)) d <- typologie_sejour(d, typo, col_age = "age", col_duree = "duree")
  if(!"lettre" %in% names(d)) d$lettre <- lettre_de(d$diag2)
  d$branche <- "court"; d$campagne <- as.character(campagne)
  list(df = d, registre = registre_depuis_courts(d, campagne, typo, populations))
}
# Cohérence livrable / registre d'une campagne adoptée : scénarios distincts par branche du livrable == lignes du registre.
verifier_adoption <- function(df_livrable, registre_lignes){
  b <- c("long", "court")
  liv <- vapply(b, function(x) dplyr::n_distinct(df_livrable$id_scenario[df_livrable$branche == x]), integer(1))
  reg <- vapply(b, function(x) sum(registre_lignes$branche == x), integer(1))
  list(ok = all(liv == reg), livrable = liv, registre = reg,
       texte = paste(sprintf("%s : livrable %d scénarios distincts / registre %d (%s)", b, liv, reg, ifelse(liv == reg, "égaux", "ÉCART")), collapse = " ; "))
}

## ---- L. Chantier « notebooks par parcours utilisateur » : empreinte de version du code ----
# Empreinte affichée par le chunk `session` de chaque notebook : nombre de fonctions etape_* chargées + hash court (8 hex,
# sha256) de la concaténation des fichiers de code. Pour les déploiements par copie manuelle : le cas réel « notebook à jour
# + etapes.R ancien = fonction inconnue » doit se lire d'un coup d'œil (deux postes à jour affichent la même empreinte ;
# 03_outils_maintenance.Rmd y renvoie). Pure : lit des fichiers, inspecte un environnement, n'écrit rien.
FICHIERS_CODE <- c("config.R", "helpers.R", "etapes.R", "extraction.R", "tirage.R", "utils.R", "referentiels.R", "exclusions.R")
empreinte_version <- function(racine, fichiers = FICHIERS_CODE, env = globalenv()){
  chemins <- file.path(racine, fichiers); presents <- file.exists(chemins)
  contenu <- unlist(lapply(chemins[presents], readLines, warn = FALSE), use.names = FALSE)
  hash <- if(length(contenu)) substr(sha256_vec(paste(contenu, collapse = "\n")), 1, 8) else NA_character_
  noms <- sort(grep("^etape_", ls(env), value = TRUE)); noms <- noms[vapply(noms, function(n) is.function(get(n, envir = env, inherits = FALSE)), logical(1))]
  list(hash = hash, nb_etapes = length(noms), etapes = noms, fichiers = fichiers[presents], manquants = fichiers[!presents],
       texte = sprintf("code : empreinte %s (%d fichiers%s) ; %d fonctions etape_* chargées", hash, sum(presents),
                       if(any(!presents)) " ; ABSENTS : " %+% paste(fichiers[!presents], collapse = ", ") else "", length(noms)))
}
