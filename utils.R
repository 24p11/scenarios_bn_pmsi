library(tidyverse)
pschema = "rflicoteaux-1578."

prep_grep<-function(x) paste(x,collapse = "|")

`%+%` <- function(x,y){paste0(x,y)}

write_xlsx<-function(df,name,sheet="Feuille1",path_out ="~/perso/data"){
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,sheet)
  openxlsx::writeData(wb,sheet,df)
  openxlsx::saveWorkbook(wb,path_out %+% name %+% ".xlsx",overwrite = TRUE)
}

tlg<-function(vec){
  paste0(vec,collapse = "|")
}


prep_data<-function(an,type_etbs){
  
  if(an>22){
    pRatihque::atihble(conn, 'mco' %+% an %+% '.um') |>
      dplyr::mutate(mode_hospit = dplyr::case_when(type_hospum_1 == "P" ~"HP",
                                                   TRUE ~ "HC")) |> 
      dplyr::select(ident,finessgeo,mode_hospit) |> 
      dplyr::distinct(ident,.keep_all = TRUE) |> 
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, 'mco' %+% an %+% '.fixe') |>
                          dplyr::select(anonyme,ident,dp,dr,age,sexe,passage_urg,modesortie,destination,duree,ghm2,rumdudp,nbda)  |> 
                          dplyr::distinct(anonyme,ghm2,.keep_all = TRUE) |> 
                          dplyr::mutate(
                            
                            mode_entree =dplyr::case_when(passage_urg %in% c("5","U","V") ~  "URGENCES" ,
                                                          TRUE  ~ "DOMICILE"),
                            mode_sortie = dplyr::case_when(
                              modesortie %in% ("9")   ~ "DECES",
                              destination %in% ("2")  ~ "SMR",
                              TRUE ~ "DOMICILE"),
                            
                            diag2 = dplyr::case_when( is.na(dr) ~ dp,
                                                      substr(dp,1,1)=="Z" ~ dr,
                                                      TRUE ~ dp ) ,
                            
                            mdp = dplyr::case_when( is.na(dr) ~ "DP",
                                                    substr(dp,1,1)=="Z" ~ dp,
                                                    TRUE ~ "DP" ) ,
                            
                            cage = dplyr::case_when(
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
                              TRUE ~  "[80-["
                            ),
                            cage3 = ifelse(age>=18,"ge_18","lt_18"),
                            racine =substr(ghm2,1,5)
                          ) 
      ) -> query
    
    
  }
  
  if(an>17 & an<=22){
    pRatihque::atihble(conn,"PRD_VUE_MCO_20" %+% an %+%  '.um') |>
      dplyr::mutate(mode_hospit = dplyr::case_when(type_hospum_1 == "P" ~"HP",
                                                   TRUE ~ "HC")) |> 
      dplyr::select(ident,finessgeo,mode_hospit) |> 
      dplyr::distinct(ident,.keep_all = TRUE) |> 
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCO_20" %+% an %+% '.fixe') |>
                          dplyr::filter(substr(ghm2,1,2)!="90") |> 
                          dplyr::distinct(anonyme,ghm2,.keep_all = TRUE) |> 
                          dplyr::select(anonyme,ident,dp,dr,age,sexe,provenance,modesortie,destination,duree,rumdudp,nbda)  |> 
                          
                          dplyr::mutate(
                            mode_entree =dplyr::case_when(provenance =="5"  ~  "URGENCES" ,
                                                          TRUE  ~ "DOMICILE"),
                            mode_sortie = dplyr::case_when(
                              modesortie %in% ("9")   ~ "DECES",
                              destination %in% ("2")  ~ "SMR",
                              TRUE ~ "DOMICILE"),
                            
                            diag2 = dplyr::case_when( is.na(dr) ~ dp,
                                                      substr(dp,1,1)=="Z" ~ dr,
                                                      TRUE ~ dp ) ,
                            
                            mdp = dplyr::case_when( is.na(dr) ~ "DP",
                                                    substr(dp,1,1)=="Z" ~ dp,
                                                    TRUE ~ "DP" ) ,
                            
                            cage = dplyr::case_when(
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
                              TRUE ~  "[80-["
                            ),
                            cage3 = ifelse(age>=18,"ge_18","lt_18"),
                            
                          ) 
      )|> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCO_20" %+% an %+% '.rgp') |>
                          dplyr::distinct(ident,ghmv2023) |> 
                          dplyr::rename(ghm2=ghmv2023) |> 
                          dplyr::mutate(racine =substr(ghm2,1,5))) -> query
    
  }
  
  if(an<=17){
    
    pRatihque::atihble(conn,"PRD_VUE_MCO_20" %+% an %+%  '.um') |>
      dplyr::mutate(mode_hospit = dplyr::case_when(type_hospum_1 == "P" ~"HP",
                                                   TRUE ~ "HC")) |> 
      dplyr::select(ident,finessgeo,mode_hospit) |> 
      dplyr::distinct(ident,.keep_all = TRUE) |> 
      dplyr::left_join(pRatihque::atihble(conn, 'nomgen.finessgeo') |> 
                         dplyr::distinct(finessgeo,categ_pmsi)) |> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCO_20" %+% an %+% '.fixe') |>
                          dplyr::filter(substr(ghm2,1,2)!="90") |> 
                          dplyr::distinct(anonyme,ghm2,.keep_all = TRUE) |> 
                          dplyr::select(anonyme,ident,dp,dr,age,sexe,provenance,modesortie,destination,duree,rumdudp,nbda)  |> 
                          
                          dplyr::mutate(
                            mode_entree =dplyr::case_when(provenance =="5"  ~  "URGENCES" ,
                                                          TRUE  ~ "DOMICILE"),
                            mode_sortie = dplyr::case_when(
                              modesortie %in% ("9")   ~ "DECES",
                              destination %in% ("2")  ~ "SMR",
                              TRUE ~ "DOMICILE"),
                            
                            diag2 = dplyr::case_when( is.na(dr) ~ dp,
                                                      substr(dp,1,1)=="Z" ~ dr,
                                                      TRUE ~ dp ) ,
                            
                            mdp = dplyr::case_when( is.na(dr) ~ "DP",
                                                    substr(dp,1,1)=="Z" ~ dp,
                                                    TRUE ~ "DP" ) ,
                            
                            cage = dplyr::case_when(
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
                              TRUE ~  "[80-["
                            ),
                            cage3 = ifelse(age>=18,"ge_18","lt_18"),
                            
                          ) 
      )|> 
      dplyr::inner_join(pRatihque::atihble(conn, "PRD_VUE_MCO_20" %+% an %+% '.rgp') |>
                          dplyr::distinct(ident,ghmv2021) |> 
                          dplyr::rename(ghm2=ghmv2021) |> 
                          dplyr::mutate(racine =substr(ghm2,1,5))) -> query
    
    
    
  }
  
  query |> dplyr::select(anonyme,ident,mode_hospit,mode_entree,mode_sortie,sexe,categ_pmsi,cage3,cage,racine,ghm2,diag2,mdp,rumdudp,nbda,duree)  |>
    dplyr::filter(substr(ghm2,1,2)!="90") |> 
    dplyr::rename(age = cage3) |> 
    dplyr::compute("prep_data_" %+% an,temporary=TRUE,overwrite=TRUE)
  
  
  
}


rnorm_limits <- function(n, min = 1, max = 3) {
  x <- rnorm(n)
  x <- (max - min) * x/diff(range(x))
  return(x - min(x) + min)
}

age_aleatoire <- function(intervalle) {
  # Extraire les nombres de l'intervalle (ex: "[50-60[" -> 50 et 60)
  nombres <- as.numeric(unlist(regmatches(intervalle, gregexpr("[0-9]+", intervalle))))
  
  # Si un seul nombre est trouvé (ex: "[50-["), ajouter 92 comme deuxième nombre
  if (length(nombres) == 1) {
    nombres <- c(nombres, 92)
  }
  
  # Vérifier que l'intervalle est valide
  if (length(nombres) != 2 || nombres[1] >= nombres[2]) {
    stop("Format d'intervalle invalide. Utilisez un format comme '[50-60['.")
  }
  
  # Tirer au sort une valeur entière dans l'intervalle [min, max[
  age_aleatoire <- sample(seq(nombres[1], nombres[2] - 1), size = 1)
  
  return(age_aleatoire)
}


sample_vars<-function(mode_hospit,sexe,age,ghm2,diag2,nb){
  
  mode_hospit_sel = as.character(mode_hospit)
  sexe_sel = as.character(sexe)
  age_sel = as.character(age)
  ghm2_sel = as.character(ghm2)
  diag2_sel = as.character(diag2)
  nb_sel = as.integer(nb)
  
  df_stat |> dplyr::filter(mode_hospit==mode_hospit_sel,sexe == sexe_sel,age == age_sel, ghm2 == ghm2_sel, diag2 == diag2_sel )-> tmp

    if(nrow(tmp)<1) {
      tibble::as_tibble(list("mode_hospit"=mode_hospit,"sexe" = sexe_sel, "age" = age_sel,"ghm2" = ghm2_sel, "diag2"=diag2_sel)) |> 
        merge(tibble::as_tibble(list("mode_entree" =rep(NA,nb_sel),
                                     "mode_sortie" =rep(NA,nb_sel),
                                     "agean" = rep(NA,nb_sel),
                                     "duree" = rep(NA,nb_sel)
        )) ) |> tibble::as_tibble() -> df_tmp
      return(df_tmp)
    }

  if(mode_hospit_sel=="HP"){
    
    samples_ds = rep(0,nb_sel)
    samples_mde = rep('DOMICILE',nb_sel)
    samples_mds = rep('DOMICILE',nb_sel)
    
  }else{
    
    vec_mode_entree = stringr::str_split(tmp$mode_entree,",")[[1]]
    vec_prob_mode_entree = stringr::str_split(tmp$prob_mode_entree,",")[[1]]
    vec_mode_sortie = stringr::str_split(tmp$mode_sortie,",")[[1]]
    vec_prob_mode_sortie = stringr::str_split(tmp$prob_mode_sortie,",")[[1]]
  
    
    sample(x= vec_mode_entree,prob = vec_prob_mode_entree,size = nb_sel,replace=TRUE) -> samples_mde
    sample(x= vec_mode_sortie,prob = vec_prob_mode_sortie,size = nb_sel,replace=TRUE) -> samples_mds
    
    if(tmp$dms==0) samples_ds = rep(0,nb_sel)
    else round(rnorm_limits(nb_sel,tmp$dms,tmp$vards)) -> samples_ds
  }
  
  
  vec_cage = stringr::str_split(tmp$cage,",")[[1]]
  vec_prob_cage = stringr::str_split(tmp$prob_cage,",")[[1]]
  
  sample(x= vec_cage,prob = vec_prob_cage,size = nb_sel,replace=TRUE) -> samples_cage
  samples_cage<- as.integer(sapply(samples_cage,age_aleatoire))
  

  
  tibble::as_tibble(list("mode_hospit"=mode_hospit,"sexe" = sexe_sel, "age" = age_sel,"ghm2" = ghm2_sel, "diag2"=diag2_sel)) |> 
    merge(tibble::as_tibble(list("mode_entree" =samples_mde,
                                 "mode_sortie" =samples_mds,
                                 "agean" = samples_cage,
                                 "duree" = samples_ds
    )) ) |> tibble::as_tibble() -> df_tmp
  
  return(df_tmp)
  
}


# lire_codes_diabete.R
# Lecture de listes_codes_diabete.yaml et extraction des codes CIM-10
# en tibble « plat » : une ligne par (groupe, code), avec libellé quand présent.
#
# Dépendances : yaml + tidyverse (purrr, dplyr, tibble, stringr)
#   install.packages(c("yaml", "tidyverse"))

lire_codes_diabete <- function(chemin_yaml) {
  d <- yaml::read_yaml(chemin_yaml)
  
  # Sections sans codes à extraire (règles, méta, questions ouvertes)
  d$meta <- NULL
  d$contraintes <- NULL
  d$a_valider_dim <- NULL
  
  # Un code CIM-10 feuille : lettre + 2 chiffres, puis 1 à 3 chiffres après le point
  motif_cim <- "^[A-Z][0-9]{2}(\\.[0-9]{1,3})?$"
  
  extraire <- function(x, chemin = character()) {
    if (is.list(x)) {
      # Cas 1 : un bloc {code: ..., libelle: ...} -> une ligne, sans redescendre
      # (redescendre compterait le champ `code` une seconde fois)
      if (!is.null(x$code) && is.character(x$code)) {
        return(tibble::tibble(
          section = if (length(chemin) > 0) chemin[[1]] else NA_character_,
          chemin  = paste(chemin, collapse = "/"),
          code    = x$code,
          libelle = x$libelle %||% NA_character_
        ))
      }
      # Cas 2 : liste/dict quelconque -> récursion (le nom du champ enrichit le chemin)
      noms <- names(x) %||% rep("", length(x))
      purrr::map2(x, noms, \(v, n) extraire(v, c(chemin, n[nzchar(n)]))) |> purrr::list_rbind()
    } else if (is.character(x)) {
      # Cas 3 : scalaire ou vecteur de codes nus (ex. [E11.00, E11.08], coma: E10.0)
      codes <- x[str_detect(x, motif_cim)]
      if (length(codes) == 0) return(tibble())
      tibble::tibble(
        section = if (length(chemin) > 0) chemin[[1]] else NA_character_,
        chemin  = paste(chemin, collapse = "/"),
        code    = codes,
        libelle = NA_character_
      )
    } else {
      tibble::tibble()
    }
  }
  
  extraire(d) |>
    dplyr::distinct(chemin, code, .keep_all = TRUE)
}

retro_code_diabete<-function(neocode,comp){
  
  dplyr::case_when(neocode=="E10"~"E10" %+%comp,
                   neocode=="E11ni"~"E11"%+%comp%+%"0",
                   neocode=="E11i"~"E11"%+%comp%+%"8")
  
}


get_codes_diabete_from_neo<-function(diabete_,cage_){
  

     df_res_epi_comp_diabete |> dplyr::filter(diabete == diabete_,cage==cage_) -> prep_diab
      
      sample(x= prep_diab$comp,prob = prep_diab$nb,size = 1)-> comp
      code_diabete_sample<-retro_code_diabete(diabete_,comp)
      
      if(comp=="8"){
        comp="9"
        code_diabete_sample<-retro_code_diabete(diabete_,9)
      }
      
      if(comp=="7"){comp=sample(c("2","3","4","5","6"),sample(3:4,1))}
      
  
    if(!"9"%in%comp){
      
      comp_diag<-NULL
      
      for(c in comp){   
        comp_diag_diag_tmp <- dplyr::case_when( c=="2"~sample(codes_diab$code[grepl("renal/asterisques_obligatoires",codes_diab$chemin)],1),
                                                c=="3"~sample(codes_diab$code[grepl("oculaire/asterisques_obligatoires",codes_diab$chemin)],1), 
                                                c=="4"~sample(codes_diab$code[grepl("neurologique/asterisques_obligatoires",codes_diab$chemin)],1), 
                                                c=="5"~sample(codes_diab$code[grepl("vasculaire_peripherique/asterisques_obligatoires",codes_diab$chemin)],1), 
                                                c=="6"~sample(codes_diab$code[grepl("autres_precisees/asterisques_obligatoires",codes_diab$chemin)],1)
        )
        
        comp_diag<-c(comp_diag,comp_diag_diag_tmp)
      }   
      
      code_diabete_sample = c(comp_diag,code_diabete_sample)
    }
    

  
  return(code_diabete_sample)
}
