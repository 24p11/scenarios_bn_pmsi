###############################################################################
# config_locale.exemple.R — MODÈLE de configuration propre au poste (à copier en config_locale.R)
#
# config_locale.R est IGNORÉ par git (.gitignore) et sourcé par config_v8.R (et les deux lanceurs,
# pour trouver la racine) s'il existe à la racine du dépôt. Il remplace les anciens chemins par
# défaut personnels : sans lui ni la variable d'environnement SCENARIOS_PMSI_PATH, le pipeline
# s'arrête avec un message explicite. Les tests et le mode démo posent leurs chemins eux-mêmes et
# n'en ont pas besoin. Valeurs FACTICES ci-dessous, commentées : décommenter et adapter.
###############################################################################

## Racine du dépôt (obligatoire si SCENARIOS_PMSI_PATH n'est pas définie dans l'environnement)
# SCENARIOS_PMSI_PATH <- "/home/utilisateur/projets/scenarios_bn_pmsi/"

## Schéma personnel de la plateforme (utils.R, héritage v7 ; inutile au v8)
# pschema <- "prenom-nom-0000."

## Toute autre valeur propre au poste (évaluée AVANT le bloc PROFIL de config_v8.R ; les surcharges
## de campagne restent dans SCENARIOS_PMSI_SURCHARGE / palier.R)
# SEUIL_ALERTE_GO <- 10
