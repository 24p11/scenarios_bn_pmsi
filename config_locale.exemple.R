###############################################################################
# config_locale.exemple.R — MODÈLE de configuration propre au POSTE (à copier en config_locale.R)
#
# Niveau 2 des trois niveaux de paramètres (config.R = doctrine et défauts, versionné ; config_locale.R = le
# poste, local et gitignoré ; campagne.R / palier.R = la décision d'exploitation, écrite depuis les notebooks).
# config_locale.R est sourcé par config.R (et par les deux lanceurs, pour trouver la racine) s'il existe à la
# racine du dépôt, AVANT le bloc PROFIL. Sans lui ni la variable d'environnement SCENARIOS_PMSI_PATH, le pipeline
# s'arrête avec un message explicite. Les tests et le mode démo posent leurs chemins eux-mêmes.
# Multi-utilisateurs : chacun son config_locale.R (chemins, pschema), les magasins partagés (PATH_RESULTS) sont
# communs à l'équipe, les tables temporaires en base sont disjointes par pschema.
# Une ligne par clé : rôle — exemple FACTICE — qui la fournit. Décommenter et adapter.
###############################################################################

## SCENARIOS_PMSI_PATH — racine du dépôt (le dossier contenant config.R) ; obligatoire si la variable
##   d'environnement du même nom n'est pas définie — fournie par l'utilisateur (l'alias path_projet en dérive).
# SCENARIOS_PMSI_PATH <- "/home/utilisateur/projets/scenarios_bn_pmsi/"

## PATH_RESULTS — répertoire de travail (arborescence par étapes 00_partiels/ … <profil>/60_export_final/), NOUVEAU et
##   vide au départ si distinct de <racine>/results/ (défaut) ; commun à l'équipe quand les magasins sont partagés —
##   fourni par le responsable du répertoire de travail (voir RUN.md, changement de répertoire / etape_reorganiser).
# PATH_RESULTS <- "/home/equipe/travail/scenarios_pmsi_resultats/"

## pschema — schéma personnel de la plateforme pour les tables temporaires (utils.R, héritage v7 ; inutile au v8) —
##   fourni par la plateforme à chaque utilisateur.
# pschema <- "prenom-nom-0000."

## Toute autre valeur propre au poste (évaluée AVANT le bloc PROFIL). Les décisions d'exploitation (CAMPAGNE, budget,
## k, registre) n'ont PAS leur place ici : chunk ouvrir_campagne (campagne.R) ou palier_surcharge (palier.R).
# SEUIL_ALERTE_GO <- 10
