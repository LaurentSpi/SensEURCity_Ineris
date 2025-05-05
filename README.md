# Projet SENSEURCITY CALC - Documentation

Ce dépôt contient une suite de scripts R implémentant la méthodologie décrite dans Wesseling et al., 2024 selon l'approche INERIS. Ces scripts permettent de traiter, nettoyer, classifier, détecter des valeurs aberrantes et calibrer les données issues de capteurs bas-coût (Low-Cost Sensors ou LCS) utilisés pour la surveillance de la qualité de l'air, en particulier pour les particules fines PM2.5.

## Table des matières

1. [Prérequis](#prérequis)
2. [Installation](#installation)
3. [Structure des données](#structure-des-données)
4. [Configuration des chemins](#configuration-des-chemins)
5. [Ordre d'exécution des scripts](#ordre-dexécution-des-scripts)
6. [Description détaillée des scripts](#description-détaillée-des-scripts)
7. [Résultats et sorties](#résultats-et-sorties)
8. [Dépannage et problèmes courants](#dépannage-et-problèmes-courants)
9. [Références](#références)

## Prérequis

Pour exécuter ces scripts, vous aurez besoin de :

- R (version 4.0 ou supérieure)
- RStudio (recommandé pour une utilisation plus facile)
- Les packages R suivants :
  - dplyr, data.table, tidyverse (manipulation de données)
  - ggplot2, RColorBrewer (visualisation)
  - sf, raster, sp, rgdal (données spatiales)
  - openair (analyse spécifique à la qualité de l'air)
  - chron (gestion des séries temporelles)
  - stats, optimization, pracma (analyses statistiques)
  - leaflet, mapview (cartographie interactive)

## Installation

1. Clonez ce dépôt dans un répertoire local.
2. Ouvrez R ou RStudio et installez les packages nécessaires :

```r
install.packages(c("dplyr", "data.table", "tidyverse", "ggplot2", "RColorBrewer",
                   "sf", "raster", "sp", "rgdal", "openair", "chron", 
                   "optimization", "pracma", "leaflet", "mapview"))
```

3. Vous devrez également disposer de trois scripts externes (que vous trouverez dans le dossier Code R) :
   - `SensorIneris_Toolbox.R`
   - `uBss and uCi.R`
   - `interpolate.R` (pour la calibration)

Ces scripts seront demandés lors de l'exécution via des fenêtres de dialogue.

## Structure des données

Le projet attend la structure de répertoires suivante :

```
[path_project_root]/
├── ALT_SensEURCity/
│   ├── INPUTS/
│   │   ├── metadata/
│   │   │   └── metadata_sites.csv
│   │   ├── SIG_data/
│   │   │   ├── legend_CLC_V4.csv
│   │   │   └── CLC_Netherlands_NT.tif
│   │   ├── Stations_RIVM_LML_Feb2020_format_modif.csv
│   │   ├── gadm41_BEL_2.shp
│   │   ├── ref_df_all.csv
│   │   └── LCS_df_all.csv
│   ├── OUTPUTS/
│   │   ├── figs/
│   │   │   ├── outliers_detection/
│   │   │   └── outliers_detection TEST/
│   │   └── df_correlations_timeplots_RefSensData_ALL/
│   │       └── after_calibration/
│   │           ├── correlation_plots/
│   │           └── timeSeries_plots/
│   └── dataset/
│       └── [fichiers CSV des capteurs]
```

## Configuration des chemins

Tous les chemins sont centralisés dans le fichier `00_paths_and_setting.R`. Vous devez modifier uniquement la variable `path_project_root` pour qu'elle pointe vers votre répertoire racine du projet :

```r
# Root project directory - CHANGE THIS TO YOUR PATH
path_project_root <- "C:/Votre/Chemin/Vers/Le/Projet"
```

Ce fichier de configuration :
- Définit tous les chemins vers les répertoires d'entrée et de sortie
- Définit les chemins vers tous les fichiers d'entrée et de sortie
- Configure les paramètres communs (nom du polluant, paramètres de calibration, tailles des figures)
- Crée automatiquement les répertoires nécessaires s'ils n'existent pas

## Ordre d'exécution des scripts

Pour un traitement complet des données, exécutez les scripts dans l'ordre suivant :

1. `df_creation.r` - Création des dataframes initiaux
2. `021data_cleaning.R` - Nettoyage initial des données
3. `031_typology_comparison.R` - Comparaison des classifications typologiques
4. `032_data_classification.R` - Classification des capteurs
5. `041_outliers_detection.R` - Détection des valeurs aberrantes
6. `042_timesplot_correlation_SensorReferenceData.R` - Visualisation avant calibration
7. `051_data_calibration.R` - Calibration des données
8. `052_afterCalibration_timesplot_correlation_SensorReferenceData.R` - Visualisation après calibration

## Description détaillée des scripts

### 1. `00_paths_and_setting.R`
**Rôle :** Centralise tous les chemins et paramètres pour le projet.\

**Fonctionnalités :**
- Définit les chemins vers tous les répertoires et fichiers
- Configure les paramètres communs comme le nom du polluant et les paramètres de calibration
- Crée automatiquement les répertoires nécessaires

### 2. `df_creation.r`
**Rôle :** Création des dataframes initiaux à partir des données brutes.\
**Fonctionnalités :**
- Importe les fichiers CSV des capteurs depuis le répertoire `dataset`
- Effectue une agrégation horaire des données
- Crée les fichiers de base `LCS_df_all.csv` et `ref_df_all.csv` qui seront utilisés par les autres scripts
- Gère trois types de données : OPC, PMS et REF (référence)

### 3. `021data_cleaning.R`
**Rôle :** Nettoyage initial des données capteurs.\
**Fonctionnalités :**
- Supprime les valeurs négatives
- Élimine les valeurs supérieures à un seuil basé sur les stations de référence
- Identifie et supprime les valeurs "figées" (persistant pendant plus de 3 heures)
- Élimine les capteurs présentant un biais constant positif
- Sauvegarde les données nettoyées dans `LCS_df_all_clean.Rda`
- Génère un histogramme de distribution des concentrations de PM2.5

### 4. `031_typology_comparison.R`
**Rôle :** Comparaison des classifications typologiques.\
**Fonctionnalités :**
- Charge les métadonnées des sites et extrait les informations typologiques
- Convertit les coordonnées dans un système de projection adapté
- Extrait les classifications Corine Land Cover (CLC) pour chaque site
- Compare les typologies issues de la base de données et du CLC
- Sauvegarde les résultats dans `typo_CLC_BDD_comparison.Rda/csv`

### 5. `032_data_classification.R`
**Rôle :** Classification détaillée des capteurs.\
**Fonctionnalités :**
- Attribue une typologie (URB, TRA, INDUS) à chaque capteur
- Ajoute des informations temporelles (saisons, périodes de la journée)
- Classifie les capteurs en trois catégories : "dedicated", "colocated", et "duplicated-colocated"
- Réalise un clustering spatial des capteurs
- Génère des cartes interactives de localisation des capteurs
- Crée des graphiques de distribution par typologie
- Sauvegarde les données classifiées dans `LCS_df_all_clean_groups.Rda`

### 6. `041_outliers_detection.R`
**Rôle :** Détection statistique des valeurs aberrantes.\
**Fonctionnalités :**
- Traite séparément chaque groupe de capteurs
- Applique une transformation en racine carrée pour stabiliser la variance
- Calcule les moyennes et écarts-types via une validation croisée
- Optimise par maximisation de la log-vraisemblance pour identifier les outliers
- Génère des graphiques montrant les outliers détectés
- Sauvegarde les données annotées dans `LCS_df_all_clean_groups_outliers.Rda`

### 7. `042_timesplot_correlation_SensorReferenceData.R`
**Rôle :** Génération de graphiques avant calibration.\
**Fonctionnalités :**
- Crée des séries temporelles comparant les capteurs aux stations de référence
- Génère des graphiques de corrélation entre capteurs et stations de référence
- Calcule des statistiques de performance (R², RMSE)
- Sauvegarde les graphiques dans `correlation_plots` et `timeSeries_plots`
- Sauvegarde les données colocalisées dans `colocated_LCS_df_all_clean.Rda`

### 8. `051_data_calibration.R`
**Rôle :** Calibration des données capteurs.\
**Fonctionnalités :**
- Forme des groupes de capteurs autour des stations de référence
- Calcule les facteurs de calibration horaires basés sur la méthodologie RIVM et uBss
- Interpole les facteurs de correction pour les capteurs non colocalisés
- Applique les corrections aux mesures brutes
- Sauvegarde les données calibrées dans `calibratedSensorsAlltime.Rda`
- Sauvegarde les facteurs de calibration dans `calibrationFactorsAlltime_alltime_nmax1000_distmaxRepmax_outliers.csv`

### 9. `052_afterCalibration_timesplot_correlation_SensorReferenceData.R`
**Rôle :** Génération de graphiques après calibration.\
**Fonctionnalités :**
- Crée des séries temporelles comparant les capteurs calibrés aux stations de référence
- Génère des graphiques de corrélation après calibration
- Calcule des statistiques de performance post-calibration (R², RMSE)
- Permet d'évaluer l'efficacité de la calibration
- Sauvegarde les graphiques dans les répertoires appropriés

## Résultats et sorties

Le pipeline de traitement génère :

1. **Données nettoyées et classifiées**
   - `LCS_df_all_clean.Rda` - Données après nettoyage
   - `LCS_df_all_clean_groups.Rda` - Données après classification
   - `LCS_df_all_clean_groups_outliers.Rda` - Données après détection des outliers

2. **Données de calibration**
   - `calibratedSensorsAlltime.Rda` - Données capteurs après calibration
   - `calibrationFactorsAlltime_alltime_nmax1000_distmaxRepmax_outliers.csv` - Facteurs de calibration

3. **Visualisations**
   - Histogrammes de distribution des concentrations
   - Cartes de répartition spatiale des capteurs
   - Graphiques de séries temporelles avant/après calibration
   - Graphiques de corrélation avant/après calibration
   - Boxplots par typologie et période

## Dépannage et problèmes courants

### Problèmes de chemins
Si vous rencontrez des erreurs liées aux chemins de fichiers :
1. Vérifiez que `path_project_root` dans `00_paths_and_setting.R` pointe vers le bon répertoire
2. Assurez-vous que tous les scripts utilisent le fichier de configuration pour leurs chemins
3. Vérifiez que la structure de répertoires correspond à celle attendue

### Erreurs de packages
Si vous rencontrez des erreurs liées aux packages R :
```r
install.packages("nom_du_package_manquant")
```

### Données insuffisantes
Les scripts supposent une certaine densité de données. Si vous avez peu de capteurs ou de stations de référence, certaines fonctionnalités (comme l'interpolation spatiale) pourraient ne pas fonctionner correctement. Prévoyez dans ce cas de petits ajustements au niveau de votre jeu de données.

## Références

- Wesseling, J., et al. (2024). INERIS Methodology for PMS Calibration
- Guide CEN pour l'évaluation de capteurs dans l'air ambiant
- Documentation des packages R utilisés (openair, sf, etc.)

---

Pour toute question ou problème, n'hésitez pas à ouvrir une issue dans le dépôt GitHub ou à contacter directement les auteurs.
