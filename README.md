---
title: "Projet SENSEURCITY CALC - Documentation"
output:
  html_document:
    toc: true
    toc_depth: 3
    number_sections: true
    theme: united
  pdf_document:
    toc: true
    toc_depth: 3
    number_sections: true
  word_document:
    toc: true
    toc_depth: 3
editor_options: 
  markdown: 
    wrap: 72
---

# Projet SENSEURCITY CALC - Documentation

Ce dépôt contient une suite de scripts R implémentant la méthodologie
décrite dans Wesseling et al., 2024 selon l'approche INERIS ainsi que
deux algos (Isolation Forest et Local Outlier Factor) de détections
d'outliers développés au cours de l'alternance 2024-2025. Ces scripts
permettent de traiter, nettoyer, classifier, détecter des valeurs
aberrantes et calibrer les données issues de capteurs bas-coût (Low-Cost
Sensors ou LCS) utilisés pour la surveillance de la qualité de l'air, en
particulier pour les particules fines PM2.5.

## Table des matières

1.  [Prérequis](#prérequis)\
2.  [Installation](#installation)\
3.  [Structure des données](#structure-des-données)\
4.  [Configuration des chemins](#configuration-des-chemins)\
5.  [Ordre d'exécution des scripts](#ordre-dexécution-des-scripts)\
6.  [Description détaillée des
    scripts](#description-détaillée-des-scripts)\
7.  [Résultats et sorties](#résultats-et-sorties)\
8.  [Dépannage et problèmes courants](#dépannage-et-problèmes-courants)\
9.  [Références](#références)

## Prérequis {#prérequis}

Pour exécuter ces scripts, vous aurez besoin de :

-   R (version 4.0 ou supérieure)\
-   RStudio (recommandé pour une utilisation plus facile)\
-   Les packages R suivants :
    -   dplyr, data.table, tidyverse (manipulation de données)
    -   ggplot2, RColorBrewer (visualisation)
    -   sf, raster, sp, rgdal (données spatiales)
    -   openair (analyse spécifique à la qualité de l'air)
    -   chron (gestion des séries temporelles)
    -   stats, optimization, pracma (analyses statistiques)
    -   leaflet, mapview (cartographie interactive)

## Installation {#installation}

1.  Clonez ce dépôt dans un répertoire local.\
2.  Ouvrez R ou RStudio et installez les packages nécessaires :

``` r
install.packages(c("dplyr","data.table","tidyverse","ggplot2","RColorBrewer",
                   "sf","raster","sp","rgdal","openair","chron",
                   "optimization","pracma","leaflet","mapview","dbscan","RANN","isotree","tictoc"))
```

<<<<<<< HEAD:README.md
3. Vous devrez également disposer de trois scripts externes (que vous trouverez dans le dossier Code R) :
   - `SensorIneris_Toolbox.R`
   - `uBss and uCi.R`
   - `interpolate.R` (pour la calibration)
=======
3.  Vous devrez également disposer de trois scripts externes :
    -   `SensorIneris_Toolbox.R`
    -   `uBss and uCi.R`
    -   `interpolate.R` (pour la calibration)
>>>>>>> f020b6f (Dernière version des scripts/README avec rajout des algos de IsoForest et LOF):README_V2.md

Ces scripts seront demandés lors de l'exécution via des fenêtres de
dialogue.

## Structure des données {#structure-des-données}

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

## Configuration des chemins {#configuration-des-chemins}

Tous les chemins sont centralisés dans le fichier
`00_paths_and_setting.R`. Vous devez modifier uniquement la variable
`path_project_root` pour qu'elle pointe vers votre répertoire racine du
projet :

``` r
# Root project directory - CHANGE THIS TO YOUR PATH
path_project_root <- "C:/Votre/Chemin/Vers/Le/Projet"
```

Ce fichier de configuration :\
- Définit tous les chemins vers les répertoires d'entrée et de sortie\
- Définit les chemins vers tous les fichiers d'entrée et de sortie\
- Configure les paramètres communs (nom du polluant, paramètres de
calibration, tailles des figures)\
- Crée automatiquement les répertoires nécessaires s'ils n'existent pas

## Ordre d'exécution des scripts {#ordre-dexécution-des-scripts}

Pour un traitement complet des données, exécutez les scripts dans
l'ordre suivant :\

1.  `df_creation.r` - Création des dataframes initiaux\
2.  `021data_cleaning.R` - Nettoyage initial des données\
3.  `031_typology_comparison.R` - Comparaison des classifications
    typologiques\
4.  `032_data_classification.R` - Classification des capteurs\
5.  **Détection des valeurs aberrantes — choisir une (1) variante** :\
    -   `041_outliers_detection.R` — Méthode classique
        (log-vraisemblance / CV)\
    -   `041_outliers_detection_IsoForest.R` — Isolation Forest +
        système de votes (seuils alignés par `contamination`)\
    -   `041_outliers_detectionLOF.R` — LOF + densité + filtre global
        (seuils alignés par `contamination`)\
6.  `042_timesplot_correlation_SensorReferenceData.R` - Visualisation
    avant calibration\
7.  `051_data_calibration.R` - Calibration des données\
8.  `052_afterCalibration_timesplot_correlation_SensorReferenceData.R` -
    Visualisation après calibration

> ⚠️ Ne lancez **qu’une seule** variante 041 par run. Les fichiers de
> sortie diffèrent et servent d’entrée aux étapes suivantes.

### Vue d’ensemble du pipeline

``` mermaid
flowchart TD
    A[df_creation.r] --> B[021data_cleaning.R]
    B --> C[031_typology_comparison.R]
    C --> D[032_data_classification.R]
    D --> E{041 : Détection des outliers<br/>(choisir 1 variante)}
    E --> E1[041_outliers_detection.R<br/>Historique]
    E --> E2[041_outliers_detection_IsoForest.R<br/>Isolation Forest + votes]
    E --> E3[041_outliers_detectionLOF.R<br/>LOF + densité + filtre global]
    E1 --> F[042_timesplot_correlation_SensorReferenceData.R]
    E2 --> F
    E3 --> F
    F --> G[051_data_calibration.R]
    G --> H[052_afterCalibration_timesplot_correlation_SensorReferenceData.R]
```

## Description détaillée des scripts {#description-détaillée-des-scripts}

### 1. `00_paths_and_setting.R`
<<<<<<< HEAD:README.md
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
=======

**Rôle :** Centralise tous les chemins et paramètres pour le projet.\
**Fonctionnalités :**\
- Définit les chemins vers tous les répertoires et fichiers\
- Configure les paramètres communs (nom du polluant, paramètres de
calibration, tailles des figures)\
- Crée automatiquement les répertoires nécessaires

### 2. `df_creation.r`

**Rôle :** Création des dataframes initiaux à partir des données
brutes.\
**Fonctionnalités :**\
- Importe les fichiers CSV des capteurs depuis le répertoire `dataset`\
- Effectue une agrégation horaire des données\
- Crée les fichiers de base `LCS_df_all.csv` et `ref_df_all.csv`\
- Gère trois types de données : OPC, PMS et REF (référence)

### 3. `021data_cleaning.R`

**Rôle :** Nettoyage initial des données capteurs.\
**Fonctionnalités :**\
- Supprime les valeurs négatives\
- Élimine les valeurs supérieures à un seuil basé sur les stations de
référence\
- Identifie et supprime les valeurs "figées" (≥ 3 heures)\
- Élimine les capteurs présentant un biais constant positif\
- Sauvegarde : `LCS_df_all_clean.Rda`\
- Graphe : histogramme de la distribution PM2.5

### 4. `031_typology_comparison.R`

**Rôle :** Comparaison des classifications typologiques.\
**Fonctionnalités :**\
- Chargement des métadonnées et extraction des typologies\
- Conversion des coordonnées en projection métrique adaptée\
- Extraction CLC pour chaque site et comparaison BDD vs CLC\
- Sorties : `typo_CLC_BDD_comparison.Rda/csv`

### 5. `032_data_classification.R`

**Rôle :** Classification détaillée des capteurs.\
**Fonctionnalités :**\
- Attribution de typologie (URB, TRA, INDUS)\
- Enrichissement temporel (saisons, périodes)\
- Catégories : "dedicated", "colocated", "duplicated-colocated"\
- Clustering spatial + cartes interactives\
- Sortie : `LCS_df_all_clean_groups.Rda`

### 6. `041_outliers_detection.R` (méthode historique)

**Rôle :** Détection statistique via transformation racine carrée,
validation croisée et optimisation de la log-vraisemblance.\
**Fonctionnalités :**\
- Travail par groupe (Group/Typology/Season/Cluster)\
- Identification des points aberrants par optimisation de vraisemblance\
- Graphiques d’outliers par groupe\
- Sortie : `LCS_df_all_clean_groups_outliers.Rda`

### 6bis. `041_outliers_detection_IsoForest.R` (nouveau)
>>>>>>> f020b6f (Dernière version des scripts/README avec rajout des algos de IsoForest et LOF):README_V2.md

**Rôle :** Détection d’outliers par **Isolation Forest** enrichie d’un
**système de votes** pour robustifier les décisions.\
**Idée clé :** un **seul paramètre** `contamination` pilote **tous** les
seuils internes pour une cohérence globale.

**Entrées minimales :**\
- Fichier RData des groupes : `LCS_df_all_clean_groups_Rda` (ou
équivalent)\
- Colonnes requises : `PM2.5` (obligatoire), `datetime` (recommandé pour
le vote temporel), variables contextuelles si disponibles
(`Temperature`, `Humidity`, `Pressure`)

**Étapes principales :**\
1. **Prétraitement** — Imputation par médiane + normalisation robuste
(médiane/MAD)\
2. **Isolation Forest (IF)** — `ntrees` (ex. 500), `sample_size` (≤
256), multi-thread si dispo ; **seuil IF** =
`quantile(score, 1 - contamination)`\
3. **Système de votes (5 signaux)**\
- IF : score \> seuil IF\
- Global modéré (PM2.5) : bornes
`c(contamination/2, 1 - contamination/2)`\
- Global strict (PM2.5) : bornes
`c(contamination/10, 1 - contamination/10)`\
- Temporel : z-score MAD local, seuil `qnorm(1 - contamination/2)`
(fenêtre ≈ 10%, bornée 30–150)\
- Densité : rayon fixe (hérité) mais cutoff sur le nb de voisins calé au
quantile `contamination`\
4. **Agrégation**\
-
`vote_count = IF + global_modéré + global_strict + temporel + densité`\
- `extreme_outliers` si `vote_count ≥ 4` ou (`IF` & `strict`)\
- `moderate_outliers` si `vote_count ≥ 3` & (`IF` ou `global_modéré`)\
- **Ultra** : `score > quantile(score, 1 - contamination/5)`\
- `final_outliers = extreme_outliers | moderate_outliers | ultra`\
5. **Sorties** — RData : `LCS_df_all_clean_groups_outliers2*.Rda` ;\
Figures : `OPC_Outliers_IsoForest_<Group>.png`

**Paramètres clés :** - `contamination` (ex. 0.05) — bouton unique de
calibration du taux d’alertes\
- `feature_names` — inclure `PM2.5` + variables environnementales si
dispo\
- `ntrees`, `sample_size` — performance/robustesse

**Bonnes pratiques :**\
- Trier par `datetime` avant le vote temporel\
- `set.seed()` pour la reproductibilité\
- Log par groupe : contamination finale et % d’outliers

### 6ter. `041_outliers_detectionLOF.R` (nouveau)

**Rôle :** Détection d’outliers par **LOF (Local Outlier Factor)**,
complétée par un vote **densité (k-distance)** et un **filtre global**
sur PM2.5.\
**Idée clé :** les trois seuils (LOF, densité, global) sont **alignés**
via un unique `contamination`.

**Entrées minimales :**\
- Fichier RData des groupes : `LCS_df_all_clean_groups_Rda` (ou
équivalent)\
- Colonnes : `PM2.5` (recommandée pour le filtre global), variables
contextuelles éventuelles

**Étapes principales :**\
1. **Prétraitement** — Imputation médiane, normalisation robuste
(médiane/MAD)\
2. **Choix de `k` (grid-search léger)** — `k_candidates = c(10,15,25)` ;
sélection du `k` minimisant la variance des extrêmes LOF (fallback sûr
si données petites)\
3. **Votes (3 signaux)**\
- LOF : `lof_score > quantile(lof_score, 1 - contamination)`\
- Densité (k-distance) : `kdist > quantile(kdist, 1 - contamination)`\
- Global (PM2.5) : hors `c(contamination/2, 1 - contamination/2)`\
4. **Agrégation** — décision **ET** :
`final_outliers = vote_lof & vote_dens & vote_glob`\
5. **Sorties** — RData : `LCS_df_all_clean_groups_outliers3*.Rda` ;\
Figures : `LOF_OPC_outliers_<Group>.png`

**Paramètres clés :**\
- `contamination` (ex. 0.05) — contrôle global du taux d’alertes\
- `k_candidates` — compromis robustesse vs sensibilité\
- `feature_names` — inclure `PM2.5` si possible (sinon vote global
neutre)

**Notes de performance :**\
- `RANN::nn2` est rapide ; éviter des `k` trop grands si `n` est élevé\
- Groupes \< 3 points : renvoie NA/flags neutres

## Résultats et sorties {#résultats-et-sorties}

Le pipeline génère :

1.  **Données nettoyées et classifiées**
    -   `LCS_df_all_clean.Rda` — après nettoyage\
    -   `LCS_df_all_clean_groups.Rda` — après classification\
    -   **Outliers (selon la variante)** :
        -   Historique : `LCS_df_all_clean_groups_outliers.Rda`\
        -   IF : `LCS_df_all_clean_groups_outliers2*.Rda` +
            `_Outliers_IsoForest_*.png`\
        -   LOF : `LCS_df_all_clean_groups_outliers3*.Rda` +
            `_Outliers_LOF_*.png`
2.  **Données de calibration**
    -   `calibratedSensorsAlltime.Rda`\
    -   `calibrationFactorsAlltime_alltime_nmax1000_distmaxRepmax_outliers.csv`
3.  **Visualisations**
    -   Histogrammes, cartes, séries temporelles avant/après
        calibration\
    -   Graphiques de corrélation\
    -   Boxplots par typologie et période

## Dépannage et problèmes courants {#dépannage-et-problèmes-courants}

### Problèmes de chemins

<<<<<<< HEAD:README.md
### Erreurs de packages
Si vous rencontrez des erreurs liées aux packages R :
```r
install.packages("nom_du_package_manquant")
```

### Données insuffisantes
Les scripts supposent une certaine densité de données. Si vous avez peu de capteurs ou de stations de référence, certaines fonctionnalités (comme l'interpolation spatiale) pourraient ne pas fonctionner correctement. Prévoyez dans ce cas de petits ajustements au niveau de votre jeu de données.
=======
1.  Vérifiez `path_project_root` dans `00_paths_and_setting.R`\
2.  Assurez-vous que tous les scripts utilisent ce fichier de
    configuration\
3.  Vérifiez la structure des répertoires

### Packages manquants

Installer le package manquant :

``` r
install.packages("nom_du_package_manquant")
```

### Paramètre `contamination` (IF & LOF)
>>>>>>> f020b6f (Dernière version des scripts/README avec rajout des algos de IsoForest et LOF):README_V2.md

-   **Un seul paramètre** pour piloter tous les seuils internes\
-   Recommandé : 0.03–0.08 selon la variabilité des groupes\
-   Si trop d’alertes : diminuer la valeur ; si trop peu : l’augmenter
    légèrement

### Groupes trop petits

-   \< 3 points : IF/LOF peuvent retourner des NA ; basculer sur la
    méthode historique ou regrouper davantage

### Colonnes attendues

-   `PM2.5` doit exister (sinon les votes globaux échouent)\
-   `datetime` recommandé (vote temporel IF et graphique)

## Références {#références}

-   Wesseling, J., et al. (2024). INERIS Methodology for PMS Calibration
-   Breunig, M. et al., LOF : Identifying Density-Based Local Outliers\
-   Ester, M., Kriegel, H.-P., Sander, J., Xu, X. A Density-Based
    Algorithm for Discovering Clusters in Large Spatial Databases with
    Noise (DBSCAN). Proceedings of the Second International Conference
    on Knowledge Discovery and Data Mining (KDD-96)\
-   Von Zoest, J. et al., Robust Statistical Methods for Outlier
    Detection in Low-Cost Air Quality Sensor Data\
-   Breunig, M., Kriegel, H.-P., Ng, R., Sander, J. LOF : Identifying
    Density- Based Local Outliers\
-   Guide CEN pour l'évaluation de capteurs dans l'air ambiant\
-   Documentation des packages R utilisés (openair, sf, dbscan, isotree,
    RANN, etc.)

------------------------------------------------------------------------

Pour toute question ou problème, ouvrez une issue sur le dépôt ou
contactez les auteurs.
