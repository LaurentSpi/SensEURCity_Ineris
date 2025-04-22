# Projet SENSEURCITY CALC

Ce dépôt contient une suite de scripts correspondant à la méthodologie décrite dans Wesseling et al., 2024 selon l'approche INERIS. Les scripts permettent de traiter, nettoyer, classifier, détecter des valeurs aberrantes et calibrer les données issues des capteurs bas-coût dédiés à la surveillance de la qualité de l'air.

## Structure des Scripts

### 1. 02PMS_V2_data_cleaning_PM2.5.R
**Objectifs :**
- Nettoyage initial des données capteurs.
- Suppression des valeurs négatives et des valeurs supérieures à un seuil basé sur la station de référence.
- Identification des valeurs "figées" (persistant pendant plus de 3 heures).
- Élimination des capteurs présentant un biais constant.
- Sauvegarde des données nettoyées dans le répertoire OUTPUTS.

**Technologies :**
- R (`dplyr`, `data.table`, `ggplot2`)
---

### 2. 02PMS_V2_timesplot_correlation_SensorReferenceData.R
**Objectifs :**
- Génération de graphiques de séries temporelles et de corrélations entre les données nettoyées et les données des stations de référence.
- Vérification de la cohérence des mesures avant la calibration.

**Technologies :**
- R (`openair`, `ggplot2`)
---

### 3. 03PMS_V2_data_classification_PM2.5.R
**Objectifs :**
- Classification typologique des capteurs en fonction des données géospatiales (land use, démographie, réseau routier).
- Clustering spatial des capteurs par type et saison et ajout d'informations temporelles (saisons, périodes de la journée), et de colonnes telles que typologie, période, jour

**Technologies :**
- R (`sf`, `tidyverse`, `ggplot2`)
---

### 4. 03PMS_V2_typology_comparison.R
**Objectifs :**
- Comparaison des classifications typologiques obtenues par différentes méthodes (ex. données brutes vs ajustées).
- Visualisations cartographiques et statistiques de la répartition typologique.

**Technologies :**
- R (`sf`, `ggplot2`)
---

### 5. 04PMS_V2_outliers_detection.R
**Objectifs :**
- Détection statistique des valeurs aberrantes dans chaque groupe de données capteurs.
- Application d'une transformation en racine carrée pour stabiliser la variance.
- Calcul des moyennes et écarts-types via une validation croisée.
- Optimisation par maximisation de la log-vraisemblance pour identifier et flagger les outliers.
- Sauvegarde des données annotées après détection des outliers.

**Technologies :**
- R (`dplyr`, `tidyr`, `stats`)
---

### `05PMS_V2_uBss_data_calibration.R`
Calibration via RIVM/uBss :
- Boucle horaire sur les données PMS
- Création de groupes de capteurs autour des stations de référence
- Calcul et interpolation des facteurs de calibration
- Application aux données capteurs
---

### 6. 05PMS_V2_uBss_data_calibration.R
**Objectifs :**
- Calibration des données capteurs sur la base de la méthodologie RIVM et du calcul de uBss.
- Formation de groupes de capteurs autour des stations de référence selon la distance.
- Calcul et interpolation des facteurs de correction horaires.
- Application des corrections aux mesures brutes et sauvegarde des données calibrées.

**Technologies :**
- R (`dplyr`, `interpolation spatiale`)
---

### 7. 05PMS_V2_afterCalibration_timesplot_correlation_SensorReferenceData.R
**Objectifs :**
- Génération de graphiques de séries temporelles et de corrélations après calibration des capteurs.
- Analyse comparative entre les mesures brutes et les mesures corrigées pour évaluer l'efficacité de la calibration.

**Technologies :**
- R (`openair`, `ggplot2`)

---


## Résultats Principaux

- **Données Nettoyées et Classifiées :** Structuration des données et suppression des valeurs aberrantes.
- **Détection d'Outliers :** Utilisation d'une transformation en racine carrée et d'une optimisation par maximisation de la log-vraisemblance.
- **Calibration des Capteurs :** Application des méthodes RIVM et calcul des facteurs de correction via uBss.
- **Visualisations :** Graphiques de séries temporelles et de corrélations avant et après calibration permettant d’évaluer les performances de la calibration.

---

## Instructions d'Utilisation

1. **Exécution des scripts**  
   Chaque script peut être exécuté séquentiellement pour reproduire le pipeline complet de traitement, depuis le nettoyage jusqu’à la calibration des capteurs.

2. **Visualisation des résultats**  
   Les graphiques générés (séries temporelles, corrélations, boxplots, etc.) sont sauvegardés dans le répertoire OUTPUTS. Ces graphiques fournissent une analyse visuelle des données à chaque étape du processus.

3. **Données de Référence**  
   Les données des stations de référence sont intégrées pour calibrer et valider les mesures des capteurs, assurant ainsi la fiabilité des corrections apportées.

---
## Références

- Wesseling et al., 2024 – INERIS Methodology for PMS Calibration
- Guide CEN pour l’évaluation de capteurs dans l’air ambiant
---

Ce README.md offre un aperçu complet du travail réalisé, permettant aux utilisateurs et évaluateurs de comprendre facilement le flux de travail, les outils utilisés, et les résultats obtenus jusqu'à la calibration des capteurs bas-coût pour l’évaluation de la qualité de l’air.
