# ==============================================================================
# SCRIPT : Détection d'outliers par Isolation Forest + Système de Vote Equilibré
# ==============================================================================

# CONFIGURATION ET LIBRAIRIES
rm(list = ls())

setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity")
source("00_paths_and_setting.R")

# Librairies
required_pkgs <- c("raster", "sf", "RColorBrewer", "fields", "ggplot2", "dplyr",
                   "data.table", "chron", "isotree", "tictoc")
invisible(lapply(required_pkgs, require, character.only = TRUE))


# FONCTION PRINCIPALE DE DÉTECTION DES OUTLIERS (ISOLATION FOREST + VOTE)
detect_if_outliers_balanced <- function(data, contamination = 0.05, ntrees = 500,
                                        sample_size = NULL, feature_names = NULL,
                                        return_all_vars = FALSE) {
  # ----------- Sélection & Préparation des Données Numériques --------------
  if (is.data.frame(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (sum(numeric_cols) == 0) stop("Aucune colonne numérique détectée.")
    if (!is.null(feature_names)) {
      feature_names <- intersect(feature_names, colnames(data))
      if (length(feature_names) == 0) stop("Aucune variable valide à utiliser.")
      data_matrix <- as.matrix(data[, feature_names, drop = FALSE])
    } else {
      data_matrix <- as.matrix(data[, numeric_cols, drop = FALSE])
    }
  } else if (is.numeric(data)) {
    data_matrix <- matrix(data, ncol = 1)
  } else if (is.matrix(data)) {
    data_matrix <- data
  } else {
    stop("Type de données non pris en charge pour la détection d'outliers.")
  }
  original_data_matrix <- data_matrix
  
  # ----------- Remplacement NA par la médiane ------------------------------
  if (any(is.na(data_matrix))) {
    for (col in 1:ncol(data_matrix)) {
      na_idx <- is.na(data_matrix[, col])
      if (any(na_idx)) {
        data_matrix[na_idx, col] <- median(data_matrix[!na_idx, col], na.rm = TRUE)
      }
    }
  }
  
  # ----------- Normalisation (Médiane/MAD) -------------------------
  normalized_data <- data_matrix
  for (col in 1:ncol(data_matrix)) {
    col_median <- median(data_matrix[, col], na.rm = TRUE)
    col_mad <- mad(data_matrix[, col], na.rm = TRUE)
    if (is.na(col_mad) || col_mad == 0) col_mad <- sd(data_matrix[, col], na.rm = TRUE)
    if (is.na(col_mad) || col_mad == 0) col_mad <- 1
    normalized_data[, col] <- (data_matrix[, col] - col_median) / col_mad
  }
  
  # ----------- Application de l'Isolation Forest ---------------------------
  if (is.null(sample_size)) sample_size <- min(256, nrow(normalized_data))
  tic("Isolation Forest")
  iso <- isolation.forest(
    normalized_data,
    ntrees = ntrees,
    sample_size = sample_size,
    missing_action = "impute",
    nthreads = max(1, parallel::detectCores() - 1)
  )
  scores <- predict(iso, normalized_data)
  
  # ----------- Système de Votes pour la robustesse ----------------------------
  n_data <- nrow(data_matrix)
  pm25_data <- original_data_matrix[, 1]  # <- variable principale à surveiller
  
  # Statistiques globales
  global_median <- median(pm25_data, na.rm = TRUE)
  global_mad <- mad(pm25_data, na.rm = TRUE)
  global_q95 <- quantile(pm25_data, 0.95, na.rm = TRUE)
  global_q05 <- quantile(pm25_data, 0.05, na.rm = TRUE)
  global_q98 <- quantile(pm25_data, 0.98, na.rm = TRUE)
  global_q02 <- quantile(pm25_data, 0.02, na.rm = TRUE)
  
  # VOTE 1: Isolation Forest
  threshold_if <- quantile(scores, 0.95, na.rm = TRUE)
  vote_if <- scores > threshold_if
  
  # VOTE 2: Global (Q5-Q95)
  vote_global_moderate <- pm25_data > global_q95 | pm25_data < global_q05
  
  # VOTE 3: Global strict (Q2-Q98)
  vote_global_strict <- pm25_data > global_q98 | pm25_data < global_q02
  
  # VOTE 4: Validation temporelle (local z-score)
  window_size <- max(30, min(150, ceiling(n_data / 10)))
  vote_temporal <- rep(FALSE, n_data)
  for (i in 1:n_data) {
    start_idx <- max(1, i - ceiling(window_size / 2))
    end_idx <- min(n_data, i + ceiling(window_size / 2))
    window_data <- pm25_data[start_idx:end_idx]
    local_median <- median(window_data, na.rm = TRUE)
    local_mad <- mad(window_data, na.rm = TRUE)
    if (is.na(local_mad) || local_mad == 0) local_mad <- global_mad
    if (is.na(local_mad) || local_mad == 0) local_mad <- 1
    local_z_score <- abs(pm25_data[i] - local_median) / local_mad
    vote_temporal[i] <- local_z_score > 3.0
  }
  
  # VOTE 5: Validation par densité
  vote_density <- rep(FALSE, n_data)
  data_range <- max(pm25_data, na.rm = TRUE) - min(pm25_data, na.rm = TRUE)
  radius <- data_range * 0.08
  for (i in 1:n_data) {
    neighbors_count <- sum(abs(pm25_data - pm25_data[i]) <= radius, na.rm = TRUE)
    vote_density[i] <- neighbors_count <= 3
  }
  
  # ----------- Agrégation des votes -------------------
  vote_count <- vote_if + vote_global_moderate + vote_global_strict + vote_temporal + vote_density
  extreme_outliers <- (vote_count >= 4) | (vote_global_strict & vote_if)
  moderate_outliers <- (vote_count >= 3) & (vote_if | vote_global_moderate)
  threshold_ultra <- quantile(scores, 0.99, na.rm = TRUE)
  ultra_high_scores <- scores > threshold_ultra
  final_outliers <- extreme_outliers | moderate_outliers | ultra_high_scores
  exec_time <- toc(quiet = TRUE)
  
  # ----------- Résultats / Diagnostics -------------------------------------
  if (return_all_vars && is.data.frame(data)) {
    data$outlier_score <- scores
    data$outliers <- ifelse(final_outliers, "outlier", "Accepted value")
    data$vote_if <- vote_if
    data$vote_global_moderate <- vote_global_moderate
    data$vote_global_strict <- vote_global_strict
    data$vote_temporal <- vote_temporal
    data$vote_density <- vote_density
    data$vote_count <- vote_count
    data$extreme_outliers <- extreme_outliers
    data$moderate_outliers <- moderate_outliers
    data$ultra_high_scores <- ultra_high_scores
    return(data)
  } else {
    results <- list(
      outliers = final_outliers,
      scores = scores,
      threshold = threshold_if,
      model = iso,
      execution_time = exec_time$toc - exec_time$tic,
      vote_if = vote_if,
      vote_global_moderate = vote_global_moderate,
      vote_global_strict = vote_global_strict,
      vote_temporal = vote_temporal,
      vote_density = vote_density,
      vote_count = vote_count,
      extreme_outliers = extreme_outliers,
      moderate_outliers = moderate_outliers,
      ultra_high_scores = ultra_high_scores,
      window_size = window_size
    )
    return(results)
  }
}

# DÉTECTION GROUPE PAR GROUPE
print("Lecture des données capteurs et références...")
load(file_LCS_df_all_clean_groups_Rda)  # => LCS_df_all_clean_groups


nbr_groups <- length(unique(LCS_df_all_clean_groups$Group))
group_names <- unique(LCS_df_all_clean_groups$Group)

dataout <- c()
tic("Total execution")

cat("\n================ ISOLATION FOREST - SYSTÈME DE VOTE ================\n")

for (i in seq_along(group_names)) {
  separator <- paste(rep("-", 60), collapse = "")
  Group <- group_names[i]
  cat(paste0("\n", separator, "\n"))
  cat(sprintf("Groupe %d/%d : %s\n", i, nbr_groups, Group))
  cat(paste0(separator, "\n"))
  df <- LCS_df_all_clean_groups[LCS_df_all_clean_groups$Group == Group, ]
  if (nrow(df) < 2) {
    cat(sprintf("→ Groupe %s ignoré (seulement %d obs)\n\n", Group, nrow(df)))
    next
  }
  
  # Info pour logs
  df_group <- paste(unique(df$Typology), unique(df$Season), unique(df$Clust))
  cat(sprintf("Type: %s\nNombre d'observations: %d\n", df_group, nrow(df)))
  
  # Sélection intelligente des variables
  var_names <- c("PM2.5")
  potential_vars <- c("Temperature", "Humidity", "Pressure")
  var_names <- c(var_names, intersect(potential_vars, colnames(df)))
  cat(sprintf("Variables utilisées: %s\n", paste(var_names, collapse = ", ")))
  
  # Stats & ajustements
  pm25_stats <- summary(df$PM2.5)
  pm25_cv <- sd(df$PM2.5, na.rm = TRUE) / mean(df$PM2.5, na.rm = TRUE)
  base_contamination <- 0.05
  if (pm25_cv > 1.5) {
    base_contamination <- 0.04
  } else if (pm25_cv < 0.5) {
    base_contamination <- 0.06
  }
  if (grepl("TRA", df_group, ignore.case = TRUE)) base_contamination <- base_contamination * 0.85
  if (grepl("INDUS", df_group, ignore.case = TRUE)) base_contamination <- base_contamination * 1.15
  if (grepl("URB", df_group, ignore.case = TRUE)) base_contamination <- base_contamination * 0.95
  if ("Season" %in% colnames(df)) {
    if (grepl("Winter", df$Season[1], ignore.case = TRUE)) base_contamination <- base_contamination * 1.1
    if (grepl("Summer", df$Season[1], ignore.case = TRUE)) base_contamination <- base_contamination * 0.9
  }
  contamination <- min(max(base_contamination, 0.03), 0.08)
  cat(sprintf("CV: %.2f | Médiane: %.2f | Typologie: %s\n", pm25_cv, pm25_stats[3], unique(df$Typology)))
  cat(sprintf("Contamination ajustée: %.2f %%\n", contamination * 100))
  # --- Application de la fonction ---
  if_result <- detect_if_outliers_balanced(
    data = df, feature_names = var_names, contamination = contamination,
    ntrees = 500, sample_size = min(256, nrow(df))
  )
  
  # Mise à jour DataFrame
  df$outlier_score <- if_result$scores
  df$outliers <- ifelse(if_result$outliers, "outlier", "Accepted")
  df$vote_count <- if_result$vote_count
  df$extreme_outliers <- if_result$extreme_outliers
  df$moderate_outliers <- if_result$moderate_outliers
  df$ultra_high_scores <- if_result$ultra_high_scores
  
  # ---- Visualisation ----
  cat("Création du graphique...\n")
  png(filename = file.path(path_figures_outliers, sprintf("_Outliers_IsoForest_%s.png", Group)),
      width = 1200, height = 700, type = "cairo", bg = "white")
  p1 <- ggplot(df, aes(x = datetime, y = PM2.5)) +
    geom_point(aes(color = outliers), size = 1.5, alpha = 0.7) +
    scale_color_manual(values = c("Accepted value" = "black", "outlier" = "red")) +
    labs(title = sprintf("%s: %s - IF ÉQUILIBRÉ (Système de vote)", Group, df_group),
         subtitle = sprintf("Outliers détectés: %d (%.1f%%)", sum(if_result$outliers),
                            100 * sum(if_result$outliers)/length(if_result$outliers)),
         x = "", y = bquote(.(pollutant_name) ~ (mu*g/m^3))) +
    theme_minimal(base_size = 16) +
    theme(legend.title = element_blank(), legend.position = "right")
  print(p1)
  dev.off()
  # ---- Agrégation des résultats ----
  dataout <- rbind(dataout, df)
}

# SAUVEGARDE ET LOG FINAL
exec_time <- toc(quiet = TRUE)
cat("\n=========================================================\n")
cat(sprintf("Temps total d'exécution: %.2f secondes\n", exec_time$toc - exec_time$tic))
cat("=========================================================\n")

cat("Sauvegarde des résultats...\n")
LCS_df_all_clean_groups_outliers2 <- dataout
save(LCS_df_all_clean_groups_outliers2, file = file_LCS_df_all_clean_groups_outliers_Rda2)

cat("Traitement terminé !\n")
