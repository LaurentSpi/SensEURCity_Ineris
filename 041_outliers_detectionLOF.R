################################################################################
#  Script R : Détection d'outliers LOF optimisé + densité + filtre global      #
################################################################################

rm(list = ls())
setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity")   # À adapter
source("00_paths_and_setting.R")


# Librairies
required_pkgs <- c("ggplot2", "dplyr", "dbscan", "RANN", "tictoc")
invisible(lapply(required_pkgs, require, character.only = TRUE))

load(file_LCS_df_all_clean_groups_Rda)  # charge : LCS_df_all_clean_groups


# Fonction principale de détection LOF/densité/filtre global
detect_lof_outliers <- function(data,
                                feature_names   = NULL,
                                k_candidates    = c(5,10,15,20),
                                lof_contam      = 0.07,
                                dens_contam     = 0.05,
                                global_quantile = c(0.05, 0.95),
                                verbose         = FALSE) {
  # ------ Vérifications de l'entrée ------
  stopifnot(is.data.frame(data))
  is_num <- sapply(data, is.numeric)
  if (!is.null(feature_names)) {
    is_num <- is_num & names(is_num) %in% feature_names
  }
  # Exclure datetime si présente
  if ("datetime" %in% names(is_num)) is_num["datetime"] <- FALSE
  X <- as.matrix(data[, is_num, drop = FALSE])
  if (ncol(X) < 1) stop("Aucune variable numérique sélectionnée.")
  
  # ------ Imputation des NA par la médiane ------
  for (j in seq_len(ncol(X))) {
    idx_na <- is.na(X[, j])
    if (any(idx_na)) X[idx_na, j] <- median(X[!idx_na, j], na.rm = TRUE)
  }
  
  # ------ Normalisation (médiane/MAD) ------
  Xn <- X
  for (j in seq_len(ncol(Xn))) {
    med  <- median(Xn[, j], na.rm = TRUE)
    madj <- mad(Xn[, j], na.rm = TRUE)
    if (is.na(madj) || madj == 0) madj <- sd(Xn[, j], na.rm = TRUE)
    if (is.na(madj) || madj == 0) madj <- 1
    Xn[, j] <- (Xn[, j] - med) / madj
  }
  
  # ------ Si jeu de données trop petit ------
  if (nrow(Xn) < 3) {
    return(list(
      outliers          = rep(FALSE, nrow(Xn)),
      lof_scores        = rep(NA_real_, nrow(Xn)),
      k_used            = NA_integer_,
      lof_threshold     = NA_real_,
      density_threshold = NA_real_,
      q_low             = NA_real_,
      q_high            = NA_real_
    ))
  }
  
  # ------ Grid-search optimal sur k ------
  valid_ks <- k_candidates[k_candidates >= 2 & k_candidates <= (nrow(Xn) - 1)]
  if (length(valid_ks) == 0) {
    best_k   <- max(2, nrow(Xn) - 1)
    best_var <- Inf
  } else {
    best_var <- Inf
    for (k in valid_ks) {
      scores_k <- lof(Xn, minPts = k)
      thr_k    <- quantile(scores_k, probs = 1 - lof_contam, na.rm = TRUE)
      extremes <- scores_k[scores_k > thr_k]
      v_k      <- if (length(extremes) < 2) Inf else var(extremes, na.rm = TRUE)
      if (!is.na(v_k) && v_k < best_var) {
        best_var <- v_k
        best_k   <- k
      }
    }
    best_k <- max(2, min(best_k, nrow(Xn) - 1))
  }
  if (verbose) message(sprintf("k optimal = %d (var extrêmes = %.4f)", best_k, best_var))
  
  # ------ Calcul des scores LOF et votes ------
  lof_scores <- lof(Xn, minPts = best_k)
  lof_thr    <- quantile(lof_scores, probs = 1 - lof_contam, na.rm = TRUE)
  vote_lof   <- lof_scores > lof_thr
  
  # ------ Vote par densité (k-distance) ------
  nn        <- nn2(Xn, k = best_k + 1)
  kdist     <- nn$nn.dists[, best_k + 1]
  dens_thr  <- quantile(kdist, probs = 1 - dens_contam, na.rm = TRUE)
  vote_dens <- kdist > dens_thr
  
  # ------ Filtre global (quantiles) ------
  if ("PM2.5" %in% colnames(data)) {
    q_lo   <- quantile(data$`PM2.5`, global_quantile[1], na.rm = TRUE)
    q_hi   <- quantile(data$`PM2.5`, global_quantile[2], na.rm = TRUE)
    vote_glob <- (data$`PM2.5` < q_lo) | (data$`PM2.5` > q_hi)
  } else {
    vote_glob <- rep(TRUE, nrow(data))
    q_lo <- q_hi <- NA
  }
  
  # ------ Combinaison finale des votes ------
  final_outliers <- vote_lof & vote_dens & vote_glob
  
  return(list(
    outliers          = final_outliers,
    lof_scores        = lof_scores,
    k_used            = best_k,
    lof_threshold     = lof_thr,
    density_threshold = dens_thr,
    q_low             = q_lo,
    q_high            = q_hi
  ))
}

# Boucle principale groupe par groupe
LCS_df_all_clean_groups <- LCS_df_all_clean_groups

data_out <- vector("list", length = length(unique(LCS_df_all_clean_groups$Group)))
names(data_out) <- unique(LCS_df_all_clean_groups$Group)

global_start <- tictoc::tic()

for (grp in names(data_out)) {
  df_grp <- filter(LCS_df_all_clean_groups, Group == grp)
  features <- c("PM2.5", intersect(c("Temperature", "Humidity", "Pressure"), names(df_grp)))
  
  # Paramètres personnalisables par groupe (si besoin)
  k_vals       <- c(10, 15, 25)
  lof_contam   <- 0.04
  dens_contam  <- 0.04
  quantile_lim <- c(0.15, 0.85)
  
  # ----- Détection -----
  res <- detect_lof_outliers(
    data            = df_grp,
    feature_names   = features,
    k_candidates    = k_vals,
    lof_contam      = lof_contam,
    dens_contam     = dens_contam,
    global_quantile = quantile_lim,
    verbose         = TRUE
  )
  
  message(sprintf("[%s] k=%d | LOF_thr=%.3f | dens_thr=%.3f | Q_low=%.1f | Q_high=%.1f",
                  grp, res$k_used, res$lof_threshold, res$density_threshold, res$q_low, res$q_high))
  
  df_out <- df_grp %>%
    mutate(
      outlier_score = res$lof_scores,
      outliers  = ifelse(res$outliers, "outlier", "accepted")
    )
  
  # ----- Visualisation -----
  out_file <- file.path(path_figures_outliers, sprintf("_Outliers_LOF_%s.png", grp))
  png(filename = out_file, width = 1200, height = 700, type = "cairo", bg = "white")
  print(
    ggplot(df_out, aes(x = datetime, y = `PM2.5`, color = outliers)) +
      geom_point(size = 1.8, alpha = 0.8) +
      scale_color_manual(values = c("accepted" = "black", "outlier" = "red")) +
      labs(
        title    = sprintf("%s : LOF", grp),
        subtitle = sprintf("%d outliers (%.1f%%)", sum(res$outliers),
                           100 * mean(res$outliers))
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title    = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.text     = element_text(size = 12),
        legend.position = "right"
      )
  )
  dev.off()
  data_out[[grp]] <- df_out
}


global_time <- tictoc::toc(global_start, quiet = TRUE)
message("Temps total d'exécution: ", round(global_time$toc - global_time$tic, 2), " sec")

# Sauvegarde des résultats finaux
LCS_df_all_clean_groups_outliers3 <- bind_rows(data_out)
save(LCS_df_all_clean_groups_outliers3, file = file_LCS_df_all_clean_groups_outliers_Rda3)

message("Script terminé, résultats sauvegardés.")
