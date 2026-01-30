# geocontext_R.R
# ------------------------------------------------------------
# Geographical context based on population-weighted k-neighbours
# R implementation inspired by:
# Hennerdal & Nielsen (2017)
# Original Python version by Pontus Hennerdal (2022)
# ------------------------------------------------------------

library(dplyr)
library(sf)

geocontext_R <- function(points,
                         pop_locations,
                         groups,
                         populations,
                         proportions,
                         k_values,
                         points_x = "x",
                         points_y = "y",
                         pop_x = "x",
                         pop_y = "y") {
  
  # -----------------------------
  # Input validation
  # -----------------------------
  stopifnot(is.data.frame(points))
  stopifnot(is.data.frame(pop_locations))
  stopifnot(is.character(groups))
  stopifnot(is.character(populations))
  stopifnot(is.logical(proportions))
  stopifnot(is.numeric(k_values))
  
  if (length(groups) != length(proportions)) {
    stop("groups and proportions must have the same length")
  }
  
  if (!(length(populations) == 1 || length(populations) == length(groups))) {
    stop("populations must be length 1 or same length as groups")
  }
  
  k_values <- sort(k_values)
  
  multi_pop <- length(populations) > 1
  
  # -----------------------------
  # Convert to sf (points only)
  # -----------------------------
  points_sf <- st_as_sf(
    points,
    coords = c(points_x, points_y),
    crs = 4326,
    remove = FALSE
  )
  
  pop_sf <- st_as_sf(
    pop_locations,
    coords = c(pop_x, pop_y),
    crs = 4326,
    remove = FALSE
  )
  
  # Preallocate result list
  results <- vector("list", nrow(points_sf))
  
  # -----------------------------
  # Main loop over points
  # -----------------------------
  for (i in seq_len(nrow(points_sf))) {
    
    pt <- points_sf[i, ]
    
    # Distances (numeric vector, meters if projected / degrees if lonlat)
    dists <- as.numeric(st_distance(pt, pop_sf))
    
    pop_tmp <- pop_locations
    pop_tmp$distance <- dists
    
    pop_tmp <- pop_tmp |> arrange(distance)
    
    res <- list(point_index = i)
    
    # Loop over population columns
    for (g_idx in seq_along(groups)) {
      
      pop_col <- if (multi_pop) populations[g_idx] else populations[1]
      group_col <- groups[g_idx]
      is_prop <- proportions[g_idx]
      
      cum_pop <- cumsum(pop_tmp[[pop_col]])
      
      # Find radius for each k
      radius_vec <- sapply(k_values, function(k) {
        idx <- which(cum_pop >= k)[1]
        pop_tmp$distance[idx]
      })
      
      # Store radii
      for (j in seq_along(k_values)) {
        nm <- if (multi_pop) {
          paste0("radius_", group_col, "_k", k_values[j])
        } else {
          paste0("radius_k", k_values[j])
        }
        res[[nm]] <- radius_vec[j]
      }
      
      # Calculate stats inside each radius
      for (j in seq_along(k_values)) {
        
        sel <- pop_tmp |> filter(distance <= radius_vec[j])
        
        total_pop <- sum(sel[[pop_col]])
        
        if (is_prop) {
          res[[paste0("group_", group_col, "_k", k_values[j])]] <-
            sum(sel[[group_col]])
        } else {
          w_mean <- weighted.mean(sel[[group_col]], sel[[pop_col]])
          w_var  <- weighted.mean((sel[[group_col]] - w_mean)^2, sel[[pop_col]])
          
          res[[paste0("mean_", group_col, "_k", k_values[j])]] <- w_mean
          res[[paste0("sd_",   group_col, "_k", k_values[j])]] <- sqrt(w_var)
        }
        
        total_nm <- if (multi_pop) {
          paste0("total_", group_col, "_k", k_values[j])
        } else {
          paste0("total_k", k_values[j])
        }
        
        res[[total_nm]] <- total_pop
      }
    }
    
    results[[i]] <- res
  }
  
  # -----------------------------
  # Bind results to points
  # -----------------------------
  res_df <- bind_rows(results)
  
  out <- bind_cols(points, res_df |> select(-point_index))
  
  # -----------------------------
  # Calculate proportions
  # -----------------------------
  for (j in seq_along(k_values)) {
    for (g_idx in seq_along(groups)) {
      
      if (!proportions[g_idx]) next
      
      group <- groups[g_idx]
      k     <- k_values[j]
      
      num <- paste0("group_", group, "_k", k)
      
      den <- if (multi_pop) {
        paste0("total_", group, "_k", k)
      } else {
        paste0("total_k", k)
      }
      
      out[[paste0("mean_", group, "_k", k)]] <- out[[num]] / out[[den]]
    }
  }
  
  out
}
