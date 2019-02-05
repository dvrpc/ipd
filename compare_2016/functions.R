st_dev_breaks <- function(x, i, na.rm = TRUE){
  half_st_dev_count <- c(-1 * rev(seq(1, i, by = 2)),
                         seq(1, i, by = 2))
  if((i %% 2) == 1) {
    half_st_dev_breaks <- unlist(lapply(half_st_dev_count,
                                        function(i) (0.5 * i * sd(x, na.rm = TRUE)) + mean(x, na.rm = TRUE)))
    half_st_dev_breaks[[1]] <- ifelse(min(x, na.rm = TRUE) < half_st_dev_breaks[[1]],
                                      min(x, na.rm = TRUE), half_st_dev_breaks[[1]])
    half_st_dev_breaks[[i + 1]] <- ifelse(max(x, na.rm = TRUE) > half_st_dev_breaks[[i + 1]],
                                          max(x, na.rm = TRUE), half_st_dev_breaks[[i + 1]])
  } else {
    half_st_dev_breaks <- NA
  }
  return(half_st_dev_breaks)
}

move_last <- function(df, last_col) {
  match(c(setdiff(names(df), last_col), last_col), names(df))
}

summary <- function(i){
  output <- list()
  output$min_val <- min(i, na.rm = TRUE)
  output$median_val <- median(i, na.rm = TRUE)
  output$mean_val <- mean(i, na.rm = TRUE)
  output$sd_val <- sd(i, na.rm = TRUE)
  output$max_val <- max(i, na.rm = TRUE)
  return(output)
}
