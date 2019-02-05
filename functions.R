min <- function(x, ..., na.rm = TRUE) {
  base::min(x, ..., na.rm = na.rm)
}
median <- function(x, ..., na.rm = TRUE) {
  base::median(x, ..., na.rm = na.rm)
}
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}
sd <- function(x, ..., na.rm = TRUE) {
  stats::sd(x, ..., na.rm = na.rm)
}
max <- function(x, ..., na.rm = TRUE) {
  base::max(x, ..., na.rm = na.rm)
}

st_dev_breaks <- function(x, i, na.rm = TRUE){
  half_st_dev_count <- c(-1 * rev(seq(1, i, by = 2)),
                         seq(1, i, by = 2))
  if((i %% 2) == 1) {
    half_st_dev_breaks <- sapply(half_st_dev_count, function(i) (0.5 * i * sd(x)) + mean(x))
    half_st_dev_breaks[[1]] <- ifelse(min(x) < half_st_dev_breaks[[1]],
                                      min(x), half_st_dev_breaks[[1]])
    half_st_dev_breaks[[i + 1]] <- ifelse(max(x) > half_st_dev_breaks[[i + 1]],
                                          max(x), half_st_dev_breaks[[i + 1]])
  } else {
    half_st_dev_breaks <- NA
  }
  return(half_st_dev_breaks)
}

move_last <- function(df, last_col) {
  match(c(setdiff(names(df), last_col), last_col), names(df))
}

description <- function(i) {
  summarytools::descr(i, na.rm = TRUE, stats = c("min", "med", "mean", "sd", "max"))
}
