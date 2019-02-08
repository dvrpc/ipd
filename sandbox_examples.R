# empirical cumulative distribution function
TEST <- c(1,2,3,4,5,6,7,8,9,9,9,9,9,9,9,10)
rank1 <- TEST / max(TEST) * 100
rank2 <- ecdf(TEST)(TEST) * 100
plot(TEST, rank1, main = "rank1")
plot(TEST, rank2, main = "rank2")
# class breaks
my_breaks <- c(-100, 5, 7, 100)
TEST <- seq(0, 10, by = 1)
cut(TEST, labels = FALSE, breaks = my_breaks, include.lowest = TRUE, right = FALSE) - 1
