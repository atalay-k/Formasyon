library(ggplot2)
library(dplyr)

mean.1 <-0
sd.1 <- 1
zstart <- 2
zend <- 3
zcritical <- 1.65

my_col <- "#00998a"


x <- seq(from = mean.1 - 3*sd.1, to = mean.1 + 3*sd.1, by = .01)


MyDF <- data.frame(x = x, y = dnorm(x, mean = mean.1, sd = sd.1))


shade_curve <- function(MyDF, zstart, zend, fill = "red", alpha = .5){
  geom_area(data = subset(MyDF, x >= mean.1 + zstart*sd.1
                          & x < mean.1 + zend*sd.1),
            aes(y=y), fill = fill, color = NA, alpha = alpha)
}
p1a <- ggplot(MyDF, aes(x = x, y = y)) + geom_line() +
  shade_curve(MyDF = MyDF, zstart = -1, zend = 1, fill = my_col, alpha = .3) +
  shade_curve(MyDF = MyDF, zstart = 1, zend = 2, fill = my_col, alpha = .5) +
  shade_curve(MyDF = MyDF, zstart = -2, zend = -1, fill = my_col, alpha = .5) +
  shade_curve(MyDF = MyDF, zstart = 2, zend = 6, fill = my_col, alpha = .7) +
  shade_curve(MyDF = MyDF, zstart = -3, zend = -2, fill = my_col, alpha = .7) +
  scale_x_continuous(breaks = -3:3) +
  scale_y_continuous(breaks = NULL) +
  theme_classic() +
  ylab("") + xlab("")

p1a


MyDF %>%
  mutate(y_cdf = cumsum(y)) -> MyDF


MyDF %>%
  filter(x %in% c(-3, -2.58, -2, -1.65, -1, -.5, 0, .5, 1, 1.65, 2, 2.58, 3)) -> MyDF_filtered
p1a + geom_text(data = MyDF_filtered,
                aes(x = x, y = y + .1, label = paste(round(y_cdf, 0),"%")),
                check_overlap = TRUE) +
  geom_segment(data = MyDF_filtered,
               aes(x = x, xend = x, y = 0, yend = y), linetype = "dashed")
