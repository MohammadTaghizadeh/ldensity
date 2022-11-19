
lndens <- function(a, b, c, d, e, f){
lab <- c(rep('Norm 1', 100), rep('Norm 2', 100), rep('Norm 3', 100))
data <- c(rnorm(100, a, b), rnorm(100, c, d), rnorm(100, e, f))
df <- data.frame(A = lab, B = data)

library(ggplot2)
ggplot(df, aes(B, fill = A)) + geom_density(alpha = 0.3) +
  theme_bw(base_rect_size = 2) +
  theme(plot.margin = unit(c(rep(1.4,4)), 'cm'),
        axis.text = element_text(size = 12, face = 'bold'))
}


lngtedens <- function(a, b, c, d, e = 1){
  lab <- c(rep('Norm', 100), rep('Gamma', 100), rep('Tstudent', 100),
           rep('Expo', 100))
  data <- c(rnorm(100, a, b), rgamma(100, c), rt(100, d), rexp(100, e))
  df <- data.frame(A = lab, B = data)

  library(ggplot2)
  ggplot(df, aes(B, fill = A)) + geom_density(alpha = 0.4) +
    theme_bw(base_rect_size = 2) +
    theme(plot.margin = unit(c(rep(1.4,4)), 'cm'),
          axis.text = element_text(size = 12, face = 'bold'))

}


