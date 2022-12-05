
lndens <- function(a =1, b =2, c =3, d =4, e =5, f =6){
  cat('    default parameters: a =1, b =2, c =3, d =4, e = 5, f =6')
lab <- c(rep('Norm 1', 100), rep('Norm 2', 100), rep('Norm 3', 100))
data <- c(rnorm(100, a, b), rnorm(100, c, d), rnorm(100, e, f))
df <- data.frame(A = lab, B = data)

library(ggplot2)
ggplot(df, aes(B, fill = A)) + geom_density(alpha = 0.3) +
  theme_bw(base_rect_size = 2) +
  theme(plot.margin = unit(c(rep(1.4,4)), 'cm'),
        axis.text = element_text(size = 12, face = 'bold'))
}


lngtedens <- function(a =1, b =2, c =3, d =4, e = 1){
  cat('    default parameters: a =1, b =2, c =3, d =4, e = 1')
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

gamma.plot <- function(gshapea =1, gshapeb = 2, gshapec =3, gshaped =4){
  cat('    default parameters: gshapea =1, gshapeb = 2, gshapec =3, gshaped =4')
  plot(dgamma(seq(-1,20,0.001), gshapea), type = 'l',
       xlim = c(-1, 19000))
  lines(dgamma(seq(-1,17,0.001), gshapeb), type = 'l', col = 'red')
  lines(dgamma(seq(-1,17,0.001), gshapec), type = 'l', col = 'blue')
  lines(dgamma(seq(-1,17,0.001), gshaped), type = 'l', col = 'green')
}

cauchy.plot.scale <- function(scalea =1, scaleb =2, scalec =3, scaled =4){
  cat('    default parameters: scalea =1, scaleb =2, scalec =3, scaled =4')

  plot(dcauchy(seq(-9,20,0.001), scale = scalea), type = 'l',
       xlim = c(-300, 19000))
  lines(dcauchy(seq(-9,17,0.001), scale = scaleb), type = 'l', col = 'red')
  lines(dcauchy(seq(-9,17,0.001), scale = scalec), type = 'l', col = 'blue')
  lines(dcauchy(seq(-9,17,0.001), scale = scaled), type = 'l', col = 'green')
}

df.plot <- function(df1a =2, df1b =3, df1c =4, df1d =5, df2 = 1){
  cat('    default parameters: df1a =2, df1b =3, df1c =4, df1d =5, df2 = 1')
  plot(df(seq(-1,20,0.001), df1 = df1a, df2 = df2), type = 'l',
       xlim = c(-10, 6000))
  lines(df(seq(-1,20,0.001), df1 = df1b, df2 = df2), type = 'l', col = 'red')
  lines(df(seq(-1,20,0.001), df1 = df1c, df2 = df2), type = 'l', col = 'blue')
  lines(df(seq(-1,20,0.001), df1 = df1d, df2 = df2), type = 'l', col = 'green')
}

