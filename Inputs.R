# -----------------------------------------------------------------------------#
# Program to create inputs for Student-Learning-Objectives.Rmd
# -----------------------------------------------------------------------------#

# Set up
packages <- c("devtools", "Rcpp", "ggplot2", "gganimate", "gapminder", "dplyr", 
              "installr", "animation", "tweenr", "ggforce", "plotly", "tidyr", 
              "MASS", "bindata", "gifski", "png", "transformr", "grid", "magick",
              "gridExtra", "knitr", "shiny")
lapply(packages, require, character.only = TRUE)

# Create dataframe
n <- 1000
rho.1 <- 0.75
rho.2 <- .3


train <- mvrnorm(n = n, mu = c(0, 0), Sigma = matrix(c(1, rho.1, rho.1, 1), nrow = 2), empirical = TRUE) %>% data.frame() %>%
  bind_cols(rmvbin(n = n, margprob = c(.5, .5), bincorr = matrix(c(1, rho.2,rho.2, 1), ncol = 2)) %>% data.frame()) %>%
  dplyr::rename(pretest = X1, posttest= X2, ELL = X11, SPED = X21) %>%
  mutate(subgroup = factor(ifelse(SPED == 1 & ELL == 1, "Both", 
                                  ifelse(ELL == 1, "ELL", 
                                         ifelse(SPED == 1, "SPED", "None"))),
                           levels = c("None", "ELL", "SPED", "Both")),
         posttest = ifelse(subgroup=="Both", posttest - .8, 
                           ifelse(subgroup=="ELL", posttest -.4,
                                  ifelse(subgroup=="SPED", posttest - .6, posttest + .35)))) 

cormatrix <- 
cor(train[, sapply(train, is.numeric)],
    use = "complete.obs", method = "pearson")

# Train model
  mod <- lm(posttest ~ pretest + ELL + SPED, train)
  
  yint.ell <- mod$coefficients[1] + mod$coefficients[3]
  yint.sped <- mod$coefficients[1] + mod$coefficients[4]
  yint.none <- mod$coefficients[1]
  yint.both <- mod$coefficients[1] + mod$coefficients[3] + mod$coefficients[4]
  
  beta = mod$coefficients[2]

# Test model
  pre = 0.5
  test <- data.frame(id = c(1001:1004), 
                     pretest = rep(pre, times = 4), 
                     subgroup = c("None", "ELL", "SPED", "Both"), 
                     ELL = c(0, 1, 0, 1), 
                     SPED = c(0, 0, 1, 1))
  
  test$yhat <- round(predict(mod, newdata = test), digits = 2)