# -----------------------------------------------------------------------------#
# Program to create inputs for SLO-Animated REPO Scripts
#   1. Install packages
#   2. Set color palette
#   3. Create ggplot theme
#   4. Create dataframe 
#   5. Create models
# -----------------------------------------------------------------------------#

# Install packages -------------------------------------------------------------
rm(list=ls())

packages <- c("devtools", "Rcpp", "ggplot2", "gganimate", "gapminder", "dplyr", 
              "installr", "animation", "tweenr", "ggforce", "plotly", "tidyr", 
              "MASS", "bindata", "gifski", "png", "transformr", "grid", "magick",
              "gridExtra", "knitr", "shiny", "RCurl", "magrittr", "png", "readPNG",
              "rstudioapi")
lapply(packages, require, character.only = TRUE)

# Assign colors for graphing subgroups -----------------------------------------
Both <- "#70ad47"
ELL <- "#ed7d31" 
SPED <- "#ffc000"
None <- "#4472c4" 

mycolors <- c( "None" = None, "ELL" = ELL, "SPED" = SPED, "Both" = Both)

yellow <- "#ffc000"
orange <- "#ed7d31"
green <- "#70ad47"
darkblue <- "#4472c4"
lightblue <- "#5b9bd5"

# Create default theme ---------------------------------------------------------    
mytheme <-
  theme(
    panel.background = element_rect(fill = "white"), 
    axis.line = element_line(),
    legend.key=element_blank()
  )

# Create dataframe -------------------------------------------------------------
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
         posttest = ifelse(subgroup=="Both", posttest - 1, 
                           ifelse(subgroup=="ELL", posttest -.25,
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
  
  jordanpre = -2
  tompre = 0
  tinapre = 2
  
  test <- data.frame(id = c(1001:1004), 
                     pretest = c(pre, jordanpre, tompre, tinapre), 
                     subgroup = c("None", "SPED", "ELL", "None"), 
                     ELL = c(0, 1, 0, 0), 
                     SPED = c(0, 0, 1, 0))
  
  test$yhat <- round(predict(mod, newdata = test), digits = 2)
  
  yhat_none <- test$yhat[test$subgroup == "None"]
  yhat_jordan <-test$yhat[test$id==1002] 
  yhat_tom <-test$yhat[test$id==1003] 
  yhat_tina <- test$yhat[test$id==1004] 
  