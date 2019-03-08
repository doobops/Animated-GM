# Install packages -------------------------------------------------------------
rm(list=ls())

packages <- c("devtools", "Rcpp", "ggplot2", "gganimate", "gapminder", "dplyr", 
              "installr", "animation", "tweenr", "ggforce", "plotly", "tidyr", 
              "MASS", "bindata", "gifski", "png", "transformr", "grid", "magick",
              "gridExtra", "knitr", "shiny")
lapply(packages, require, character.only = TRUE)

source("D:/GitHub/Animated-SLO/Inputs.R")
source("C:/Users/Christina Kim/Documents/GitHub/Animated-SLO/Inputs.R")

# Assign colors for graphing subgroups -----------------------------------------
Both <- "#ff7373"
ELL <- "#ffad5a" 
SPED <- "#4f9da6"
None <- "#280c64" 

mycolors <- c( "None" = None, "ELL" = ELL, "SPED" = SPED, "Both" = Both)

# Create default theme ---------------------------------------------------------    
mytheme <-
  theme(
    panel.background = element_rect(fill = "white"), 
    axis.line = element_line(),
    legend.key=element_blank()
  )

# Create regression ------------------------------------------------------------

notice1 <- textGrob(paste("This line represents the relationship between \nprior achievement and end of year scores for \nstudents with no particular needs"), 
                    gp = gpar(fontsize = 9, fontface = "bold", col = None), just=c("left", "bottom"))
notice2 <- textGrob(paste("This line represents the relationship between \nprior achievement and end of year scores for \nELL students"), 
                    gp = gpar(fontsize = 9, fontface = "bold", col = ELL), just=c("left", "bottom"))
notice3 <- textGrob(paste("This line represents the relationship between \nprior achievement and end of year scores for \nSPED students"), 
                    gp = gpar(fontsize = 9, fontface = "bold", col = SPED), just=c("left", "bottom"))
notice4 <- textGrob(paste("This line represents the relationship between \nprior achievement and end of year scores for \nstudents who are both ELL and SPED"), 
                    gp = gpar(fontsize = 9, fontface = "bold", col = Both), just=c("left", "bottom"))

reganim <- 
  
ggplot(train, aes(x = pretest, y = posttest, color = subgroup)) + 
  
  geom_point(data = train[train$subgroup=="None", ], alpha = .75) +
  geom_abline(intercept = yint.none, slope = beta, color = None, size = 1) +
  annotation_custom(notice1, xmin = -3.25, xmax = -3.25, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  geom_point(data = train[train$subgroup=="ELL", ], alpha = .75) +
  geom_abline(intercept = yint.ell, slope = beta, color = ELL, size = 1) +
  annotation_custom(notice2, xmin = -3.25, xmax = -3.25, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  geom_point(data = train[train$subgroup=="SPED", ], alpha = .75) +
  geom_abline(intercept = yint.sped, slope = beta, color = SPED, size = 1) +
  annotation_custom(notice3, xmin = -3.25, xmax = -3.25, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  geom_point(data = train[train$subgroup=="Both", ], alpha = .75) + 
  geom_abline(intercept = yint.both, slope = beta, color = Both, size = 1) +
  annotation_custom(notice4, xmin = -3.25, xmax = -3.253, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  scale_colour_manual(breaks = c("None", "ELL", "SPED", "Both"), values = mycolors) + 
  
  labs(x = "Prior Achievement Score",
       y = "End of Year Score",
       color = "Student Profile") + 
  
  mytheme +
  
  transition_layers(layer_length = 5, transition_length = 1,
                    from_blank = TRUE, keep_layers = c(rep(c(2, 1, 1, 1), 4))) +

  enter_fade()+
  exit_fade()


animate(reganim)

# Plot goals -------------------------------------------------------------------

# Fitted values for None and Both
yhat_none <- test$yhat[test$subgroup == "None"]
yhat_both <- test$yhat[test$subgroup == "Both"]

# Grob for pretest annotation
txt_pretest <- textGrob(paste("Pretest=", pre), gp = gpar(fontsize = 10, fontface = "bold"))

# Grobs for goal annotations  
txt_yhat_none <- textGrob(paste("Expected end of \nyear score =", yhat_none), gp = gpar(fontsize = 10, fontface = "bold"))
txt_yhat_both <- textGrob(paste("Expected end of \nyear score =", yhat_both), gp = gpar(fontsize = 10, fontface = "bold"))  

# Animate
predanim <- 
  ggplot(train, aes(x = pretest, y = posttest)) +
  
  geom_point(alpha = .5, aes(color = subgroup)) + 
  
  # Best fit lines
  geom_abline(intercept = yint.none, slope = beta, size = 1, alpha = .75, color = None) +
  geom_abline(intercept = yint.ell, slope = beta, size = 1, alpha = .75, color = ELL) +
  geom_abline(intercept = yint.sped, slope = beta, size = 1, alpha = .75, color = SPED) +
  geom_abline(intercept = yint.both, slope = beta, size = 1, alpha = .75, color = Both) +
  
  # Pretest setup
  annotation_custom(txt_pretest, xmin = pre, xmax = pre, ymin = min(train$posttest), ymax = min(train$posttest)) +
  geom_point(data = test[test$subgroup == "None", ], aes(x = pretest, y = -Inf), color="black") +
  geom_segment(aes(x = pre, xend = pre, y = -Inf, yend = yhat_none), color = "black", linetype = 2, size=.75) + 
  
  # Subgroup None
  geom_segment(aes(x = pre, xend = -Inf, y = yhat_none, yend = yhat_none), color = "black", linetype = 2, size = .75, arrow=arrow(length=unit(0.4,"cm"))) +
  geom_point(data = test[test$subgroup == "None", ], aes(x = -Inf, y = yhat_none), color="black") +
  annotation_custom(txt_yhat_none, xmin = min(train$pretest) + 1, xmax = min(train$pretest) + .5, ymin = yhat_none + .5, ymax = yhat_none + .5 ) + 
  
  # Subgroup Both
  geom_segment(aes(x = pre, xend = -Inf, y = yhat_both, yend = yhat_both), color = "black", linetype = 2, size = .75, arrow=arrow(length=unit(0.4,"cm"))) +
  geom_point(data = test[test$subgroup == "None", ], aes(x = -Inf, y = yhat_both), color="black") +
  annotation_custom(txt_yhat_both, xmin = min(train$pretest) + 1, xmax = min(train$pretest) + .5, ymin = yhat_both + .5, ymax = yhat_both + .5) +    
  
  # Pause
  geom_point(x=0, y=0, color = "white") + 
  
  # Transitions
  transition_layers(layer_length = 1, 
                    transition_length = 1,
                    from_blank = TRUE, 
                    keep_layers = c(4, Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf
                    )) + 
  
  enter_appear()+
  exit_fade()+
  
  # Format
  scale_colour_manual(breaks = c("None", "ELL", "SPED", "Both"), values = mycolors) + 
  
  labs(x = "Prior Achievement Score",
       y = "End of Year Score",
       color = "Student Profile") + 
  
  mytheme

animate(predanim)

