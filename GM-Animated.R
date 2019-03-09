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

# Explain regression -----------------------------------------------------------

# Fitted values for None and Both
yhat_none <- test$yhat[test$subgroup == "None"]

# Grob for pretest annotation
txt_pretest <- textGrob(paste("Pretest=", pre), 
                        gp = gpar(fontsize = 11, fontface = "bold"))

# Grobs for yhat annotations  
txt_yhat_none <- textGrob(paste(yhat_none), 
                          gp = gpar(fontsize = 10, fontface = "bold"),
                          just = c("left", "center")) 

# Grobs for explanation
xaxis <- textGrob(paste("The horizontal axis contains \nstudents' scores on prior exams"), 
                  gp = gpar(fontsize = 10, fontface = "bold"), just = c("left", "top"))
yaxis <- textGrob(paste("The vertical axis contains \nstudent's expected scores \non their end of year exam."),
                  gp = gpar(fontsize = 10, fontface = "bold"), just = c("left", "top"))
step0 <- textGrob(paste("Each dot represents a student \nwith no particular needs"),
                  gp = gpar(fontsize = 11, fontface = "bold", col = None), just = c("left", "center"))
step1 <- textGrob(paste("This line of best fit represents the \nrelationship between prior achievement and \nend of year score for students \nwith no particular needs"), 
                  gp = gpar(fontsize = 10, fontface = "bold"))
step2 <- textGrob(paste("To get your student's estimated \nend of year score, we begin with \nyour student's prior achievement score. \nLet's say this is 0.5"),
                  gp = gpar(fontsize = 10), just = c("left"))
step3 <- textGrob(paste("which we locate on \nthe line of best fit"),
                  gp = gpar(fontsize = 10), just = c("left", "bottom"))
step4 <- textGrob(paste("to get your student's \nexpected end of year score"),
                  gp = gpar(fontsize = 10), just = c("left"))

# Animate
predanim <- 
  ggplot(train[train$subgroup=="None",], aes(x = pretest, y = posttest)) +
  
  # Explain axis: 1 - 2
  annotation_custom(xaxis, xmin = 1.5, xmax = 1.5, ymin = -3.75, ymax = -3.75) +
  annotation_custom(yaxis, xmin = -4, xmax = -4, ymin = 3.5, ymax = 3.5) +  
  
  # Insert points: 3
  geom_point(color = None) +
  
  # Explain points: 4 - 5
  annotate("rect", xmin = -2.5, xmax = 2.5, ymin = -2, ymax = 2, alpha = .2) + 
  annotation_custom(step0, xmin = 1, xmax = 1, ymin = -1.5, ymax = -1.5) + 
  
  # Insert best fit line: 6 
  geom_abline(intercept = yint.none, slope = beta, color = None, size = 1) +
  
  # Explain best fit line: 7
  annotation_custom(step1, xmin = 2, xmax = 2, ymin = 3, ymax = 3) +
  
  # Explain how we start: 8
  annotation_custom(step2, xmin = .74, xmax = .74, ymin = -3.5, ymax = -3.5) +
  
  # Pretest setup: 10 - 11
  annotation_custom(txt_pretest, xmin = pre, xmax = pre, ymin = min(train$posttest), ymax = min(train$posttest)) +
  geom_point(data = test[test$subgroup == "None", ], aes(x = pretest, y = -Inf), color="black", size = 3) +
  
  # Rise: 12 
  geom_segment(aes(x = pre, xend = pre, y = -Inf, yend = yhat_none), color = "black", linetype = 2, size=.75) +
  
  # Explain rise: 13
  annotation_custom(step3, xmin = pre, xmax = pre, ymin = yhat_none, ymax = yhat_none) +
  
  # Run: 14
  geom_segment(aes(x = pre, xend = -Inf, y = yhat_none, yend = yhat_none), color = "black", linetype = 2, size = .75, arrow=arrow(length=unit(0.4,"cm"))) +
  
  # Explain run: 15
  annotation_custom(step4, xmin = -4, xmax = -4, ymin = yhat_none + .5, ymax = yhat_none + .5 ) + 
  
  # Locate yhat: 16 - 17
  geom_point(data = test[test$subgroup == "None", ], aes(x = -Inf, y = yhat_none), color="black") +
  annotation_custom(txt_yhat_none, xmin = -4, xmax = -4, ymin = yhat_none - .5, ymax = yhat_none - .5 ) + 
  
  # Pause
  geom_point(x=0, y=0, color = "white") + 
  
  # Transitions
  transition_layers(layer_length = 1, 
                    transition_length = 1,
                    from_blank = TRUE, 
                    keep_layers = c(2, 1, Inf, 1, 2, 1, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf
                    )) + 
  
  enter_appear()+
  exit_fade()+
  
  labs(x = "Prior Achievement Score",
       y = "End of Year Score",
       color = "Student Profile") +
  
  lims(x = c(-4, 4),
       y = c(-4, 4)) +
  
  coord_cartesian(clip="off") +
  
  mytheme

animate(predanim)


# Regression for different subgroups ------------------------------------------------------------

# Grobs for explanation
note1 <- textGrob(paste("This line represents the relationship between \nprior achievement and end of year scores for \nstudents with no particular needs"), 
                    gp = gpar(fontsize = 9, fontface = "bold", col = None), just=c("left", "bottom"))
note2 <- textGrob(paste("This line represents the relationship between \nprior achievement and end of year scores for \nELL students"), 
                    gp = gpar(fontsize = 9, fontface = "bold", col = ELL), just=c("left", "bottom"))
note3 <- textGrob(paste("This line represents the relationship between \nprior achievement and end of year scores for \nSPED students"), 
                    gp = gpar(fontsize = 9, fontface = "bold", col = SPED), just=c("left", "bottom"))
note4 <- textGrob(paste("This line represents the relationship between \nprior achievement and end of year scores for \nstudents who are both ELL and SPED"), 
                    gp = gpar(fontsize = 9, fontface = "bold", col = Both), just=c("left", "bottom"))

# Plot
reganim <- 
  
ggplot(train, aes(x = pretest, y = posttest, color = subgroup)) + 
  
  geom_point(data = train[train$subgroup=="None", ], alpha = .75) +
  geom_abline(intercept = yint.none, slope = beta, color = None, size = 1) +
  annotation_custom(note1, xmin = -3.25, xmax = -3.25, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  geom_point(data = train[train$subgroup=="ELL", ], alpha = .75) +
  geom_abline(intercept = yint.ell, slope = beta, color = ELL, size = 1) +
  annotation_custom(note2, xmin = -3.25, xmax = -3.25, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  geom_point(data = train[train$subgroup=="SPED", ], alpha = .75) +
  geom_abline(intercept = yint.sped, slope = beta, color = SPED, size = 1) +
  annotation_custom(note3, xmin = -3.25, xmax = -3.25, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  geom_point(data = train[train$subgroup=="Both", ], alpha = .75) + 
  geom_abline(intercept = yint.both, slope = beta, color = Both, size = 1) +
  annotation_custom(note4, xmin = -3.25, xmax = -3.253, ymin = 2.5, ymax = 2.5) + 
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
