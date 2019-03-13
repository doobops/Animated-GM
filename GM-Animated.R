# Read in Inputs.R--------------------------------------------------------------

# From GitHub 
require("RCurl")

inputscript <- 
  getURL("https://raw.githubusercontent.com/doobops/Animated-GM/master/Inputs.R", ssl.verifypeer = FALSE)

eval(parse(text = inputscript))

# Explain regression -----------------------------------------------------------

# Grobs for yhat annotations  
txt_yhat_none <- textGrob(paste(yhat_none), 
                          gp = gpar(fontsize = 12, fontface = "bold", col = orange),
                          just = c("left", "center")) 

# Grobs for explanation
xaxis <- textGrob(paste("The horizontal axis contains \nstudents' scores on prior exams"), 
                  gp = gpar(fontsize = 10, fontface = "bold"), just = c("left", "top"))
yaxis <- textGrob(paste("The vertical axis contains \nstudent's expected scores \non their end of year exam."),
                  gp = gpar(fontsize = 10, fontface = "bold"), just = c("left", "top"))
step0 <- textGrob(paste("Each dot represents \na student with \nno particular needs"),
                  gp = gpar(fontsize = 10, fontface = "bold", col = None), just = c("center", "center"))
step1 <- textGrob(paste("This line of best fit \nrepresents the \nrelationship between \nprior achievement \nand end of year score \nfor students \nwith no particular \nneeds"), 
                  gp = gpar(fontsize = 10, fontface = "bold", col = None), just = c("left"))
step2 <- textGrob(paste("To get your student's estimated \nend of year score, we begin with \nyour student's prior achievement score. \nLet's say this is 0.5"),
                  gp = gpar(fontsize = 10, fontface = "bold"), just = c("left"))
step3 <- textGrob(paste("which we locate on \nthe line of best fit"),
                  gp = gpar(fontsize = 10, fontface = "bold"), just = c("left", "bottom"))
step4 <- textGrob(paste("to get your student's \nexpected end of year score"),
                  gp = gpar(fontsize = 10, fontface = "bold"), just = c("left"))

# Animate
predanim <- 
  ggplot(train[train$subgroup=="None",], aes(x = pretest, y = posttest)) +
  
  # Explain axis: 1 - 2
  annotation_custom(xaxis, xmin = 1.5, xmax = 1.5, ymin = -3.75, ymax = -3.75) +
  annotation_custom(yaxis, xmin = -4, xmax = -4, ymin = 3.5, ymax = 3.5) +  
  
  # Insert points: 3
  geom_point(color = None) +
  
  # Explain points: 4 - 5
  annotate("rect", xmin = -2.5, xmax = 2.5, ymin = -2, ymax = 2, alpha = .2, fill = yellow) + 
  annotation_custom(step0, xmin = -2.5, xmax = -2.5, ymin = 2, ymax = 2) + 
  
  # Insert best fit line: 6 
  geom_abline(intercept = yint.none, slope = beta, color = None, size = 1) +
  
  # Explain best fit line: 7
  annotation_custom(step1, xmin = 3, xmax = 3, ymin = 0, ymax = 0) +
  
  # Pause: 8
  geom_point(x=0, y=0, color = "white") +
  
  # Explain how we start: 9
  annotation_custom(step2, xmin = .74, xmax = .74, ymin = -3.5, ymax = -3.5) +
  
  # Pretest setup: 10 
  geom_point(data = test[test$subgroup == "None", ], aes(x = pretest, y = -Inf), color = orange, size = 3) +
  
  # Rise: 12 
  geom_segment(aes(x = pre, xend = pre, y = -Inf, yend = yhat_none), color = orange, linetype = 2, size=.75) +
  
  # Explain rise: 13
  annotation_custom(step3, xmin = .74, xmax = .74, ymin = -2.5, ymax = -2.5) +
  
  # Run: 14
  geom_segment(aes(x = pre, xend = -Inf, y = yhat_none, yend = yhat_none), color = orange, linetype = 2, size = .75, arrow=arrow(length=unit(0.4,"cm"))) +
  
  # Explain run: 15
  annotation_custom(step4, xmin = -4, xmax = -4, ymin = yhat_none + .5, ymax = yhat_none + .5 ) + 
  
  # Locate yhat: 16 - 17
  geom_point(data = test[test$subgroup == "None", ], aes(x = -Inf, y = yhat_none), color = orange, size = 3) +
  annotation_custom(txt_yhat_none, xmin = -4, xmax = -4, ymin = yhat_none - .5, ymax = yhat_none - .5 ) + 
  
  # Pause: 18 - 19
  geom_point(x=0, y=0, color = "white") + 
  geom_point(x=0, y=0, color = "white") + 
  
  # Transitions
  transition_layers(layer_length = 5, 
                    transition_length = 1,
                    from_blank = TRUE, 
                    keep_layers = c(2, 1, Inf, 2, 3, Inf, 1, 2, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf
                    )) + 
  
  enter_appear()+
  exit_fade()+
  
  labs(x = "Prior Achievement Score",
       y = "End of Year Score",
       color = "Student Profile") +
  
  lims(x = c(-4.5, 4.5),
       y = c(-4, 4)) +
  
  coord_cartesian(clip="off") +
  
  mytheme

animate(predanim, fps = 5)
anim_save(filename="how_to_regress.gif")


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
reg_bysub <- 
  
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
                    # from_blank = TRUE, keep_layers = c(rep(c(2, 1, 1, 1), 4))) +
                    from_blank = TRUE, keep_layers = c(rep(c(3, 2, 1, 1), 4))) +

  enter_fade()+
  exit_fade()

animate(reg_bysub, fps = 5)
anim_save(filename="intercept_shift.gif")

# Tina, Tom and Jordan ---------------------------------------------------------

# Image Grobs
jordanGrob <- rasterGrob(jordanPNG, interpolate = TRUE)
tomGrob <- rasterGrob(tomPNG, interpolate = TRUE)
tinaGrob <- rasterGrob(tinaPNG, interpolate = TRUE)

# Annotation Grobs
yhat_jordan_grob <- textGrob(paste(yhat_jordan), 
                          gp = gpar(fontsize = 12, fontface = "bold", col = SPED))
yhat_tom_grob <- textGrob(paste(yhat_tom), 
                           gp = gpar(fontsize = 12, fontface = "bold", col = ELL))
yhat_tina_grob <- textGrob(paste(yhat_tina), 
                           gp = gpar(fontsize = 12, fontface = "bold", col = None))

# Annotation for YAXIS
yaxisGrob <- textGrob(paste("Estimated \nEnd of \nYear Score"), 
                      gp = gpar(fontface = "bold", col = "black"), just = c("left", "center")) 

tina_tom_jordan <- 
ggplot(train, aes(x = pretest, y = posttest, color = subgroup)) + 
  
  annotation_custom(yaxisGrob, xmin = -3.5, xmax = -3.5, ymin = yhat_tina+1, ymax = yhat_tina+1) +
  
  geom_point(alpha = 0.25) +
  
  geom_abline(intercept = yint.none, slope = beta, color = None, size = 1) +
  geom_abline(intercept = yint.ell, slope = beta, color = ELL, size = 1) +
  geom_abline(intercept = yint.sped, slope = beta, color = SPED, size = 1) +
  geom_abline(intercept = yint.both, slope = beta, color = Both, size = 1) +
  
  annotation_custom(jordanGrob, xmin = 1.625, xmax = 2.375, ymin = -4, ymax = -3.25) +
  annotation_custom(tomGrob, xmin = -1.625, xmax = -2.375, ymin = -4, ymax = -3.25) +
  annotation_custom(tinaGrob, xmin = 0.375, xmax = -0.375, ymin = -4, ymax = -3.25) +
  
  annotate("segment", x = jordanpre, xend = jordanpre, y = -3.25 , yend = yhat_jordan, color = SPED, 
           size = 1, linetype = 2) + 
  annotate("segment", x = jordanpre, xend = -3, y = yhat_jordan, yend = yhat_jordan, color = SPED, 
           size = 1, linetype = 2, arrow=arrow(length=unit(0.4,"cm"))) +
  annotation_custom(yhat_jordan_grob, xmin = -3.25, xmax = -3.25, ymin = yhat_jordan, ymax = yhat_jordan+.3) +
  
  annotate("segment", x = tompre, xend = tompre, y = -3.25 , yend = yhat_tom, color = ELL, 
           size = 1, linetype = 2) + 
  annotate("segment", x = tompre, xend = -3, y = yhat_tom, yend = yhat_tom, color = ELL, 
           size = 1, linetype = 2, arrow=arrow(length=unit(0.4,"cm"))) +
  annotation_custom(yhat_tom_grob, xmin = -3.25, xmax = -3.25, ymin = yhat_tom, ymax = yhat_tom+.3) +
  
  annotate("segment", x = tinapre, xend = tinapre, y = -3.25 , yend = yhat_tina, color = None, 
           size = 1, linetype = 2) +  
  annotate("segment", x = tinapre, xend = -3, y = yhat_tina, yend = yhat_tina, color = None, 
           size = 1, linetype = 2, arrow=arrow(length=unit(0.4,"cm"))) +
  annotation_custom(yhat_tina_grob, xmin = -3.25, xmax = -3.25, ymin = yhat_tina, ymax = yhat_tina+.3) +
  
  coord_cartesian(clip="off") +
  
  scale_colour_manual(breaks = c("None", "ELL", "SPED", "Both"), values = mycolors) + 
  
  labs(x = "Prior Achievement Score",
       y = " ",
       color = "Student Profile") + 
  
  lims(x = c(-3, 3),
       y = c(-4, 4)) +
  
  mytheme +
  
  theme(axis.text.y = element_blank(),
        axis.line = element_line(color = "grey77")) + 
  
  transition_layers(layer_length = 5, transition_length = 1, from_blank = FALSE) + 
  
  enter_fade()+
  exit_fade()

animate(tina_tom_jordan, fps = 1)
anim_save(filename="tina_tom_jordan.gif")