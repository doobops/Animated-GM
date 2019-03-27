# Read in Inputs.R--------------------------------------------------------------

# From GitHub 
  require("RCurl")
  
  inputscript <- 
    getURL("https://raw.githubusercontent.com/doobops/Animated-GM/master/Inputs.R", ssl.verifypeer = FALSE)
  
  eval(parse(text = inputscript))

# Connect to Repo
  current_path <- gsub(x = paste0(getSourceEditorContext()$path), pattern = "/GM-Animated.R", "")
  setwd(file.path(current_path))
  getwd()

# Read in images ---------------------------------------------------------------  
  
# Import 
  jordanPNG <- readPNG("studentjordan_64bit.png")
  tomPNG <- readPNG("studenttom_64bit.png")
  tinaPNG <- readPNG("studenttina_64bit.png")
  peerboy_PNG <- readPNG("peer_boy_64bit.png")
  peergirl_PNG <- readPNG("peer_girl_64bit.png")
  jillPNG <- readPNG("studentjill_64bit.png")
  
  peerexample_PNG <- readPNG("peer_example_64bit.png")
 
  bracketPNG <- readPNG("Bracket.png")
  bracket2PNG <- readPNG("Bracket2.png")
  
# Create Grobs
  jordanGrob <- rasterGrob(jordanPNG, interpolate = TRUE)
  tomGrob <- rasterGrob(tomPNG, interpolate = TRUE)
  tinaGrob <- rasterGrob(tinaPNG, interpolate = TRUE)
  peerboyGrob <- rasterGrob(peerboy_PNG, interpolate = TRUE) 
  peergirlGrob <- rasterGrob(peergirl_PNG, interpolate = TRUE) 
  jillGrob <- rasterGrob(jillPNG, interpolate = TRUE) 
  
  peerexampleGrob <- rasterGrob(peerexample_PNG, interpolate = TRUE) 

  bracketGrob <- rasterGrob(bracketPNG, interpolate = TRUE)
  bracket2Grob <- rasterGrob(bracket2PNG, interpolate = TRUE)

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
  annotation_custom(note1, xmin = -3, xmax = -3, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  geom_point(data = train[train$subgroup=="ELL", ], alpha = .75) +
  geom_abline(intercept = yint.ell, slope = beta, color = ELL, size = 1) +
  annotation_custom(note2, xmin = -3, xmax = -3, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  geom_point(data = train[train$subgroup=="SPED", ], alpha = .75) +
  geom_abline(intercept = yint.sped, slope = beta, color = SPED, size = 1) +
  annotation_custom(note3, xmin = -3, xmax = -3, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  geom_point(data = train[train$subgroup=="Both", ], alpha = .75) + 
  geom_abline(intercept = yint.both, slope = beta, color = Both, size = 1) +
  annotation_custom(note4, xmin = -3, xmax = -3, ymin = 2.5, ymax = 2.5) + 
  geom_point(x=0, y=0, color = "white") +
  
  scale_colour_manual(breaks = c("None", "ELL", "SPED", "Both"), values = mycolors) + 
  
  labs(x = "Prior Achievement Score",
       y = "End of Year Score",
       color = "Student Profile") + 
  
  mytheme +
  
  transition_layers(layer_length = 2, transition_length = .5,
                    # from_blank = TRUE, keep_layers = c(rep(c(2, 1, 1, 1), 4))) +
                    from_blank = TRUE, keep_layers = c(rep(c(3, 2, 1, 1), 4))) +

  enter_fade()+
  exit_fade()

animate(reg_bysub, fps = 5)
anim_save(filename="intercept_shift.gif")

# Tina, Tom and Jordan ---------------------------------------------------------

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
  
  annotation_custom(jordanGrob, xmin = -1.625, xmax = -2.375, ymin = -4, ymax = -3.25) +
  annotation_custom(tomGrob, xmin = 0.375, xmax = -0.375, ymin = -4, ymax = -3.25) +
  annotation_custom(tinaGrob, xmin = 1.625, xmax = 2.375, ymin = -4, ymax = -3.25) +
  
  annotate("segment", x = jordanpre, xend = jordanpre, y = -3.25 , yend = yhat_jordan, color = SPED, 
           size = 1, linetype = 2) + 
  annotate("segment", x = jordanpre, xend = -3, y = yhat_jordan, yend = yhat_jordan, color = SPED, 
           size = 1, linetype = 2, arrow=arrow(length=unit(0.4,"cm"))) +
  annotation_custom(yhat_jordan_grob, xmin = -3.25, xmax = -3.25, ymin = yhat_jordan, ymax = yhat_jordan +.3) +
  
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
  
  transition_layers(layer_length = 1.5, transition_length = 1, from_blank = FALSE) + 
  
  enter_fade()+
  exit_fade()

animate(tina_tom_jordan)
anim_save(filename="tina_tom_jordan.gif")

# Peer group flow chart ----------------------------------------------------------------

# Name tags
iam_tina_grob <- textGrob("Tina", gp = gpar(fontsize = 11, fontface = "bold", col = orange))
tina_peer_grob <- textGrob(paste0("Tina's \nPeer Group"), gp = gpar(fontsize = 11, fontface = "bold", col = "black"), just = c("right", "center"))
iam_jill_grob <- textGrob("Jill", gp = gpar(fontsize = 11, fontface = "bold", col = green))
jill_peer_grob <- textGrob(paste0("Jill's \nPeer Group"), gp = gpar(fontsize = 11, fontface = "bold", col = "black"), just = c("right", "center"))

peergroup <- 
ggplot(data=train, aes(x = 0, y = 0)) +
  geom_point(color = "white", fill = "white") +
  
  # I am Tina
  annotation_custom(iam_tina_grob, xmin = -6, xmax = 6, ymin = 41, ymax = 41) + 
  
  # Tina
  annotation_custom(tinaGrob, xmin = -6, xmax = 6, ymin = 32.5, ymax = 42.5) +
  
  # Arrow
  annotate("segment", x = 0, xend = 0, y = 35, yend = 28, color = orange, 
           size = 1,  arrow=arrow(length=unit(0.25,"cm"))) + 
  
  # Tina's peers
  annotation_custom(peerboyGrob, xmin = -95, xmax = -85, ymin = 20, ymax = 30) +
  annotation_custom(peergirlGrob, xmin = -80, xmax = -70, ymin = 20, ymax = 30) +
  annotation_custom(peerboyGrob, xmin = -65, xmax = -55, ymin = 20, ymax = 30) +
  annotation_custom(jillGrob, xmin = -50, xmax = -40, ymin = 20, ymax = 30) +
  annotation_custom(peerboyGrob, xmin = -35, xmax = -25, ymin = 20, ymax = 30) + 
  annotation_custom(peergirlGrob, xmin = -20, xmax = -10, ymin = 20, ymax = 30) +
  annotation_custom(peerboyGrob, xmin = -5, xmax = 5, ymin = 20, ymax = 30) + 
  annotation_custom(peergirlGrob, xmin = 10, xmax = 20, ymin = 20, ymax = 30) +
  annotation_custom(peerboyGrob, xmin = 25, xmax = 35, ymin = 20, ymax = 30) + 
  annotation_custom(peergirlGrob, xmin = 40, xmax = 50, ymin = 20, ymax = 30) +
  
  # Group
  annotate("rect", xmin = -97, xmax = 52, ymin = 20, ymax = 30, alpha = 0, color = orange, fill = "white", linetype = "dashed") +
  annotation_custom(tina_peer_grob, xmin = -98, xmax = -98, ymin = 25, ymax = 25) + 
  
  # Pause
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +

  # I am Jill
  annotation_custom(iam_jill_grob, xmin = -50, xmax = -40, ymin = 28.5, ymax = 28.5) + 
  
  # Arrow
  annotate("segment", x = -45, xend = -45, y = 22, yend = 13, color = green, 
           size = 1,  arrow=arrow(length=unit(0.25,"cm"))) + 
  
  # Jill's peers
  annotation_custom(peerboyGrob, xmin = -125, xmax = -115, ymin = 5, ymax = 15) +
  annotation_custom(peergirlGrob, xmin = -110, xmax = -100, ymin = 5, ymax = 15) +
  annotation_custom(peerboyGrob, xmin = -95, xmax = -85, ymin = 5, ymax = 15) +
  annotation_custom(peergirlGrob, xmin = -80, xmax = -70, ymin = 5, ymax = 15) +
  annotation_custom(peerboyGrob, xmin = -65, xmax = -55, ymin = 5, ymax = 15) +
  annotation_custom(peergirlGrob, xmin = -50, xmax = -40, ymin = 5, ymax = 15) +
  annotation_custom(peerboyGrob, xmin = -35, xmax = -25, ymin = 5, ymax = 15) + 
  annotation_custom(peergirlGrob, xmin = -20, xmax = -10, ymin = 5, ymax = 15) +
  annotation_custom(peerboyGrob, xmin = -5, xmax = 5, ymin = 5, ymax = 15) + 
  annotation_custom(peergirlGrob, xmin = 10, xmax = 20, ymin = 5, ymax = 15) + 
  
  # Group
  annotate("rect", xmin = -127, xmax = 22, ymin = 5, ymax = 15, alpha = 0, color = green, fill = "white", linetype = "dashed") + 
  annotation_custom(jill_peer_grob, xmin = -128, xmax = -128, ymin = 10, ymax = 10) + 
  
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +

  lims(x = c(-155, 55),
       y = c(-10, 50)) + 
  
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  
  transition_layers(layer_length = 1, transition_length = 1, from_blank = FALSE) + 
  
  enter_fade()+
  exit_fade()
  
animate(peergroup, fps = 3.5) 
anim_save(filename="peergroup.gif")


# Create Peer Group ------------------------------------------------------------

# Data frame
make.peers <- data.frame(a = seq(from = -2, to = 18, by = .5), 
                         b = seq(from = 0, to = 20, by = .5),
                         c = seq(from = 2, to = 22, by = .5))

# Grobs for annotations
grobexp <- textGrob("Expected \nEnd of Year Score", gp = gpar(fontsize = 11, fontface = "bold"))

grob50above <- textGrob("50 Students \nAbove", gp = gpar(fontsize = 10, fontface = "bold"), just = c("left", "center"))
grob50below <- textGrob("50 Students \nBelow", gp = gpar(fontsize = 10, fontface = "bold"), just = c("left", "center"))

grob90 <- textGrob("90", gp = gpar(fontsize = 10))
grob88.8 <- textGrob("88.8", gp = gpar(fontsize = 10))
grob90.2 <- textGrob("90.2", gp = gpar(fontsize = 10))

grobTinasPeers <- textGrob("Tina's peer group, \ncentered around \n90", 
                           gp = gpar(fontsize = 10,
                                     fontface = "bold",
                                     just = c("right", "center")))

# Plot
peerTina <- 
ggplot(data = make.peers, aes(x = 0, y = 0)) + 

  # Set up background
  geom_vline(xintercept = 0, color = "#a5a5a5") + 
  annotation_custom(grobexp, xmin = 0, xmax = 0, ymin = 24, ymax = 24) + 
  
  # Set up Tina
  annotation_custom(tinaGrob, xmin = -2.25, xmax = 0, ymin = 10.5, ymax = 12.75) + 
  geom_point(data = make.peers[20, ], aes(x = 0, y = c), size = 5, color = None) +
  annotation_custom(grobTinasPeers, xmin = -5, xmax = -2.25, ymin = 10.5, ymax = 12.75) + 

  # Set up surrounding peers
  geom_point(data = make.peers[c(0:19), ], aes(x = 0, y = c), size = 4, alpha = .5, colour = None) +
  geom_point(data = make.peers[c(20:41), ], aes(x = 0, y = c), size = 4, alpha = .5, colour = None) +
  annotation_custom(grob90.2, xmin = 0, xmax = 2, ymin = 22, ymax = 22) + 
  annotation_custom(grob88.8, xmin = 0, xmax = 2, ymin = 2, ymax = 2) + 
  
  # Explain
  annotation_custom(bracketGrob, xmin = 1, xmax = 1.5, ymin = 11, ymax = 23) + 
  annotation_custom(grob50above, xmin = 2, xmax = 2, ymin = 11, ymax = 23) + 
  annotation_custom(bracketGrob, xmin = 1, xmax = 1.5, ymin = 2, ymax = 10) + 
  annotation_custom(grob50below, xmin = 2, xmax = 2, ymin = 10, ymax = 2) + 

  # Pause
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  
  lims(x = c(-7, 7), 
       y = c(-3, 25)) +
  
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  
  transition_layers(transition_length = 1,
                    layer_length = 1.1)

animate(peerTina)

anim_save(filename="peerTina.gif")  


  
  

# SGP I ------------------------------------------------------------------------

peering <- data.frame(x = rep(c(-.5, .5), each = 11),
                
                y = c(seq(from = 0, to = 10, by = 1), 
                      sample(c(-1, 0, 1, 2, 3, 5, 7, 8, 9, 10, 11), size=11)))

peering$image <- ifelse(peering$x == -.5 & peering$y == 5 | peering$x == .5 & peering$y == 7, 
                        "studenttina_64bit.png",
                        "peer_boy_64bit.png")

sgp <- 
ggplot(peering, aes(x, y)) +
  geom_image(aes(image=image), size=.065) + 
  lims(y = c(-2, 15)) +
  
  scale_x_continuous(limits = c(-1, 1), 
                     breaks = c(-.5, .5), 
                     labels = c(paste0("Expected \nEnd of Year Score"), paste0("Actual \nEnd of Year Score"))) +
  
  labs(y = "Score",
       x = " ",
       col = "Student") + 
  
  transition_states(x, wrap=FALSE) +
  
  theme(legend.position = "none") + 
  
  mytheme + 
  
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 12))

animate(sgp)
anim_save(filename="sgp_i.gif")

# SGP II------------------------------------------------------------------------


# Name tags
iam_tina_grob <- textGrob("Tina", gp = gpar(fontsize = 11, fontface = "bold", col = orange))
GrobOutperform1 <- textGrob(paste0("Tina outperformed 70 out of 100 students in her peer group. \nTherefore, her Student Growth Percentile is 70 !"), 
                           gp = gpar(fontsize = 10, fontface = "bold", col = orange))


peergroup <- 
  ggplot(data=train, aes(x = 0, y = 0)) +
  geom_point(color = "white", fill = "white") +

  # Tina's peers
  annotation_custom(peerboyGrob, xmin = -95, xmax = -85, ymin = 20, ymax = 30) +
  annotation_custom(peergirlGrob, xmin = -80, xmax = -70, ymin = 20, ymax = 30) +
  annotation_custom(peerboyGrob, xmin = -65, xmax = -55, ymin = 20, ymax = 30) +
  annotation_custom(peergirlGrob, xmin = -50, xmax = -40, ymin = 20, ymax = 30) +
  annotation_custom(peerboyGrob, xmin = -35, xmax = -25, ymin = 20, ymax = 30) + 
  annotation_custom(peergirlGrob, xmin = -20, xmax = -10, ymin = 20, ymax = 30) +
  annotation_custom(tinaGrob, xmin = -5, xmax = 5, ymin = 20, ymax = 30) + 
  annotation_custom(peergirlGrob, xmin = 10, xmax = 20, ymin = 20, ymax = 30) +
  annotation_custom(peerboyGrob, xmin = 25, xmax = 35, ymin = 20, ymax = 30) + 
  annotation_custom(peergirlGrob, xmin = 40, xmax = 50, ymin = 20, ymax = 30) +

  # I am Tina
  annotation_custom(iam_tina_grob, xmin = -6, xmax = 6, ymin = 31, ymax = 31) + 
  
  # Outperformed
  annotation_custom(bracket2Grob, xmin = -100, xmax = -10, ymin = 15, ymax = 22) + 
  
  annotation_custom(GrobOutperform, xmin = -80, xmax = -20, ymin = 10, ymax = 17) + 
  
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +
  geom_point(x=0, y=0, color = "white") +

  lims(x = c(-110, 50),
       y = c(-10, 50)) + 
  
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  
  transition_layers(layer_length = 1, transition_length = 1, from_blank = TRUE) + 
  
  enter_appear()

animate(peergroup, fps = 3.5) 
anim_save(filename="sgp_ii.gif")
