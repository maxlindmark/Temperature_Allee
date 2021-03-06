####
## 2018.01.09 - Script for reading data and producing the figure: Continuation of bifurcation points (limit point, Hopf and branch point) in two parameter space; temperature and p
####

# Note I have added the arrows and roman numerals using Adoe Acrobat DC

#---- Clear the workspace
rm(list=ls(all=TRUE))

#---- Load libraries (install first if needed)
library(tidyverse)
library(gridExtra)
library(RCurl)
library(patchwork)

#---- Get the bifurcation continuations from github
# q=0.5

#BP <- read.csv("p_temp_dyn_str_c0EK43_BP.csv", header=FALSE)
BP <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/p_temp_generic/q05/p_temp_dyn_str_c0EK43_BP.csv"), header=FALSE)

#LP <- read.csv("p_temp_dyn_str_c0EK43_LP.csv", header=FALSE)
LP <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/p_temp_generic/q05/p_temp_dyn_str_c0EK43_LP.csv"), header=FALSE)

#H <- read.csv("p_temp_dyn_str_c0EK43_H.csv", header=FALSE)
H <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/p_temp_generic/q05/p_temp_dyn_str_c0EK43_H.csv"), header=FALSE)

H <- arrange(H, desc(V5))                       

BP$tc <- BP$V6 - 273.15
H$tc <- H$V6 - 273.15
LP$tc <- LP$V6 - 273.15
                             
# There is a Cusp-point when the limit point and branch point collide into one curve (BP). The limit point only goes to this point, because at lower p-values the BP is the persistence boundary. So for plotting purposes, I will create a new polygon so that the BP is the persistence boundary even when alternative stable states are absent

BP_cusp <- subset(BP, V5 < min(LP$V5))                            
       
BP_cusp$tc <- BP_cusp$tc - 0.07
                                 
pal <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")

ggplot(BP, aes(tc, V5)) + 
  geom_line() +
  geom_line(data = LP, aes(tc, V5)) +
  xlim(10, 37) + 
  ylim(0, 1) 

ggplot(H, aes(tc, V5)) + geom_point() + xlim(10, 37) + ylim(0, 1)

plot(BP$V5~BP$V6)

#library(png)
#mypng_3spec <- readPNG("//storage-og.slu.se/home$/mxli0002/Desktop/ms_II/Draft_1/Figures&Data/T_Rmax19_EK_Heatmap/3spec.png")
                  
pal <- c("white", "#fdcc8a", "#fc8d59", "#d7301f")
pal <- c("white", "#9ecae1", "#3182bd", "#d7301f")
#pal <- c("white","#f4a582","#92c5de","#0571b0")
pal <- c("white", "#fdae6b", "#e6550d", "#d7301f")
                 
#---- Plot for ms
a1 <- ggplot(data=BP, aes(tc, V5)) +              
  coord_cartesian(xlim = c(10, 37), ylim = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  geom_ribbon(data=BP, aes(x=tc, ymax=1, ymin=V5), fill=pal[2], col="NA") + 
  geom_ribbon(data=LP, aes(x=tc, ymax=1, ymin=V5), fill=pal[3], col="NA") + 
  geom_ribbon(data=BP_cusp, aes(x=tc, ymax=1, ymin=V5), fill=pal[3], col="NA") + 
  geom_polygon(data=H, aes(x=tc, y=V5), fill="grey80", col="NA")  + 

  annotate("text", y = c(0.96, 0.87, 0.77, 0.3), x = c(26.3, 34.2, 13, 25), label = c("Bistability", "C-R", "Limit\ncycles", "Stable fixed points"), size=2.9, fontface=3) +
 
  ggtitle("q = 0.5") +

  theme_classic() +
  labs(x = expression(paste("Temperature [", degree*C, "]"))) +
  labs(y = expression(paste("Feeding proportion on juveniles", italic(" (p)")))) +
       
  theme(legend.title = element_text(size=11), # 23 for paper, 28 for pres, same for all sizes
        legend.text = element_text(size=11),
        axis.title = element_text(size=11),
        axis.text = element_text(size=10),
        strip.background = element_blank(),
        panel.border = element_rect(colour="black", fill="NA"),
        plot.title = element_text(size = 13, face = "italic"),
        panel.background = element_rect(fill = pal[1], colour = pal[1]),
        aspect.ratio = 1,
        plot.margin = unit(c(1,0.1,0.1,0.1), "cm")) #+




##----------------
# q=2

# setwd("//storage-og.slu.se/home$/mxli0002/My Documents/Max SLU/Papers/ms_II/Paper/Supplement_Figures_R/General_model_two_param_bif/data/q2")

BP <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/p_temp_generic/q2/p_temp_dyn_str_c0EK43_BP.csv"), header=FALSE)

LP <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/p_temp_generic/q2/p_temp_dyn_str_c0EK43_LP.csv"), header=FALSE)

H <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/p_temp_generic/q2/p_temp_dyn_str_c0EK43_H.csv"), header=FALSE)

H <- arrange(H, desc(V5))                       

BP$tc <- BP$V6 - 273.15
H$tc <- H$V6 - 273.15
LP$tc <- LP$V6 - 273.15
                             
# There is a Cusp-point when the limit point and branch point collide into one curve (BP). The limit point only goes to this point, because at lower p-values the BP is the persistence boundary. So for plotting purposes, I will create a new polygon so that the BP is the persistence boundary even when alternative stable states are absent

BP_cusp <- subset(BP, V5 > max(LP$V5))                            
       
BP_cusp$tc <- BP_cusp$tc - 0.05
                                 
pal <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")

ggplot(BP, aes(tc, V5)) + 
  geom_line() +
  geom_line(data = LP, aes(tc, V5)) +
  xlim(10, 37) + 
  ylim(0, 1) 

ggplot(H, aes(tc, V5)) + geom_point() + xlim(10, 37) + ylim(0, 1)

plot(BP$V5~BP$V6)

#library(png)
#mypng_3spec <- readPNG("//storage-og.slu.se/home$/mxli0002/Desktop/ms_II/Draft_1/Figures&Data/T_Rmax19_EK_Heatmap/3spec.png")
                  
pal <- c("white", "#fdcc8a", "#fc8d59", "#d7301f")
pal <- c("white", "#9ecae1", "#3182bd", "#d7301f")
#pal <- c("white","#f4a582","#92c5de","#0571b0")
pal <- c("white", "#fdae6b", "#e6550d", "#d7301f")
                 
#---- Plot for ms
a2 <- ggplot(data=BP, aes(tc, V5)) +              
  coord_cartesian(xlim = c(10, 37), ylim = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  geom_ribbon(data=BP, aes(x=tc, ymax=V5, ymin=0), fill=pal[2], col="NA") + 
  geom_ribbon(data=LP, aes(x=tc, ymax=V5, ymin=0), fill=pal[3], col="NA") + 
  geom_ribbon(data=BP_cusp, aes(x=tc, ymax=V5, ymin=0), fill=pal[3], col="NA") + 
  geom_polygon(data=H, aes(x=tc, y=V5), fill="grey80", col="NA")  + 

#  annotate("text", y = c(0.97, 0.87, 0.77, 0.3), x = c(26, 34.2, 13, 25), label = c("Bistability", "C-R", "Limit\ncycles", "Stable fixed points"), size=6, fontface=3) +
 
  theme_classic() +
  labs(x = expression(paste("Temperature [", degree*C, "]"))) +
  labs(y = expression(paste("Feeding proportion on juveniles", italic(" (p)")))) +
 
  ggtitle("q = 2") +
  theme(legend.title = element_text(size=11), # 23 for paper, 28 for pres, same for all sizes
        legend.text = element_text(size=11),
        axis.title = element_text(size=11),
        axis.text = element_text(size=10),
        plot.title = element_text(size = 13, face = "italic"),
        strip.background = element_blank(),
        panel.border = element_rect(colour="black", fill="NA"),
        panel.background = element_rect(fill = pal[1], colour = pal[1]),
        aspect.ratio = 1,
        plot.margin = unit(c(1,0.1,0.1,0.1), "cm")) #+
#        
#        annotation_raster(mypng_3spec,ymin=30,ymax=37,xmin=0.5,xmax=0.63) 


##-- Plot both together:

a1 + a2


