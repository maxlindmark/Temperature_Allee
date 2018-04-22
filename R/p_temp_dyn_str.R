####
## 2018.01.09 - Script for reading data and producing the figure: Continuation of bifurcation points (limit point, Hopf and branch point) in two parameter space; temperature and p
####

#---- Clear the workspace
rm(list=ls(all=TRUE))

#---- Load libraries (install first if needed)
library(tidyverse)
library(gridExtra)
library(RCurl)

#---- Get the bifurcation continuations from github
# p=1, default
BP <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/p_temp_dyn_str/p_temp_dyn_str_c0EK43_BP.csv"), header=FALSE)

LP <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/p_temp_dyn_str/p_temp_dyn_str_c0EK43_LP.csv"), header=FALSE)

H <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/p_temp_dyn_str/p_temp_dyn_str_c0EK43_H.csv"), header=FALSE)

BP$tc <- BP$V5 - 273.15
H$tc <- H$V5 - 273.15
LP$tc <- LP$V5 - 273.15
                              
# There is a Cusp-point when the limit point and branch point collide into one curve (BP). The limit point only goes to this point, because at lower p-values the BP is the persistence boundary. So for plotting purposes, I will create a new polygon so that the BP is the persistence boundary even when alternative stable states are absent

BP_cusp <- subset(BP, V6 < min(LP$V6))                               
                                 
pal <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")

#library(png)
#mypng_3spec <- readPNG("//storage-og.slu.se/home$/mxli0002/Desktop/ms_II/Draft_1/Figures&Data/T_Rmax19_EK_Heatmap/3spec.png")
                  
#---- Plot for ms
ggplot(data=BP, aes(V6, tc)) +              
  coord_cartesian(ylim = c(10, 37), xlim = c(0.5, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  geom_ribbon(data=BP, aes(x=V6, ymax=40, ymin=tc), fill=pal[2], col="NA") + 
  geom_ribbon(data=LP, aes(x=V6, ymax=40, ymin=tc), fill=pal[3], col="NA") + 
  geom_ribbon(data=BP_cusp, aes(x=V6, ymax=40, ymin=tc), fill=pal[3], col="NA") + 
  geom_ribbon(data=H, aes(x=V6, ymax=tc, ymin=9), fill="grey80", col="NA") + 

  annotate("text", x = c(0.91, 0.86, 0.77, 0.7), y=c(32, 36.2, 13, 24), label = c("Bistability", "Predator extinction", "Limit cycles", "Stable fixed points"), size=9, fontface=3) +
 
  theme_classic() +
  labs(y = expression(paste("Temperature [", degree*C, "]"))) +
  labs(x = expression(paste("Feeding proportion on juveniles ", italic("(p)")))) +
       
  theme(legend.title = element_text(size=23), 
        legend.text = element_text(size=21),
        axis.title = element_text(size=23),
        axis.text = element_text(size=16),
        strip.background = element_blank(),
        panel.border = element_rect(colour="black", fill="NA"),
        panel.background = element_rect(fill = pal[1], colour = pal[1]),
        aspect.ratio = 1,
        plot.margin = unit(c(1,0.1,0.1,0.1), "cm")) #+
#        
#        annotation_raster(mypng_3spec,ymin=30,ymax=37,xmin=0.5,xmax=0.63) 