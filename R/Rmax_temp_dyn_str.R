####
## 2018.01.09 - Script for reading data and producing the figure: Continuation of bifurcation points (limit point and branch point) in two parameter space; temperature and Rmax, for different c and ERmax values
####

#---- Clear the workspace
rm(list=ls(all=TRUE))

#---- Load libraries (install first if needed)
library(tidyverse) 
library(gridExtra)
library(RCurl)
library(png)
library(grid)

#---- Get the continuations of bifurcations from github
# c=0, EK=0. *No branch point for this scenario within the selected parameter range!
BPc0EK0 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Rmax_temp_dyn_str/Rmax_temp_dyn_str_c0EK0_BP.csv"), header=FALSE)
BPc0EK0$c_c <- 0
BPc0EK0$ERmax <- 0
BPc0EK0$bif <- "BP" # to distinguish between bifurcations later

# c=0.005, EK=0
BPc05EK0 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Rmax_temp_dyn_str/Rmax_temp_dyn_str_c05EK0_BP.csv"), header=FALSE)
BPc05EK0$c_c <- 0.005
BPc05EK0$ERmax <- 0
BPc05EK0$bif <- "BP"

LPc05EK0 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Rmax_temp_dyn_str/Rmax_temp_dyn_str_c05EK0_LP.csv"), header=FALSE)
LPc05EK0$V7 <- NA
LPc05EK0$c_c <- 0.005
LPc05EK0$ERmax <- 0
LPc05EK0$bif <- "LP"

# c=0, EK=-0.43
BPc0EK43 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Rmax_temp_dyn_str/Rmax_temp_dyn_str_c0EK43_BP.csv"), header=FALSE)
BPc0EK43$c_c <- 0
BPc0EK43$ERmax <- 0.43
BPc0EK43$bif <- "BP"

LPc0EK43 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Rmax_temp_dyn_str/Rmax_temp_dyn_str_c0EK43_LP.csv"), header=FALSE)
LPc0EK43$V7 <- NA
LPc0EK43$c_c <- 0
LPc0EK43$ERmax <- 0.43
LPc0EK43$bif <- "LP"

# c=0.005, EK=-0.43
BPc05EK43 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Rmax_temp_dyn_str/Rmax_temp_dyn_str_c05EK43_BP.csv"), header=FALSE)
BPc05EK43$c_c <- 0.005
BPc05EK43$ERmax <- 0.43
BPc05EK43$bif <- "BP"

LPc05EK43 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Rmax_temp_dyn_str/Rmax_temp_dyn_str_c05EK43_LP.csv"), header=FALSE)
LPc05EK43$V7 <- NA
LPc05EK43$c_c <- 0.005
LPc05EK43$ERmax <- 0.43
LPc05EK43$bif <- "LP"

#---- Merge data frames and then rename columns
dat_ty <- tbl_df(rbind(BPc0EK0, BPc05EK0, LPc05EK0, BPc0EK43, LPc0EK43, BPc05EK43, LPc05EK43))

#dat_ty
glimpse(dat_ty)

dat_ty <- rename(dat_ty,
                 R = V1,
                 J = V2,
                 A = V3,
                 P = V4,
                 Temp = V5,
                 Rmax = V6,
                 na = V7)

##---- Inspect bifurcations in two parameter space
dat_ty$tc <- dat_ty$Temp-273.15

ggplot(dat_ty, aes(tc, Rmax, colour=bif)) + 
       geom_point() + 
       facet_grid(c_c ~ ERmax) + 
       scale_x_continuous(expand = c(0, 0)) +
       scale_y_continuous(expand = c(0, 0)) +
       coord_cartesian(xlim=c(15,35), ylim=c(1.3,2.6)) 

# Add temperature-scaling scenario, used for plotting purposes (facets and for panel specific boxes)
dat_ty$scen <- 1
dat_ty$scen <- ifelse(dat_ty$c_c == 0 & dat_ty$ERmax == 0.43, 2, dat_ty$scen)
dat_ty$scen <- ifelse(dat_ty$c_c == 0.005 & dat_ty$ERmax == 0, 3, dat_ty$scen)
dat_ty$scen <- ifelse(dat_ty$c_c == 0.005 & dat_ty$ERmax == 0.43, 4, dat_ty$scen)

# Calculate min x in each panel for alternative stable states (for rectangles in plot, see below)
xmin_p2 <- max(subset(dat_ty, scen == 2 & bif == "BP")$tc)
xmin_p3 <- max(subset(dat_ty, scen == 3 & bif == "BP")$tc)
xmin_p4 <- max(subset(dat_ty, scen == 4 & bif == "BP")$tc)

xmin_p3_lp <- max(subset(dat_ty, scen == 3 & bif == "LP" & Rmax < 2.6)$tc)
xmin_p4_lp <- max(subset(dat_ty, scen == 4 & bif == "LP" & Rmax < 2.6)$tc)

#---- custom2-function to plot png's in specific facets instead of all  
# https://stackoverflow.com/questions/44688623/adding-custom-images-to-ggplot-facets
annotation_custom2 <- 
function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){ layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))}
 
#---- Full plot
pal <- c("white", "#fdae6b", "#e6550d", "#d7301f")

#-- Create new variables for plotting
#dat_ty$ERmax2 <- factor(dat_ty$ERmax)
#levels(dat_ty$ERmax2) = c("0" = expression(paste(italic("E"[Rmax]),"=0")),
#                          "0.43" = expression(paste(italic("E"[Rmax]),"=-0.43")))

dat_ty$ERmax2 <- factor(dat_ty$ERmax)
levels(dat_ty$ERmax2) = c("0" = expression(paste(italic("E"[R[max]]),"=0")),
                          "0.43" = expression(paste(italic("E"[R[max]]),"=-0.43")))


dat_ty$c_c <- factor(dat_ty$c_c)
levels(dat_ty$c_c) = c("0" = expression(paste(italic("c"),"=0")),
                       "0.005" = expression(paste(italic("c"),"=0.005")))

#---- Add in food web png's. *Save locally to your computer!
library(png)
mypng_3spec <- readPNG("//storage-og.slu.se/home$/mxli0002/My Documents/Max SLU/Papers/ms_II/Paper/Figures_R/Main_Figures/3spec_v6.png")

mypng_2spec <- readPNG("//storage-og.slu.se/home$/mxli0002/My Documents/Max SLU/Papers/ms_II/Paper/Figures_R/Main_Figures/2spec_v6.png")
                 
# Create a variable for food web structure to match legend in figure 
fws <- tail(dat_ty)
fws$bif <- 2 # this is the new level, before only "BP" and "LP"
fws$Rmax <- 1.3
fws$tc <- 16

dat_ty2 <- rbind(dat_ty, fws)

# Create dataframe to hold texts for subplots A-D, include all levels for grouping to facet_grid (sub-panel structure)
#dat_text <- data.frame(label = c("A", "B", "C", "D"),
#                       ERmax2 = c("paste(italic(\"E\"[Rmax]), \"=0\")",
#                                  "paste(italic(\"E\"[Rmax]), \"=-0.43\")",
#                                  "paste(italic(\"E\"[Rmax]), \"=0\")",
#                                  "paste(italic(\"E\"[Rmax]), \"=-0.43\")"),
#                       c_c = c("paste(italic(\"c\"), \"=0\")",
#                               "paste(italic(\"c\"), \"=0\")",
#                               "paste(italic(\"c\"), \"=0.005\")",
#                               "paste(italic(\"c\"), \"=0.005\")"),
#                       bif = 2)



dat_text <- data.frame(label = c("A", "B", "C", "D"),
                       ERmax2 = c("paste(italic(\"E\"[R[max]]), \"=0\")",
                                  "paste(italic(\"E\"[R[max]]), \"=-0.43\")",
                                  "paste(italic(\"E\"[R[max]]), \"=0\")",
                                  "paste(italic(\"E\"[R[max]]), \"=-0.43\")"),
                       c_c = c("paste(italic(\"c\"), \"=0\")",
                               "paste(italic(\"c\"), \"=0\")",
                               "paste(italic(\"c\"), \"=0.005\")",
                               "paste(italic(\"c\"), \"=0.005\")"),
                       bif = 2)



# Plot for ms
ggplot(dat_ty2, aes(tc, Rmax, fill = factor(bif))) +
       scale_x_continuous(expand = c(0, 0)) +
       scale_y_continuous(expand = c(0, 0)) +
       coord_cartesian(xlim=c(15,35), ylim=c(1.3,2.6)) +
       scale_fill_manual(values = c(pal[1], pal[2], pal[3]), 
       labels=c("P-C-R", "Bistable:\nP-C-R/C-R", "C-R"), guides(name="Community \ncomposition")) +

       facet_grid(c_c ~ ERmax2, labeller= label_parsed) +
       
       geom_rect(aes(xmin=15, xmax=35, ymin=1.3, ymax=2.6), fill=pal[1]) +
       geom_rect(data=subset(dat_ty2, scen==2), aes(xmin=xmin_p2, xmax=35, ymin=1.3, ymax=2.6), fill=pal[2]) +
       geom_rect(data=subset(dat_ty2, scen==3), aes(xmin=xmin_p3, xmax=35, ymin=1.3, ymax=2.6), fill=pal[2]) +
       geom_rect(data=subset(dat_ty2, scen==4), aes(xmin=xmin_p4, xmax=35, ymin=1.3, ymax=2.6), fill=pal[2]) +
       geom_rect(data=subset(dat_ty2, scen==3), aes(xmin=xmin_p3_lp, xmax=35, ymin=1.3, ymax=2.6), fill=pal[3]) +
       geom_rect(data=subset(dat_ty2, scen==4), aes(xmin=xmin_p4_lp, xmax=35, ymin=1.3, ymax=2.6), fill=pal[3]) +
       
       geom_ribbon(aes(x=tc, ymax=Rmax, ymin=1.3)) +
              
       theme_classic() +
       labs(x = expression(paste("Temperature [", degree*C, "]")))+
       labs(y = expression(paste(italic("R"[paste(max, ",","T19")]), " [g", ~m^{-3}, "]"))) +
       
       theme(panel.spacing = unit(0.9, "lines"),
             panel.border = element_rect(colour="black", fill="NA"),
             legend.title = element_text(size=18),
             legend.text = element_text(size=13, face = "italic"),
             axis.text = element_text(size=13), 
             axis.title = element_text(size=18),
             strip.text = element_text(size=18),      
             strip.background = element_blank(),
             aspect.ratio = 1,
             legend.key = element_rect(color = "gray50")) +
                    
       geom_text(data=dat_text,aes(x=34, y=2.53, label=label), size=6, fontface="bold") +
       annotation_raster(mypng_3spec,
       ymin=2.1,ymax=2.57,xmin=15.4,xmax=21.7) + 
       
       annotation_custom2(rasterGrob(mypng_2spec, interpolate=TRUE), xmin=31.7, 
       xmax=34.89, ymin=1.3, ymax=1.6, data= subset(dat_ty2, scen > 1))
         
      
