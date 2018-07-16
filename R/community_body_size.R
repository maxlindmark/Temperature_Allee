####
## 2018.01.09 - Script for reading data and producing the figure: Biomass weighted mean body of the community, given variation in p, ERmax and c
####

#---- *Warning* This is a long and pretty messy script because I have several temperature scenarios and need to calculate stability, structure, mean size etc., for each one of them, and separating them already in matcont is not an option.

#---- Clear the workspace
rm(list=ls(all=TRUE))

#---- Load libraries (install first if needed)
library(tidyverse)
library(gridExtra)
library(RCurl)
library(grid)
library(png)

#---- Get the equilibrium continuations from github
#---- Will do p=0.5 and p=1 separately because of the precense of alternative stable states
#---- p05
# p=05, EK=0.
# c=0 
p05EK0c0 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/community_body_size/com_body_size_p05EK0c0.csv"), header=FALSE)
p05EK0c0$c_c <- 0
p05EK0c0$ERmax <- 0
p05EK0c0$p <- 0.5 

# c=0.005 
p05EK0c05 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/community_body_size/com_body_size_p05EK0c05.csv"), header=FALSE)
p05EK0c05$c_c <- 0.005
p05EK0c05$ERmax <- 0
p05EK0c05$p <- 0.5

# p=05, EK=-0.43.
# c=0 
p05EK43c0 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/community_body_size/com_body_size_p05EK43c0.csv"), header=FALSE)
p05EK43c0$c_c <- 0
p05EK43c0$ERmax <- 0.43
p05EK43c0$p <- 0.5 

# c=0.005 
p05EK43c05 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/community_body_size/com_body_size_p05EK43c05.csv"), header=FALSE)
p05EK43c05$c_c <- 0.005
p05EK43c05$ERmax <- 0.43
p05EK43c05$p <- 0.5

#---- Merge data frames and then rename columns
dat_ty_p05 <- tbl_df(rbind(p05EK0c0, p05EK0c05, p05EK43c0, p05EK43c05))

dat_ty_p05

dat_ty_p05 <- rename(dat_ty_p05,
                     R = V1,
                     J = V2,
                     A = V3,
                     P = V4,
                     Temp = V5)

dat_ty_p05$stability <- 1

dat_ty_p05$state <- 1 # This variable is for plotting purposes to split by alternative stable states in the case of p=1

#---- p=1
#---- In the case of bistability (i.e. p=1) it is more tricky, since I will have to calculate boundaries for alternative stable states separately for each scenario. 
# Define an extinction threshold for predators
ext_t <- 0.00001

# p=1, EK=0.
# c=0 
p1EK0c0 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/community_body_size/com_body_size_p1EK0c0.csv"), header=FALSE)
p1EK0c0$c_c <- 0
p1EK0c0$ERmax <- 0
p1EK0c0$p <- 1

ggplot(p1EK0c0, aes(V5, V4)) + geom_point()

# V4 and V5 is Predator and temperature, respectively
#bp <- min(subset(p1EK0c0, V4 < ext_t)$V5), no predator extinction in this scenario!
lp <- max(subset(p1EK0c0, V4 > ext_t)$V5)

# Find biomass density of predators at the limit point:
pred_lp <- min(subset(p1EK0c0, V5 == lp & V4 > ext_t)$V4)

# Now add in stability (1=stable, 0=unstable)
p1EK0c0$stability <- 1                         

p1EK0c0$stability <- ifelse(p1EK0c0$V4 < pred_lp & p1EK0c0$V5 < lp & p1EK0c0$V4 > ext_t, 0, p1EK0c0$stability)
ggplot(p1EK0c0, aes(V5, V4, colour=factor(stability))) + geom_point()

p1EK0c0$state <- ifelse(p1EK0c0$V4 >= pred_lp & p1EK0c0$stability == 1, 2, 0)
p1EK0c0 %>% filter(., stability==1) %>% ggplot(., aes(V5, V4, colour=factor(state))) + geom_point()

# c=0.005 
p1EK0c05 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/community_body_size/com_body_size_p1EK0c05.csv"), header=FALSE)
p1EK0c05$c_c <- 0.005
p1EK0c05$ERmax <- 0
p1EK0c05$p <- 1

# V4 and V5 is Predator and temperature, respectively
bp <- min(subset(p1EK0c05, V4 < ext_t)$V5)
lp <- max(subset(p1EK0c05, V4 > ext_t)$V5)

# Find biomass density of predators at the limit point:
pred_lp <- min(subset(p1EK0c05, V5 == lp & V4 > ext_t)$V4)

# Now add in stability (1=stable, 0=unstable)
p1EK0c05$stability <- 1                         

p1EK0c05$stability <- ifelse(p1EK0c05$V4 < pred_lp & p1EK0c05$V5 < lp & p1EK0c05$V4 > ext_t, 0, p1EK0c05$stability)
ggplot(p1EK0c05, aes(V5, V4, colour=factor(stability))) + geom_point()

p1EK0c05$state <- ifelse(p1EK0c05$V4 >= pred_lp & p1EK0c05$stability == 1, 2, 0)
p1EK0c05 %>% filter(., stability==1) %>% ggplot(., aes(V5, V4, colour=factor(state))) + geom_point()

# p=1, EK=-0.43.
# c=0 
p1EK43c0 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/community_body_size/com_body_size_p1EK43c0.csv"), header=FALSE)
p1EK43c0$c_c <- 0
p1EK43c0$ERmax <- 0.43
p1EK43c0$p <- 1

bp <- min(subset(p1EK43c0, V4 < ext_t)$V5)
lp <- max(subset(p1EK43c0, V4 > ext_t)$V5)

pred_lp <- min(subset(p1EK43c0, V5 == lp & V4 > ext_t)$V4)

p1EK43c0$stability <- 1                         

p1EK43c0$stability <- ifelse(p1EK43c0$V4 < pred_lp & p1EK43c0$V5 < lp & p1EK43c0$V4 > ext_t, 0, p1EK43c0$stability)
ggplot(p1EK43c0, aes(V5, V4, colour=factor(stability))) + geom_point()

p1EK43c0$state <- ifelse(p1EK43c0$V4 >= pred_lp & p1EK43c0$stability == 1, 2, 0)
p1EK43c0 %>% filter(., stability==1) %>% ggplot(., aes(V5, V4, colour=factor(state))) + geom_point()

# c=0.005 
p1EK43c05 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/community_body_size/com_body_size_p1EK43c05.csv"), header=FALSE)
p1EK43c05$c_c <- 0.005
p1EK43c05$ERmax <- 0.43
p1EK43c05$p <- 1

bp <- min(subset(p1EK43c05, V4 < ext_t)$V5)
lp <- max(subset(p1EK43c05, V4 > ext_t)$V5)

pred_lp <- min(subset(p1EK43c05, V5 == lp & V4 > ext_t)$V4)

p1EK43c05$stability <- 1                         

p1EK43c05$stability <- ifelse(p1EK43c05$V4 < pred_lp & p1EK43c05$V5 < lp & p1EK43c05$V4 > ext_t, 0, p1EK43c05$stability)
ggplot(p1EK43c05, aes(V5, V4, colour=factor(stability))) + geom_point()

p1EK43c05$state <- ifelse(p1EK43c05$V4 >= pred_lp & p1EK43c05$stability == 1, 2, 0)
p1EK43c05 %>% filter(., stability==1) %>% ggplot(., aes(V5, V4, colour=factor(state))) + geom_point()

#---- Merge data frames and then rename columns
dat_ty_p1 <- tbl_df(rbind(p1EK0c0, p1EK0c05, p1EK43c0, p1EK43c05))

dat_ty_p1

dat_ty_p1 <- rename(dat_ty_p1,
                    R = V1,
                    J = V2,
                    A = V3,
                    P = V4,
                    Temp = V5)

#---- Now I can merge all the scenarios...
dat_ty <- rbind(dat_ty_p1, dat_ty_p05)
                   
dat_ty$tc <- dat_ty$Temp - 273.15

#---- Number each temperature & foodweb scenario
dat_ty$scen <- NA

#-- c = 0
dat_ty$scen <- ifelse(dat_ty$ERmax == 0 & dat_ty$p == 0.5 & dat_ty$c_c == 0, 1, dat_ty$scen)
dat_ty$scen <- ifelse(dat_ty$ERmax == 0 & dat_ty$p == 1 & dat_ty$c_c == 0, 2, dat_ty$scen)
dat_ty$scen <- ifelse(dat_ty$ERmax == 0.43 & dat_ty$p == 0.5 & dat_ty$c_c == 0, 3, dat_ty$scen)
dat_ty$scen <- ifelse(dat_ty$ERmax == 0.43 & dat_ty$p == 1 & dat_ty$c_c == 0, 4, dat_ty$scen)

#-- c = 0.005
dat_ty$scen <- ifelse(dat_ty$ERmax == 0 & dat_ty$p == 0.5 & dat_ty$c_c == 0.005, 5, dat_ty$scen)
dat_ty$scen <- ifelse(dat_ty$ERmax == 0 & dat_ty$p == 1 & dat_ty$c_c == 0.005, 6, dat_ty$scen)
dat_ty$scen <- ifelse(dat_ty$ERmax == 0.43 & dat_ty$p == 0.5 & dat_ty$c_c == 0.005, 7, dat_ty$scen)
dat_ty$scen <- ifelse(dat_ty$ERmax == 0.43 & dat_ty$p == 1 & dat_ty$c_c == 0.005, 8, dat_ty$scen)

sort(unique(dat_ty$scen))

#---- Create "state" variable column and go to long format
dlong <- gather(dat_ty, state_v, biomass_dens, R, J, A, P, factor_key=T)

dlong %>%
      filter(., tc < 34 & tc > 9 & state_v == "P") %>% 
             ggplot(., aes(x = tc, y = biomass_dens, 
             colour = factor(stability))) + 
               geom_point(size=1) + 
                 facet_wrap(p ~ state_v, ncol = 4, scales = "free_y") + 
                   xlim(5,35)

#---- Calculate mean biomass levels for only stable systems:
dfs <- dat_ty %>% filter(stability ==  1 & tc > 11 & tc < 50)

dfs$tc <- round(dfs$tc, digits=1) # I do need to round since below I calculate means per temperature. 

dfs$m_size <- (dfs$J*3.9 + dfs$A*32.4 + dfs$P*642.6) / (dfs$J + dfs$A + dfs$P)

# Summarize: calculate mean size for each rounded temperature and each scenario
dfs2 <- dfs %>% group_by(tc, scen, state) %>% # this is key since I want mean size for each stable state in the case of bistability (and the reason for the messy code...)
                summarise(mean_mass = mean(m_size))

# Create groups for plotting (Rmax, c and p)
dfs2$c_group <- ifelse(dfs2$scen < 5, 0, 0.005)
dfs2$ek_group <- ifelse(dfs2$scen %in% c(1,2,5,6), 0, 0.43)
dfs2$p <- ifelse(dfs2$scen %in% c(2,4,6,8), 1, 0.5)

# This grouping separates everything but ERmax, which is split by window in plot
dfs2$col_group <- dfs2$scen
dfs2$col_group <- ifelse(dfs2$col_group == 3, 1, dfs2$col_group)
dfs2$col_group <- ifelse(dfs2$col_group == 4, 2, dfs2$col_group)
dfs2$col_group <- ifelse(dfs2$col_group == 7, 5, dfs2$col_group)
dfs2$col_group <- ifelse(dfs2$col_group == 8, 6, dfs2$col_group)

# Calculate number of species in food chain. First find extinction/persistence boundaries:
bound <- data.frame(filter(dat_ty, p==1, stability == 0 & tc < 35) %>% 
                    group_by(scen) %>%
                             summarise(bp = min(tc),
                                       lp = max(tc)))
# Add in default number of species
dfs2$no_spec <- 3

w_tresh <- exp(4) # This value comes from the last two plots (which are on log-scale, hence exp()), where it is clear that the stable P-C-R system never reaches below this mean size and the C-R system never goes above it. An alternative would be to calculate the mean weighted biomass for each scenarion within the unstable region, but that is not needed here becuase the difference is so large.
dfs2$no_spec <- ifelse(dfs2$scen == 2 & dfs2$mean_mass < w_tresh, 2, dfs2$no_spec)
dfs2$no_spec <- ifelse(dfs2$scen == 4 & dfs2$mean_mass < w_tresh, 2, dfs2$no_spec)
dfs2$no_spec <- ifelse(dfs2$scen == 6 & dfs2$mean_mass < w_tresh, 2, dfs2$no_spec) 
dfs2$no_spec <- ifelse(dfs2$scen == 8 & dfs2$mean_mass < w_tresh, 2, dfs2$no_spec)

# Also need to find no_spec for p=05
pred_ex_p05 <- data.frame(filter(dat_ty, p==0.5, P > 0.0001) %>% 
                          group_by(scen) %>%
                          summarise(ext = max(tc)))

pred_ex_p05 # Only scenario 7 has predator extinction in the given temperature range

dfs2$no_spec <- ifelse(dfs2$scen == 7 & dfs2$tc >= pred_ex_p05[4,2], 2, dfs2$no_spec)

##---- Plot (separating by states). Only stable branches (and no cycles). Rounded temperatures to nearest C

pal3 <- c("#ca0020","#f4a582","#92c5de","#0571b0")

##-- A bit difficult to see curves above. Here I use facet_grid instead:
dfs2$ek_group2 <- as.factor(dfs2$ek_group)
levels(dfs2$ek_group2)=c("0" = expression(paste(italic("E"[R[max]]),"=0")),
                         "0.43" = expression(paste(italic("E"[R[max]]),"=-0.43")))

dfs2$c_group2 <- as.factor(dfs2$c_group)
levels(dfs2$c_group2)=c("0"=expression(paste(italic(c),"=0")),
                        "0.005" = expression(paste(italic(c),"=0.005")))

dfs2$p2 <- as.factor(dfs2$p)
levels(dfs2$p2)=c("0.5"=expression(paste(italic(p),"=0.5")),
                  "1" = expression(paste(italic(p),"=1")))

#--- This is a plot of the raw data as unmanipulated as possible
dfs2 %>% 
     filter(tc > 12 & tc < 36) %>%
            ggplot(., aes(tc, log(mean_mass), 
              group = factor(state))) +
                
              geom_line(alpha=0.5, size = 2) + 
                
              labs(x=expression(paste("Temperature [", degree*C, "]")),
                   y="ln(mean community body size)", colour = "p") + 
                                
              facet_wrap(~scen, ncol=4) +
                
              scale_x_continuous(breaks = round(seq(min(dfs2$tc), max(dfs2$tc), by = 4),1)) + 
              scale_y_continuous(breaks = seq(3, 6.5, by = 0.5)) +
                               
              theme_bw() + 
              theme(aspect.ratio = 1,
              axis.text=element_text(size=12),
              axis.title=element_text(size=15),
              legend.title=element_text(size=17, face="italic"),
              legend.text=element_text(size=13, face="italic"),
              strip.text=element_text(size=15))
                          
# Looks ok but difficult to see the different temperature scenarios

##---- Plot for ms
# Create dataframe to hold texts for subplots A-D, include all levels for grouping to facet_grid (sub-panel structure)
dat_text <- data.frame(label = c("A", "B", "C", "D"),
                       ek_group2 = c("paste(italic(\"E\"[R[max]]), \"=0\")",
                                     "paste(italic(\"E\"[R[max]]), \"=0\")",
                                     "paste(italic(\"E\"[R[max]]), \"=-0.43\")",
                                     "paste(italic(\"E\"[R[max]]), \"=-0.43\")"),
                       p2 = c("paste(italic(p), \"=0.5\")",
                              "paste(italic(p), \"=1\")",
                              "paste(italic(p), \"=0.5\")",
                              "paste(italic(p), \"=1\")"),
                       c_group = 0,
                       no_spec = 2)
                 
pal <- c("#ca0020","#f4a582","#92c5de","#0571b0") # From colorbrewer

dfs2 %>% 
     filter(tc > 13 & tc < 36 & state %in% c(1,2)) %>%
            ggplot(., aes(tc, log(mean_mass), 
              colour = factor(c_group),
              linetype = factor(no_spec),
              size = factor(no_spec))) +
              coord_cartesian(xlim=c(13,36), ylim=c(3,6.5)) +
              
              geom_point(data=subset(dfs2, p==1 & ek_group == 0 & c_group == 0.005),
                         aes(x=bound[3,3], y=3), colour=pal[4], size=10, shape=42, alpha = 0.02) +

              geom_point(data=subset(dfs2, p==0.5 & ek_group == 0.43 & c_group == 0.005),
                         aes(x=min(subset(dfs2, p==0.5 & no_spec == 2)$tc), y=3), colour=pal[4], 
                         size=10, shape=42, alpha = 0.02) +
          
              geom_point(data=subset(dfs2, p==1 & ek_group == 0.43 & c_group == 0.005),
                         aes(x=bound[4,3], y=3), colour=pal[4], size=10, shape=42, alpha = 0.02) +
          
              geom_point(data=subset(dfs2, p==1 & ek_group == 0.43 & c_group == 0),
                         aes(x=bound[2,3], y=3), colour=pal[2], size=10, shape=42, alpha = 0.02) +

              scale_color_manual(values=c(pal[2], pal[4])) +
              scale_linetype_manual(values=c(2,1)) +
              scale_size_manual(values=c(1.4,3)) +
                                
              labs(x=expression(paste("Temperature [", degree*C, "]")),
                   y="ln(mean community body size)", colour = "c", linetype = "Number of\nspecies") + 

              geom_line(alpha = 0.7) + 
                
              geom_line(data = filter(dfs2, tc > 13 & tc < 36 & state == 0), 
                        aes(tc, log(mean_mass), colour = factor(c_group), 
                        linetype = factor(no_spec),
                        size = factor(no_spec)), 
                        alpha = 0.7) +
                
              facet_grid(ek_group2~p2, labeller = label_parsed) +
                  
              guides(colour = guide_legend(override.aes = list(alpha = 1, size=4)),
                     linetype = guide_legend(override.aes = list(alpha = 1, size=0.6)), size = FALSE) +

              scale_x_continuous(breaks = round(seq(min(dfs2$tc), max(dfs2$tc), by = 4),1)) +
              scale_y_continuous(breaks = seq(3, 6.45, by = 0.5)) +
              geom_text(data=dat_text,aes(x=36, y=6.5, label=label), size=6, fontface="bold", colour="black") +
                               
              theme_bw() + 
              theme(aspect.ratio=1,
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    axis.text=element_text(size=15),
                    axis.title=element_text(size=15),
                    legend.title=element_text(size=15, face="italic"),
                    legend.text=element_text(size=15),
                    strip.background=element_blank(),
                    panel.border=element_rect(colour="black", fill="NA"),
                    strip.text=element_text(size=15))
                                    
