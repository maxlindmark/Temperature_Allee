####
## 2018.01.09 - Script for reading data and producing the figure: Continuation over temperature for two different p-values
####

#---- Continuation over temperature for two different p-values

#---- Clear the workspace
rm(list=ls(all=TRUE))

#---- Load libraries (install first if needed)
library(tidyverse)
library(gridExtra)
library(RCurl)

#---- Get the files for p = 0.5 and 1 from github
# p=1, default
dat_p1 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Temp_Allee/Temp_Allee.csv"), header=FALSE)

dat_p1$p <- 1

# p=0.5
dat_p05 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Temp_Allee/Temp_Allee_p05.csv"), header=FALSE)

dat_p05$p <- 0.5

dat <- rbind(dat_p05, dat_p1)

dat <- rename(dat,
              R = V1,
              J = V2,
              A = V3,
              P = V4,
              Temp = V5)

dat_ty <- dat[order(dat$Temp),]

glimpse(dat_ty)
head(dat_ty)

##---- Add in stability. Because the curve folds, I do that by finding the maximum temperature before the curve folds (the limit point), and then the minimum temperature where predators are extinct (the branch point). Here, extinction means below a specific threshold since they first infinitely small (non-negative). When the temperature at these bifurcations are known, I can identify the bistable curve because it will be in between the two curves.
              
# Find the temperatures of the limit and branch points. This approach approximates the branch point and limit point on the third decimal

ext_t <- 0.00001
dat_ty$tc <- dat_ty$Temp-273.15

bp <- min(subset(dat_ty, p == 1 & P < ext_t)$tc)
lp <- max(subset(dat_ty, p == 1 & P > ext_t)$tc)

# Find biomass density of predators at the limit point:
pred_lp <- min(subset(dat_ty, tc == lp & p == 1)$P)

# Now add in stability (1=stable, 0=unstable)
dat_ty$stability <- 1                         

dat_ty$stability <- ifelse(dat_ty$P < pred_lp & dat_ty$tc < lp & dat_ty$P > ext_t, 0, dat_ty$stability)

#---- Create "state" variable column and go to tidy format
dlong <- gather(dat_ty, state_v, biomass_dens, R, J, A, P, factor_key=T)

#---- Plot to inspect
dlong %>%
      filter(., tc < 34 & tc > 9) %>% 
             ggplot(., aes(x = tc, y = biomass_dens, 
             colour = factor(stability))) + 
               geom_point(size=1) + 
                 facet_wrap(p ~ state_v, ncol = 4, scales = "free_y") + 
                   xlim(5,35)

#---- Now, based on the dimensions of the plot above, make a 3*2 plot in base R, with invasion and persistence boundaries, boxes for regulation etc...

# Find temperature of Hopf-bifurcation (when p=0.5)
hopf_t <-  min(subset(dat_ty, p == 0.5)$tc)



#---- Now, read in stable time orbits from p=0.5 and extract min and max for each temperature for cycles. The number after H is the temperature.

H10 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Temp_Allee/orbits/H10.csv"), header=FALSE)
H11 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Temp_Allee/orbits/H11.csv"), header=FALSE)
H12.3 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Temp_Allee/orbits/H12.3.csv"), header=FALSE)
H12 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Temp_Allee/orbits/H12.csv"), header=FALSE)
H6 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Temp_Allee/orbits/H6.csv"), header=FALSE)
H7 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Temp_Allee/orbits/H7.csv"), header=FALSE)
H8 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Temp_Allee/orbits/H8.csv"), header=FALSE)
H9 <- read.csv(text=getURL("https://raw.githubusercontent.com/maxlindmark/Temperature_Allee/master/Data/Temp_Allee/orbits/H9.csv"), header=FALSE)

#---- First apply read.csv, then rbind
dat_o_ty <- rbind(H6,H7,H8,H9,H10,H11,H12,H12.3)

glimpse(dat_o_ty)

dat_o_ty <- rename(dat_o_ty,
                   R = V1,
                   J = V2,
                   A = V3,
                   P = V4,
                   t = V5,
                   Temp = V6)
                   
dlong_o <- gather(dat_o_ty, state_v, biomass_dens, R, J, A, P, factor_key=T)
   
#---- These plots are rather big, but uncomment if you want to see!
# Plot curves for all temperature by state variables
#ggplot(dlong_o, aes(t, biomass_dens, colour=factor(Temp))) +
#  facet_wrap(~state_v) + 
#    geom_point()

# Same, but with facet_grid
#ggplot(dlong_o, aes(t, biomass_dens, colour=factor(Temp))) +
#  facet_grid(Temp ~ state_v) + 
#    geom_point()

#-- summarise mean and max biomasses in stable orbits:
# Filter to get only period solution
ds <- filter(dlong_o, t > 5000)

## summarize
dfs2 <- ds %>% group_by(Temp, state_v) %>%
               summarise(max_b = max(biomass_dens), min_b = min(biomass_dens))

dfs2$tc <- dfs2$Temp-273.15

# Plot only min and max from orbits
#ggplot(dfs2, aes(Temp, max_b)) +
#  facet_wrap(~ state_v) + 
#    geom_point() + 
#      geom_point(data=dfs2, aes(Temp, min_b))


#---- Plot for ms:
# Curves are matched in the bistable region by linetype. When predator are extinct, the consumer population is dominated by adults

dev.off()

setwd("//storage-og.slu.se/home$/mxli0002/Desktop")

#pdf("SampleGraph.pdf", useDingbats=FALSE)

op <- par(mfrow=c(2,4), 
          cex.axis=1.4, 
          mar=(c(0, 1.2, 0, 1.2)),
          bty="n", 
          las=1, 
          oma=c(15, 4, 14, 2),
          pty="s")

#x.seq <-  seq(from=14, to=34, by=4)# c(12, 23, 34)          
x.seq <-  seq(from=6, to=36, by=6)# c(12, 23, 34)          
x.seq2 <-  seq(from=6, to=36, by=3)# c(12, 23, 34)          

##---- p=0.5 | R
plot(-1,
     xlim=c(min(x.seq), max(x.seq)),
#     ylim=c(0, 0.025), # these boundaries fit with xmin=14
     ylim=c(-7,2),
     ylab="", xlab="", 
     axes=F, 
     xaxs="i", yaxs="i")
rect(0, -10, hopf_t, 20, border = NA, col= "grey90")
lines(log(R)~tc, col="black", lwd=1.8, data=subset(dat_ty, p==0.5))
points(log(max_b)~tc, pch=16, cex=0.7, 
data=subset(dfs2, state_v == "R")) # add limit cycles min and max
points(log(min_b)~tc, pch=16, cex=0.7, 
data=subset(dfs2, state_v == "R")) # add limit cycles min and max

axis(side=1, at=x.seq, labels=F, tck=-0.03)
axis(side=1, at=x.seq2, labels=F, tck=-0.03)
#axis(side=2, at=c(0,0.025), labels=F)
#axis(side=2, at=c(0.005, 0.015, 0.025), labels=T, cex.axis=1, tck=-0.03)
axis(side=2, at=c(-7,-4,-1,2), labels=T, cex.axis=1, tck=-0.03)
mtext(expression(paste(bold("A"), "   Resource")), side=1, outer=F, line=-10.5, adj=0, cex=0.9)

##---- p=0.5 | J
plot(-1,
     xlim=c(min(x.seq), max(x.seq)),
     ylim=c(0, 4),
     ylab="", xlab="", 
     axes=F, 
     xaxs="i", yaxs="i")
rect(0, -10, hopf_t, 20, border = NA, col= "grey90")
lines(J~tc, col="black", lwd=1.8, data=subset(dat_ty, p==0.5))
points(max_b~tc, pch=16, cex=0.7, 
data=subset(dfs2, state_v == "J")) # add limit cycles min and max
points(min_b~tc, pch=16, cex=0.7, 
data=subset(dfs2, state_v == "J")) # add limit cycles min and max

axis(side=1, at=x.seq, labels=F, tck=-0.03)
axis(side=1, at=x.seq2, labels=F, tck=-0.03)
axis(side=2, at=c(0, 4), labels=F)
axis(side=2, at=c(0, 1, 2, 3, 4), labels=T, cex.axis=1, tck=-0.03)
mtext(expression(paste(bold("B"), "   Juvenile")), side=1, outer=F, line=-10.5, adj=0, cex=0.9)

##---- p=0.5 | A
plot(-1,
     xlim=c(min(x.seq), max(x.seq)),
     ylim=c(0, 2),
     ylab="", xlab="", 
     axes=F, 
     xaxs="i", yaxs="i")
rect(0, -10, hopf_t, 20, border = NA, col= "grey90")
lines(A~tc, col="black", lwd=1.8, data=subset(dat_ty, p==0.5))
points(max_b~tc, pch=16, cex=0.7, 
data=subset(dfs2, state_v == "A")) # add limit cycles min and max
points(min_b~tc, pch=16, cex=0.7, 
data=subset(dfs2, state_v == "A")) # add limit cycles min and max

axis(side=1, at=x.seq, labels=F, tck=-0.03)
axis(side=1, at=x.seq2, labels=F, tck=-0.03)
axis(side=2, at=c(0, 2), labels=F)
axis(side=2, at=c(0, 1, 2), labels=T, cex.axis=1, tck=-0.03)
mtext(expression(paste(bold("C"), "   Adult")), side=1, outer=F, line=-10.5, adj=0, cex=0.9)

##---- p=0.5 | P
plot(-1,
     xlim=c(min(x.seq), max(x.seq)),
#     ylim=c(0, 4),
     ylim=c(0, 9),     
     ylab="", xlab="", 
     axes=F, 
     xaxs="i", yaxs="i")
rect(0, -10, hopf_t, 20, border = NA, col= "grey90")
lines(P~tc, col="black", lwd=1.8, data=subset(dat_ty, p==0.5))
points(max_b~tc, pch=16, cex=0.7, data=subset(dfs2, state_v == "P")) # add limit cycles min and max
points(min_b~tc, pch=16, cex=0.7, data=subset(dfs2, state_v == "P")) # add limit cycles min and max

axis(side=1, at=x.seq2, labels=F, tck=-0.03)
axis(side=1, at=x.seq, labels=F, tck=-0.03)
axis(side=2, at=c(0, 9), labels=F)
axis(side=2, at=c(0, 3, 6, 9), labels=T, cex.axis=1, tck=-0.03)
mtext(expression(paste(bold("D"), "   Predator")), side=1, outer=F, line=-10.5, adj=0, cex=0.9)

##---- p=1 | R *Note that the data-subset for lines is based on adults, since 1 column=1 state variable
plot(-1, 
     type="l",
     xlim=c(min(x.seq), max(x.seq)),
     ylim=c(-7, 1),
     ylab="", xlab="", 
     axes=F, 
     xaxs="i", yaxs="i")
rect(bp, -10, max(x.seq), 13, border = NA, density=11, col="grey80")
rect(bp, -10, lp, 13, border = NA, col= adjustcolor("#fc8d59", alpha.f = 0.3))

lines(log(R)~tc, col="black", lwd=1.7, 
      data=subset(dat_ty, p==1 & stability==1 & P>pred_lp & tc<=bp))
lines(log(R)~tc, col="black", lwd=1.7, lty=2,  
      data=subset(dat_ty, p==1 & stability==1 & P>pred_lp & tc>=bp))
lines(log(R)~tc, col="black", lwd=1.9,
      data=subset(dat_ty, p==1 & stability==1 & P<pred_lp & tc<=lp & tc>=bp))
lines(log(R)~tc, col="red", lwd=0.8, data=subset(dat_ty, p==1 & stability==0))

axis(side=1, at=x.seq, labels=T, tck=-0.03, cex.axis=1)
axis(side=1, at=x.seq2, labels=F, tck=-0.03)
axis(side=2, at=c(-7,-5,-3,-1,1), labels=T, cex.axis=1, tck=-0.03)
mtext(expression(paste(bold("E"))), side=1, outer=F, line=-10.5, adj=0, cex=0.9)

##---- p=1 | J
plot(-1, 
     type="l", 
     xlim=c(min(x.seq), max(x.seq)),
     ylim=c(0, 0.8),
     ylab="", xlab="", 
     axes=F, 
     xaxs="i", yaxs="i")
rect(bp, 0, max(x.seq), 13, border = NA, density=11, col="grey80")
rect(bp, 0, lp, 13, border = NA, col= adjustcolor("#fc8d59", alpha.f = 0.1))

lines(J~tc, col="black", lwd=1.7, 
      data=subset(dat_ty, p==1 & stability==1 & P>pred_lp & tc<=bp))
lines(J~tc, col="black", lwd=1.7, lty=2,  
      data=subset(dat_ty, p==1 & stability==1 & P>pred_lp & tc>=bp))
lines(J~tc, col="black", lwd=1.9,
      data=subset(dat_ty, p==1 & stability==1 & P<pred_lp & tc<=lp & tc>=bp))
lines(J~tc, col="red", lwd=0.8, data=subset(dat_ty, p==1 & stability==0))

axis(side=1, at=x.seq2, labels=F, tck=-0.03)
axis(side=1, at=x.seq, labels=T, tck=-0.03, cex.axis=1)
axis(side=2, at=c(0, 1), labels=F)
axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8), labels=T, cex.axis=1, tck=-0.03)
mtext(expression(paste(bold("F"))), side=1, outer=F, line=-10.5, adj=0, cex=0.9)

##---- p=1 | A
plot(-1, 
     type="l", 
     xlim=c(min(x.seq), max(x.seq)),
     ylim=c(0, 5),
     ylab="", xlab="", 
     axes=F, 
     xaxs="i", yaxs="i")
rect(bp, 0, max(x.seq), 13, border = NA, density=11, col="grey80")
rect(bp, 0, lp, 13, border = NA, col= adjustcolor("#fc8d59", alpha.f = 0.3))

lines(A~tc, col="black", lwd=1.7, 
      data=subset(dat_ty, p==1 & stability==1 & P>pred_lp & tc<=bp))
lines(A~tc, col="black", lwd=1.7, lty=2,  
      data=subset(dat_ty, p==1 & stability==1 & P>pred_lp & tc>=bp))
lines(A~tc, col="black", lwd=1.9,
      data=subset(dat_ty, p==1 & stability==1 & P<pred_lp & tc<=lp & tc>=bp))
lines(A~tc, col="red", lwd=0.8, data=subset(dat_ty, p==1 & stability==0))

axis(side=1, at=x.seq2, labels=F, tck=-0.03)
axis(side=1, at=x.seq, labels=T, tck=-0.03, cex.axis=1)
axis(side=2, at=c(0, 5), labels=F)
axis(side=2, at=c(0, 1, 2, 3, 4, 5), labels=T, cex.axis=1, tck=-0.03)
mtext(expression(paste(bold("G"))), side=1, outer=F, line=-10.5, adj=0, cex=0.9)


##---- p=1 | P *added a small number to p=0 to make it clear there's a line after bistability
plot(-1, 
     type="l", 
     xlim=c(min(x.seq), max(x.seq)),
#     ylim=c(0, 4),
     ylim=c(0, 5),
     ylab="", xlab="", 
     axes=F, 
     xaxs="i", yaxs="i")
rect(bp, 0, max(x.seq), 13, border = NA, density=11, col="grey80")
rect(bp, 0, lp, 13, border = NA, col= adjustcolor("#fc8d59", alpha.f = 0.3))

lines(P~tc, col="black", lwd=1.7, 
      data=subset(dat_ty, p==1 & stability==1 & P>pred_lp & tc<=bp))
lines(P~tc, col="black", lwd=1.7, lty=2,  
      data=subset(dat_ty, p==1 & stability==1 & P>pred_lp & tc>=bp))
lines(P~tc, col="black", lwd=1.9,
      data=subset(dat_ty, p==1 & stability==1 & P<pred_lp & tc<=lp & tc>=bp))
lines(P~tc, col="red", lwd=0.8, data=subset(dat_ty, p==1 & stability==0))

axis(side=1, at=x.seq2, labels=F, tck=-0.03)
axis(side=1, at=x.seq, labels=T, tck=-0.03, cex.axis=1)
axis(side=2, at=c(0, 4), labels=F)
axis(side=2, at=c(0, 1, 2, 3, 4, 5), labels=T, cex.axis=1, tck=-0.03)
mtext(expression(paste(bold("H"))), side=1, outer=F, line=-10.5, adj=0, cex=0.9)

mtext(expression(paste("Biomass density ", "[", g ~m^{-3}, "]")), las=3, side=2, outer=T, line=1, adj=0.5, cex=1)

mtext(text=expression(paste("Temperature [", degree*C, "]")), 
      side=1, outer=T, line=1.5, adj=0.505, cex=1, las=1)

mtext(expression(paste(italic(p),"=0.5")), side=4, outer=T, line=-0.3, adj=0.8, cex=1, las=0)
mtext(expression(paste(italic(p),"=1")), side=4, outer=T, line=-0.3, adj=0.2, cex=1, las=0)

#dev.off()
