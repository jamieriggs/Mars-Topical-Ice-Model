
# HOH table

# Program TIeda.R
# Each section loads the necessary data so it can be run independently
# Datasets loaded into a dataframe or matrix X and held in dataframe or matrix H


# Modified 
#          26 May 2010, J. Riggs, modified for IFD data
#  Created 03 Oct 2020, J. Riggs, from TI code


#  Dataset and file dependences usage.
  
# 1_Import.R
  
#  Imports
#  -------
# MDAPTES.RData
# MDAPTES4modeling.RData
# MDAPTESDustCoverIndex.RData

#  Exports
#  -------
# MDAPTES.RData


#########################################################
#     Initialization
#########################################################

# install.packages("tidyverse")
library(tidyverse) # wrangling tabular data and plotting
# install.packages("sf")
library(sf) # processing spatial vector data - the easy way
# install.packages("sp")
library(sp) # processing spatial vector data - the way gstat needs it
# install.packages("raster")
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!

# install.packages("gstat")
library(gstat)   # The most popular R-Package for Kriging (imho)
# install.packages("automap")
library(automap) # Automatize some (or all) parts of the gstat-workflow 

# install.packages("patchwork")
library(patchwork)
# install.packages("viridis")
library(viridis)

# install.packages("xtable")
# citation("xtable")
library(xtable)

Path <- "~/Desktop/SwRI/HOHtable"
setwd(Path)
(WD <- getwd())


#########################################################
#     Functions
#########################################################

WriteCSV <- function(CSVfileName, RdataSet) {
	outfile <- paste(WD, paste(CSVfileName, ".csv", sep=""), sep="/")
	write.csv(RdataSet, file=outfile, row.names=F)
	}

# depth in kilometers
#(h <- 0.13*X$D^0.89)

# box
#0N to -30S and 340E to 10E (across the meridian) -- or a 30x30 degree region (this is ~1700 km in x-direction and 1780 in y-direction, or 3,011,900  km^2 in area)

#(kmpdlon <- 1700 / 30)
#(kmpdlat <- 1780 / 30)

# crs
#Mars_2000_(Sphere) ESRI:104971
#As using a non Earth projection for the layer, must check on the No projection
#(or unknown/non-Earth) option for the project map, in Project Properties, CRS tab.

#Do not change the unknown CRS of the layer.


#########################################################
part <- "load"
#########################################################

 (Ex <- "MDAP")
(ver <- "Pilot")    # change to Sep2020
(ver <- "2021jun")
(ver <- "2022feb")
(ver <- "202204apr")
(ver <- "202206jun")

(path <- paste0(Path, "/DataR"))
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
	load(infile)
	str(X)
(infile <- paste0(WD, "/",ver, "ppp.RData"))
	load(infile)
	str(M)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


(kmpdlon <- 1700 / 30)
(kmpdlat <- 1780 / 30)

(lonkmminmax <- c(-20*kmpdlon,10*kmpdlon) )
(latkmminmax <- c(-1780,0) )


# Convert from degrees to km
A <- data.frame(x=kmpdlon*ifelse(X$Lon>180,X$Lon-360,X$Lon), y=kmpdlat*X$Lat, z=X$h)
summary(A)

B <- X
# X <- B
G <- cbind(X,A)
str(G)
X <- B
rm(A,B)

# make spatial point process
G$Big.Bin <- factor(G$Big.Bin)
G$ejecta <- factor(G$ejecta)
X <- G
str(X)

# Colors
layerColor <- c("blue", "red")
  ageColor <- c("darkorchid","green")
 corrColor <- "darkorange"


# print(citation("spatstat"), bibtex=TRUE)
require("spatstat")
X$Tri.Bin <- factor(ifelse(X$Age>3.4,"O",ifelse(X$Age<=1.2,"Y","M")),levels=c("O","M","Y"))
table(X$Tri.Bin)
table(X$Big.Bin)
marks <- factor(ifelse(X$ejecta=="L" & X$Big.Bin=="Y","YL", ifelse(X$ejecta=="L" & X$Big.Bin=="O","OL", ifelse(X$ejecta=="R" & X$Big.Bin=="Y","YR","OR"))))
marks <- factor( ifelse(X$ejecta=="L" & X$Tri.Bin=="O","OL", ifelse(X$ejecta=="L" & X$Tri.Bin=="M","ML", ifelse(X$ejecta=="L" & X$Tri.Bin=="Y","YL", ifelse(X$ejecta=="R" & X$Tri.Bin=="O","OR", ifelse(X$ejecta=="R" & X$Tri.Bin=="M","MR","YR"))))) )

Mae <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(marks))
Mae <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"))
str(Mae)

M <- Mae
X$nnd <- nndist(M)
summary(X)

# X$Big.Bin <- factor(ifelse(X$Big.Bin=="> 3.4","O","Y"))
# X$SubReg <- factor(ifelse(X$Lat>-10,"N","S"))
# table(X$SubReg)
# str(X)
# summary(X)

dd <- dist(X[,c("x","y")])


#########################################################
	part <- "ETL"
#########################################################

 (Ex <- "MDAP")
(ver <- "Pilot")    # change to Sep2020
(ver <- "Jun2021")
(ver <- "Feb2022")
(ver <- "202206jun")

(path <- paste0(Path, "/DataR"))
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
str(X)
setwd(Path)
(WD <- getwd())

# as per Kirchoff remove craters with diameters > 10km (25 May 2023)
summary(X);nrow(X)
X <- X[X$Diameter<=10,]
summary(X);nrow(X)
H <- X    # hold data
#X <- H    # restore data



# box
# 0N to -30S and 340E to 10E (across the meridian) -- or a 30x30 degree region (this is ~1700 km in x-direction and 1780 in y-direction, or 3,011,900  km^2 in area)

(kmpdlon <- 1700 / 30)
(kmpdlat <- 1780 / 30)

(lonkmminmax <- c(-20*kmpdlon,10*kmpdlon) )
(latkmminmax <- c(-1780,0) )


# Convert from degrees to km
A <- data.frame(x=kmpdlon*ifelse(X$Lon>180,X$Lon-360,X$Lon), y=kmpdlat*X$Lat, z=X$h)
summary(A)

# b <- box3(c(-20*1700/30,10*1700/30),c(-30*1780/30,0),unitname=c("kilometer","kilometers"))
# str(b)

# X <- with(A, pp3(x,y,z,b,marks=data.frame(Age=H$Group,Ejecta=H$Ejecta)))
# str(X)

B <- X
# X <- B
G <- cbind(X,A)
str(G)
X <- G
Hx <- X
# X <- Hx
rm(A,B)
str(X)


# # make spatial point process
# G$Group <- factor(G$Group)
# G$ejecta <- factor(G$ejecta)
# G$Big.Bin <- factor(G$Big.Bin)
# install.packages("spatstat")
# citation("spatstat")
require("spatstat")
# M <- ppp(G$x, G$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Group=G$Group,Big.Bin=G$Big.Bin,Type=G$ejecta))
# summary(M)
# str(M)

Mg <- ppp(G$x, G$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Group=G$Group))
summary(Mg)
str(Mg)


#########################################################
     part <- "Adjust"
#########################################################

# Adjust crater-to-crater distances by removing group-to-group distances

# M <- Mg
# Y <- data.frame(group=X$Group, as.data.frame(nndist(M, by=as.factor(marks(M)))) )
Y <- data.frame(group=as.numeric(X$Group), nndist(X[,c("x","y")], by=as.factor(X$Group)) )
Y[Y == Inf]  <- 0
str(Y)

A <- B <- data.frame(Group=factor(), dist=numeric())
i <- 1
A <- data.frame(Group=as.character(i), dist=Y[Y$group==i, paste0("X",as.character(i))])
for (i in 2:21) { # i <- 2
	B <- data.frame(Group=as.character(i), dist=Y[Y$group==i, paste0("X",as.character(i))])
	A <- rbind(A,B)
	}
str(A)
str(X)

X <- X[order(X$Group),]
A <- A[order(A$Group),]
B <- cbind(X,dist=A[,2])
str(B)
X <- B
rm(A,B)
str(X)


A <- aggregate(cbind(X$x,X$y), by=list(X$Group), FUN="mean")
names(A) <- c("Group","xbar","ybar")
str(A)
Ha <- A
# A <- aggregate(cbind(X$x,X$y), by=list(X$Group), FUN="sd")
# names(A) <- c("Group","xsd","ysd")
# str(A)

X <- X[order(X$Group),];str(X)
A <- A[order(A$Group),];str(A)
# B <- merge(X, A, by="Group")
library(dplyr)
B <- left_join(X, A, by = "Group")
str(B)

A <- data.frame(xa=B$x-B$xbar, ya=B$y-B$ybar)
str(A)

X <- cbind(X,A)
str(X)

X$Group <- factor(X$Group)
X$ejecta <- factor(X$ejecta)
X$Big.Bin <- factor(X$Big.Bin)
require(spatstat)
xaminmax <- c(min(X$xa),max(X$xa))
yaminmax <- c(min(X$ya),max(X$ya))
# M <- ppp(X$xa, X$ya, xaminmax, yaminmax, unitname=c("km","km"),
# #    marks=data.frame(Big.Bin=X$Big.Bin,Type=X$ejecta)
# #    marks=data.frame(Big.Bin=X$Big.Bin,Type=X$ejecta)
    # marks=data.frame(Type=X$ejecta)
	# )
# summary(M)
# str(M)

dd <- dist(X[,c("xa","ya")])





# G <- B[unique(B$Group),]
Mg <- ppp(G$x, G$y, lonkmminmax, latkmminmax, unitname=c("km","km"))
#Mg <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Group=factor(X$Group)))
summary(Mg)
str(Mg)


#########################################################
     part <- "spaMM"
#########################################################

####################################
# Group analysis

# install.packages("spaMM")
# citation("spaMM")
library("spaMM")
# install.packages("ROI.plugin.glpk")
# citation("ROI.plugin.glpk")
library("ROI.plugin.glpk")

# Group effect
X$S25deg <- ifelse(X$Lat<=-20,1,0)
X$S15deg <- ifelse(X$Lat>-20 & X$Lat<=-10,1,0)
X$S05deg <- ifelse(X$Lat>-10 & X$Lat<=0,1,0)
X$Grp <- ifelse(X$Lat<=-20,"S25",ifelse(X$Lat>-10,"S05","S15"))
summary(X)
table(X$Grp)
m <- fitme(ejecta ~ Group + Matern(1 | x + y),
# m <- fitme(ejecta ~ 1 + Matern(1 | x + y %in% Grp),
			# Matern(as.numeric(Grp) | x + y),
			# Matern(as.numeric(Grp) | x + y),
			data=X, family=binomial)
mg <- m
(msg <- summary(mg))
mdl <- "mdlg"

for (i in 2:length(fixef(m))) { # exclude intercept
	confint(m,attr(fixef(m),"names")[i])
	}

grp <- factor(X$Group, level=1:(length(as.numeric(table(X$Group)))))
sig <- c(9,11,12,16,17,19)
Significant <- factor(ifelse(grp %in% sig, "Yes", "No"))
(ptype <- "GroupPred1")
(loc <- paste0("Plots/",ver,"/",part,"/Layer",mdl,ptype,".png") )
gp <- ggplot(X, aes(x=grp, y=m$fv, color=Significant)) + 
	geom_boxplot() +
	coord_flip() +
	labs(y=" <------ Likely Layer         Likely Radial ------>\nProbability", x="Group Number", color="Significant") +
	ggtitle("Model-Predicted Probability Ejecta is Radial by Group Number") +
	theme(legend.position = c(0.88,0.89)) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)


(ptype <- "GroupPred1")
(loc <- paste0("Plots/",ver,"/",part,"/Layer",mdl,ptype,".png") )
sumdat <- sumdatage
gp <- ggplot(newdat, aes(x=Age, y=ejecta))+#, linetype=Big.Bin, colour= Big.Bin)) +
#	geom_jitter(color="black", size=2) +
#	geom_line(data=sumdat, aes(y=val, group=Big.Bin), size=0.75) +
	geom_line(data=sumdat, aes(y=val), size=0.75) +
	geom_ribbon(data=newdat, aes(ymin=fixefVar_0.025, ymax=fixefVar_0.975), alpha=0.2) +
#	labs(y="Probability Ejecta Radial, not Layer", x="Crater Diameter (km)", color="Age\nCategory") +
	labs(y="       <------ Likely Layer         Likely Radial------>", x="Crater Age (Ga)", color="Age\nCategory") +
	ggtitle("Model-Predicted Probability Ejecta is Radial \n vs. Crater Age (Ga) \n (Gray shaded region is 95% confidence band)") +
#	scale_color_manual(values = c("ejecta" = col)) +
	guides(linetype="none") +
	theme(legend.position = c(0.885,0.30)) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)

 dd <- as.matrix(Y[,-1])
 nu <- m$corrPars[[1]][[1]]
rho <- m$corrPars[[1]][[2]]
 mm <- MaternCorr(dd, nu=nu, rho=rho)
maxd <- 100
plt <- data.frame(x=as.numeric(dd[dd<maxd]), y=as.numeric(mm[dd<maxd]) )
plt <- plt[order(plt$x),]
# p1 <- plt
# p2 <- plt
# plot(p1);points(p2,col="red");grid(col="black")
lo <- loess(y~x, span=0.75, data=plt)
loX <- data.frame(fitted=predict(lo,c(1:20)),x=c(1:20))
summary(loX)
source("R/closest.R")
x95 <- round(loX[loX[,1]==closest(loX[,1],0.95),2])
x90 <- round(loX[loX[,1]==closest(loX[,1],0.90),2])
x80 <- round(loX[loX[,1]==closest(loX[,1],0.80),2])
x50 <- round(loX[loX[,1]==closest(loX[,1],0.50),2])

# intall.packages("ggplot2")
# citation("ggplot2")
require("ggplot2")
(main <- "Spatial Correlation Estimates vs. Group Pairwise Distances")
(ptype <- paste0("NNcorr",descriptor,maxd))
#yl <- "Layer"
ilk <- "Adjusted"
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,"ge5kmGroup.png") )
gp <- ggplot(data=plt[plt$x>3,], aes(x=x, y=y)) +
	xlim(0,maxd) +
	# ylim(0.0,1.0) +
	# annotate(x=x95, y=0.95, geom="text", label="+", size=8, col="red", hjust=1.4 ) +
	# annotate(x=x95, y=0.95, geom="text", label=paste0("(<",as.character(x95)," km, 0.95)"), size=3, vjust=-1, hjust=0.2 ) +

	# annotate(x=x90, y=0.90, geom="text", label="+", size=8, col="red", hjust=0.9 ) +
	# annotate(x=x90, y=0.90, geom="text", label=paste0("(",as.character(x90)," km, 0.90)"), size=3, vjust=-1, hjust=0.2 ) +

	# annotate(x=x80, y=0.80, geom="text", label="+", size=8, col="red", hjust=0.3 ) +
	# annotate(x=x80, y=0.80, geom="text", label=paste0("(>",as.character(x80)," km, 0.80)"), size=3, vjust=-1, hjust=-0.2 ) +

	# annotate(x=x50, y=0.50, geom="text", label="+", size=8, col="red", hjust=0.5 ) +
	# annotate(x=x50, y=0.50, geom="text", label=paste0("(",as.character(x50)," km, 0.50)"), size=3, vjust=-1, hjust=-0.2 ) +
#	geom_point(size=0.8) + geom_line(color="blue") +
	geom_point() +
	stat_smooth(method = "loess", formula = y ~ x, span=0.5, size=0.75, col=corrColor) +
	labs(title=main, y="Estimated correlation", x="Distance between pairs of Groups (km)") +
	# annotate(x=10, y=0.0001, geom="text", label="Loess Smoother (orange curve)\n Black dots are the Matern estimates\n Ordered pairs are (distance, correlation)", size=4, vjust=0, hjust=-0.2 ) +
	annotate(x=25, y=0.00007, geom="text", label="Loess Smoother (orange curve)\n Black dots are the Matern estimates\n Effective spatial correlation is zero", size=4, vjust=0, hjust=-0.2 ) +
	theme(text = element_text(size = 14)) +
	theme(axis.title = element_text(size = 12)) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)

####################################
# All predictors analysis

# install.packages("spaMM")
# citation("spaMM")
library("spaMM")
# install.packages("ROI.plugin.glpk")
# citation("ROI.plugin.glpk")
library("ROI.plugin.glpk")

#m <- fitme(ejecta ~ SubReg + Diameter + Big.Bin + Diameter:Big.Bin + nnd + Diameter:nnd + nnd:Big.Bin + Matern(1 | x + y), data=X, family=binomial)
# m <- fitme(ejecta ~ Diameter + Big.Bin + Diameter:Big.Bin + nnd + Diameter:nnd + nnd:Big.Bin + Matern(1 | x + y), data=X, family=binomial)

#---------------------
# best 2022.04.28 (2022apr data)
#---------------------
#m <- fitme(ejecta ~ SubReg + Diameter + Big.Bin + Diameter:Big.Bin + Matern(1 | x + y), data=X, family=binomial)
# m <- fitme(ejecta ~ Diameter + Big.Bin + Diameter:Big.Bin + Matern(1 | x + y), data=X, family=binomial)
# m <- fitme(ejecta ~ Diameter + Age + nnd + Matern(1 | x + y), data=X, family=binomial)
m <- fitme(ejecta ~ Age + nnd + Matern(1 | x + y), data=X, family=binomial)
m3 <- m  # m <- m3
mdl3 <- "Layerm3"
(ms <- summary(m))
ms3 <- ms
AIC(m)

m <- fitme(ejecta ~ Diameter + Age + Diameter:Age + nnd + Diameter:nnd + nnd:Age + Matern(1 | x + y), data=X, family=binomial)
m <- fitme(ejecta ~ Diameter + Age + nnd + Matern(1 | x + y), data=X, family=binomial)
m1 <- m
mdl1 <- "Layerm1"
(ms <- summary(m))
ms1 <- ms
AIC(m1)


#---------------------
# best 2022.06.13 (202206jun)
#---------------------
# Colors
layerColor <- c("blue", "red")
  ageColor <- c("darkorchid","green")
 corrColor <- "darkorange"

# m <- fitme(ejecta ~ Diameter + Big.Bin + Diameter:Big.Bin + nnd + Diameter:nnd + nnd:Big.Bin + Matern(1 | x + y), data=X, family=binomial)
# m <- fitme(ejecta ~ Diameter + Big.Bin + Diameter:Big.Bin + Matern(1 | xa + ya), data=X, family=binomial)
m <- fitme(ejecta ~ Diameter +
                    Big.Bin +
                    Diameter:Big.Bin + 
                    # dist +
                    # Diameter*dist + 
                    # Big.Bin*dist +
                    Matern(1 | xa + ya),
                    data=X, family=binomial)
m2 <- m
mdl2 <- "Layerm2"
(ms <- summary(m))
ms2 <- ms
AIC(m2)

# m <- fitme(ejecta ~ Diameter + Tri.Bin + Diameter:Tri.Bin + nnd + Diameter:nnd + nnd:Tri.Bin + Matern(1 | x + y), data=X, family=binomial)
m <- fitme(ejecta ~ Diameter + Tri.Bin + Diameter:Tri.Bin + Matern(1 | x + y), data=X, family=binomial)
m3 <- m
mdl3 <- "Layerm3"
(ms <- summary(m))
ms3 <- ms
AIC(m3)

m <- m1
m <- m2
m <- m3
for (i in 2:length(fixef(m))) { # exclude intercept
	confint(m,attr(fixef(m),"names")[i])
	}

dd <- dist(X[,c("xa","ya")])
dd <- as.matrix(Y[,-1])

 nu <- m$corrPars[[1]][[1]]
rho <- m$corrPars[[1]][[2]]
mm <- MaternCorr(dd, nu=nu, rho=rho)
maxd <- 25
plt <- data.frame(x=as.numeric(dd[dd<maxd]), y=as.numeric(mm[dd<maxd]) )
plt <- plt[order(plt$x),]
# plt$x <- ifelse(plt$x==0,1,plt$x)
# plt$y <- ifelse(plt$x==1,0.87,plt$y)
# p1 <- plt
# p2 <- plt
# plot(p1);points(p2,col="red");grid(col="black")
lo <- loess(y~x, span=0.75, data=plt)
loX <- data.frame(fitted=predict(lo,c(1:20)),x=c(1:20))
summary(loX)
# se for 95% CI
loPred <- predict(lo, se=T)
lo2sell <- loPred$fit-2*loPred$se.fit
lo2seul <- loPred$fit+2*loPred$se.fit
lo2se <- cbind(lo2sell, lo2seul)
# labels
source("R/closest.R")
x95 <- round(loX[loX[,1]==closest(loX[,1],0.95),2])
x90 <- round(loX[loX[,1]==closest(loX[,1],0.90),2])
x80 <- round(loX[loX[,1]==closest(loX[,1],0.80),2])
x50 <- round(loX[loX[,1]==closest(loX[,1],0.50),2])

# intall.packages("ggplot2")
# citation("ggplot2")
require("ggplot2")
descriptor <- ""
descriptor <- "Adjusted"
descriptor <- "Adjusted54"
(main <- paste("Spatial Correlation Estimates vs.\n Crater",descriptor,"Pairwise Distances"))
mdl <- mdl2
(ptype <- paste0("NNcorr",descriptor,maxd))
#yl <- "Layer"
ilk <- "Adjusted"
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,".png") )
gp <- ggplot(data=plt, aes(x=x, y=y)) +
	# xlim(0,25) +
	# ylim(0.0,1.0) +
	# annotate(x=x95, y=0.95, geom="text", label="+", size=8, col="red", hjust=1.4 ) +
	# annotate(x=x95, y=0.95, geom="text", label=paste0("(<",as.character(x95)," km, 0.95)"), size=3, vjust=-1, hjust=0.2 ) +

	# annotate(x=x90, y=0.90, geom="text", label="+", size=8, col="red", hjust=0.9 ) +
	# annotate(x=x90, y=0.90, geom="text", label=paste0("(",as.character(x90)," km, 0.90)"), size=3, vjust=-1, hjust=0.2 ) +

	# annotate(x=x80, y=0.80, geom="text", label="+", size=8, col="red", hjust=0.3 ) +
	# annotate(x=x80, y=0.80, geom="text", label=paste0("(>",as.character(x80)," km, 0.80)"), size=3, vjust=-1, hjust=-0.2 ) +

	# annotate(x=x50, y=0.50, geom="text", label="+", size=8, col="red", hjust=0.5 ) +
	# annotate(x=x50, y=0.50, geom="text", label=paste0("(",as.character(x50)," km, 0.50)"), size=3, vjust=-1, hjust=-0.2 ) +
#	geom_point(size=0.8) + geom_line(color="blue") +
	geom_point() +
	stat_smooth(method = "loess", formula = y ~ x, span=0.95, linewidth=0.75, col=corrColor) +
	labs(title=main, y="Estimated correlation", x="Distance between pairs of locations (km)") +
#	annotate(x=10, y=0.25, geom="text", label="Loess Smoother (blue curve)\n Black dots are the Matern estimates\n Ordered pairs are (distance, correlation)", size=4, vjust=0, hjust=-0.2 ) +
	annotate(x=7.5, y=0.25, geom="text", label="Loess Smoother (orange curve)\n Black dots are the Matern estimates\n 95% confidence region (dark gray) loess fit", size=4, vjust=0, hjust=-0.2 ) +
	theme(text = element_text(size = 16)) +
	theme(axis.title = element_text(size = 12)) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)

# install.packages("DHARMa")
# citation("DHARMa")
require("DHARMa")
sims <- simulateResiduals(m)
(ptype <- paste0("ResVqq",descriptor))
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,".png") )
png(loc)
plot(sims)    # lines should match
	dev.off()
(ptype <- paste0("ResVdia",descriptor))
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,".png") )
png(loc)
plotResiduals(sims, X$Diameter, quntreg=T)
	grid(col="gray")
	dev.off()
(ptype <- paste0("ResVage",descriptor))
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,".png") )
png(loc)
plotResiduals(sims, X$Age, quntreg=T)
	grid(col="gray")
	dev.off()
(ptype <- paste0("ResVnnd",descriptor))
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,".png") )
png(loc)
plotResiduals(sims, X$nnd, quntreg=T)
	grid(col="gray")
	dev.off()
(ptype <- paste0("ResVBigBin",descriptor))
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,".png") )
png(loc)
testCategorical(sims, catPred=X$Big.Bin) # tests residuals against a categorical predictor
	dev.off()


# https://stackoverflow.com/questions/50268919/model-evaluation-in-r-with-confusion-matrix
# install.packages("OptimalCutpoints")
# citation("OptimalCutpoints")
library("OptimalCutpoints")
# accuracy measures:
# Sensitivity/Specificity (Se/Sp)
# Predictive Values (Positive Predictive Values/Negative Predictive Values)
# Diagnostic Likelihood Ratios (Positive/Negative)
# Prevalence (False Positive/False Negative)
# Optimal criterion (optimum cut between positive and negative test)

OC <- data.frame(fit=round(m$fv), truth=as.numeric(m$data$ejecta)-1)

optimal.cutpoint.Youden <- optimal.cutpoints(X = fit ~ truth, tag.healthy = 0, 
        methods = "Youden", pop.prev = NULL,  data=OC,
        control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)

(msOC <- summary(optimal.cutpoint.Youden))
plot(optimal.cutpoint.Youden, ylim=c(0,1), xlim=c(0,1))
plot(optimal.cutpoint.Youden)

# install.packages("plotROC")
# citation("plotROC")
library(plotROC)
ggplot(OC, 
       aes(m = truth, d = fit)) + 
    geom_roc(n.cuts=) + coord_equal()

# install.packages("caret")
# citation("caret")
library("caret")
confusionMatrix(data=factor(OC$fit), reference=factor(OC$truth))


####################################

# Colors
layerColor <- c("blue", "red")
  ageColor <- c("darkorchid","green")
 corrColor <- "darkorange"

m <- m2
(mdl <- mdl2)

# diameter and Big.Bin effects
newdat <- expand.grid(x=-600, y=-1800, Diameter=seq(1,30,length.out = 30), Big.Bin=factor(X$Big.Bin) )
newdat <- expand.grid(x=-60, y=-130, Diameter=seq(1,10,length.out = 30), Big.Bin=factor(X$Big.Bin) )
str(newdat)


newdat <- expand.grid(x=-60, y=-130, Diameter=seq(1,10,length.out=30), Big.Bin=factor(X$Big.Bin), dist=seq(30,1800,length.out=30) )
str(newdat)

# age and nnd effects
newdat <- expand.grid(x=-600, y=-1800, Age=seq(0.5,4.5,length.out = 30), nnd=seq(0,130,length.out = 30) )
str(newdat)

newdat$ejecta <- as.numeric(predict(m, newdat, re.form = NA)) # re.form = NA used to remove spatial effects
newdat <- cbind(newdat, get_intervals(m, newdata=newdat, intervals="fixefVar", re.form = NA))

# install.packages("plyr")
# citation ("plyr")
require("plyr")
sumdat <- ddply(newdat,.(Diameter,Big.Bin),summarize, val = mean(ejecta))
# sumdat <- ddply(newdat,.(Age,nnd),summarize, val = mean(ejecta))
sumdatage <- ddply(newdat,.(Age),summarize, val = mean(ejecta))
sumdatnnd <- ddply(newdat,.(nnd),summarize, val = mean(ejecta))
sumdat <- ddply(newdat,.(Diameter,Big.Bin,dist),summarize, val = mean(ejecta))

require("ggplot2")

# Age
fV025a <- loess(fixefVar_0.025 ~ Age, data=newdat)
fV975a <- loess(fixefVar_0.975 ~ Age, data=newdat)
newdat <- cbind(newdat,data.frame(fV025a$fitted,fV975a$fitted))
(ptype <- "AgePred1")
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,".png") )
sumdat <- sumdatage
gp <- ggplot(newdat, aes(x=Age, y=ejecta))+#, linetype=Big.Bin, colour= Big.Bin)) +
#	geom_jitter(color="black", size=2) +
#	geom_line(data=sumdat, aes(y=val, group=Big.Bin), size=0.75) +
	geom_line(data=sumdat, aes(y=val), size=0.75) +
#	geom_ribbon(data=newdat, aes(ymin=fixefVar_0.025, ymax=fixefVar_0.975), alpha=0.2) +
	geom_ribbon(data=newdat, aes(ymin=fV025a.fitted, ymax=fV975a.fitted), alpha=0.2) +
#	labs(y="Probability Ejecta Radial, not Layer", x="Crater Diameter (km)", color="Age\nCategory") +
	labs(y="Probability\n       <------ Likely Layer         Likely Radial----->        ", x="Crater Age (Ga)", color="Age\nCategory") +
	ggtitle("Model-Predicted Probability Ejecta is Radial \n vs. Crater Age (Ga) \n (Gray shaded region is 95% confidence band)") +
#	scale_color_manual(values = c("ejecta" = col)) +
	guides(linetype="none") +
	theme(legend.position = c(0.885,0.30)) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)


# nnd
fV025nnd <- loess(fixefVar_0.025 ~ nnd, data=newdat)
fV975nnd <- loess(fixefVar_0.975 ~ nnd, data=newdat)
newdat <- cbind(newdat,data.frame(fV025nnd$fitted,fV975nnd$fitted))
(ptype <- "nndPred1")
(ilk <- "Adjusted")
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,".png") )
sumdat <- sumdatnnd
gp <- ggplot(newdat, aes(x=nnd, y=ejecta))+#, linetype=Big.Bin, colour= Big.Bin)) +
#	geom_jitter(color="black", size=2) +
#	geom_line(data=sumdat, aes(y=val, group=Big.Bin), size=0.75) +
	geom_line(data=sumdat, aes(y=val), size=0.75) +
#	geom_ribbon(data=newdat, aes(ymin=fixefVar_0.025, ymax=fixefVar_0.975), alpha=0.2) +
	geom_ribbon(data=newdat, aes(ymin=fV025nnd.fitted, ymax=fV975nnd.fitted), alpha=0.2) +
#	labs(y="Probability Ejecta Radial, not Layer", x="Crater Diameter (km)", color="Age\nCategory") +
	labs(y="Probability\n       <------ Likely Layer         Likely Radial----->        ", x="Nearest Neighbor Distance (km)", color="Age\nCategory") +
	ggtitle("Model-Predicted Probability Ejecta is Radial \n vs. Nearest Neighbor Distance (km) \n (Gray shaded region is 95% confidence band)") +
#	scale_color_manual(values = c("ejecta" = col)) +
	guides(linetype="none") +
	theme(legend.position = c(0.885,0.30)) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)

# Diameter and Big.Bin
(ptype <- paste0("DiaBigBinPred1",descriptor))
(ilk <- "Adjusted")
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,".png") )
gp <- ggplot(newdat, aes(x=Diameter, y=ejecta, linetype=Big.Bin, colour=Big.Bin)) +
	scale_color_manual(values=ageColor) +
#	geom_jitter(color="black", size=2) +
	geom_line(data=sumdat, aes(y=val, group=Big.Bin), size=0.75) +
#	geom_ribbon(data=newdat, aes(ymin=fixefVar_0.025, ymax=fixefVar_0.975), alpha=0.2) +
	labs(y="Probability Ejecta Radial vs. Layer\n   <--- Layer       Radial --->", x="Crater Diameter (km)", color="Age\nCategory", title="Model-Predicted Probability Ejecta is Radial Layer \n vs. Crater Diameter (km) \nby Age (O is >3.4 Ga & Y is <=3.4 Ga) with 95% Confidence Regions") +
#	scale_color_manual(values = c("ejecta" = col)) +
	guides(linetype="none") +
	theme(legend.position = c(0.885,0.70)) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)

fV025a <- loess(fixefVar_0.025 ~ Diameter, data=newdat)
fV975a <- loess(fixefVar_0.975 ~ Diameter, data=newdat)
(ptype <- paste0("DiaBigBinPred2",descriptor))
(ilk <- "Adjusted")
(loc <- paste0("Plots/",ver,"/",part,"/",ilk,"/",mdl,ptype,".png") )
# gp <- ggplot(newdat, aes(x=Diameter, y=ejecta, linetype=Big.Bin, colour= Big.Bin)) +
gp <- ggplot(newdat, aes(color=Big.Bin))+ #newdat, aes(x=Diameter), color=Big.Bin) +
	scale_color_manual(values=ageColor) +
	geom_line(data=newdat[newdat$Big.Bin=="O",], aes(x=Diameter, y=ejecta), color=ageColor[1], size=1.75) +
	geom_line(data=newdat[newdat$Diameter<15 & newdat$Big.Bin=="Y",], aes(x=Diameter, y=ejecta), color=ageColor[2], size=1.75) +

	geom_line(data=newdat[newdat$Big.Bin=="O",], aes(x=Diameter, y=fixefVar_0.975), color=ageColor[1], size=1) +
	geom_line(data=newdat[newdat$Big.Bin=="O",], aes(x=Diameter, y=fixefVar_0.025), color=ageColor[1], size=1) +
	geom_line(data=newdat[newdat$Diameter<15 & newdat$Big.Bin=="Y",], aes(x=Diameter, y=fixefVar_0.975), color=ageColor[2], size=1) +
	geom_line(data=newdat[newdat$Diameter<15 & newdat$Big.Bin=="Y",], aes(x=Diameter, y=fixefVar_0.025), color=ageColor[2], size=1) +

	geom_ribbon(data=newdat[newdat$Big.Bin=="O",], aes(x=Diameter, ymin=fixefVar_0.025, ymax=fixefVar_0.975), alpha=0.2) +
	geom_ribbon(data=newdat[newdat$Diameter<15 & newdat$Big.Bin=="Y",], aes(x=Diameter, ymin=fixefVar_0.025, ymax=fixefVar_0.975), alpha=0.2) +
	labs(y="Probability Ejecta Radial vs. Layer\n <--- Layer       Radial --->", x="Crater Diameter (km)", color="Age\nCategory", title="Model-Predicted Probability Ejecta is Radial Layer \n vs. Crater Diameter (km) \nby Age (O is >3.4 Ga & Y is <=3.4 Ga) with 95% Confidence Regions") +
	theme(legend.position = c(0.885,0.85)) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)
	# geom_jitter(color="black", size=2) +
	# geom_line(data=sumdat, aes(y=val, group=Big.Bin)) +

ggplot(df, aes(x = x_variable)) + 
  geom_line(aes(y = line1, color = 'line1')) + 
  geom_line(aes(y = line2, color = 'line2'))

A <- newdat[newdat$Big.Bin=="O",]
A[which.min(abs(A$Diameter-2)),]
A[which.min(abs(A$Diameter-4)),]
A[which.min(abs(A$Diameter-6)),]
A[which.min(abs(A$Diameter-8)),]
A[which.min(abs(A$Diameter-10)),]

A <- newdat[newdat$Big.Bin=="Y",]
A[which.min(abs(A$Diameter-2)),]
A[which.min(abs(A$Diameter-4)),]
A[which.min(abs(A$Diameter-6)),]
A[which.min(abs(A$Diameter-8)),]
A[which.min(abs(A$Diameter-10)),]

####################################
# Summary of predicted nnd

m <- m2
m <- m3
m <- m4

B <- sumdat
pa <- aggregate(data=B, val~Diameter+Big.Bin, mean)
pa <- aggregate(data=B, val~Age, mean)
pa$val <- round(pa$val,2)
pa[pa$val>0.34,]

B <- sumdatnnd
pa <- aggregate(data=B, val~nnd, mean)
pa$val <- round(pa$val,2)
pa[pa$val>=0.48,]
rm(B)

# O
.5*19/.49
.55*21/.54
.6*23/.59
.7*27/.69

# Y
.7*5/.69
.5*6/.52


#########################################################
# Parking lot
#########################################################


#########################################################
