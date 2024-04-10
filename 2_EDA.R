
# HOH table

# Program TIeda.R


# Modified 
#          26 May 2010, J. Riggs, modified for IFD data
#  Created 03 Oct 2020, J. Riggs, from TI code


#  Dataset and file dependences usage.
  
# 1_Import.R
  
#  Imports
#  -------
# Pilot.RData or Sep2020.RData
# Jun2021.RData

#  Exports
#  -------
# 


#########################################################
#     Initialization
#########################################################

# install.packages("remotes")
# library(remotes)
#library(fBasics)
# install.packages("ggplot2")
library(ggplot2)
#library(ggfortify)
# install.packages("reshape2")
library(reshape2)
library(scales)


# library(devtools)
# install_github("spatstat/spatstat.sphere")
# library(spatstat.sphere)
# install.packages("remotes")
# remotes::install_github("spatstat/spatstat.sphere")

Path <- "~/Desktop/SwRI/HOHtable"
setwd(Path)
(WD <- getwd())


#########################################################
# Functions
#########################################################

closest <- function(target,trial) {
	target[which(abs(target-trial)==min(abs(target-trial)))]
	}

FreqProp <- function(factor, factorLabel, dump) {
	table.x <- as.data.frame(table(factor),exclude=c(NA,dump))
	names(table.x)[1] <- c(factorLabel)
	prop <- table.x$Freq / sum(table.x$Freq)
	table.x <- data.frame(table.x, prop)
	sum.x <- colSums(table.x[,2:3])
	new.row <- table.x[1,]
	new.row[1] <- "Total"
	new.row[2:3] <- sum.x
	table.x <- rbind(table.x, new.row)
	}
  
FreqProp2 <- function(factor1, factor2, faclab1, faclab2, dump) {
	table.x <- as.data.frame(table(factor1,factor2),exclude=c(NA,dump))
	names(table.x)[1:2] <- c(faclab1,faclab2)
	prop <- table.x$Freq / sum(table.x$Freq)
	table.x <- data.frame(table.x, prop)
	sum.x <- colSums(table.x[,3:4])
	new.row <- table.x[1,]
	new.row[1:2] <- c("","Total")
	new.row[3:4] <- sum.x
	table.x <- rbind(table.x, new.row)
	}
  
WriteCSV <- function(CSVfileName, RdataSet) {
	outfile <- paste(WD, paste(CSVfileName, ".csv", sep=""), sep="/")
	write.csv(RdataSet, file=outfile, row.names=F)
	}

##########################################################
part <- "Summary"
##########################################################

 (Ex <- "MDAP")
(ver <- "Pilot")    # change to Sep2020
(ver <- "202106jun")
(ver <- "202202feb")
(ver <- "202204apr")
(ver <- "202206jun")

(path <- paste0(Path, "/DataR"))
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
str(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


require(xtable)
(sum <- as.data.frame.matrix(summary(X)))
names(sum) <- gsub(" ", "", names(sum))
ilk <- "1"     # diameter, depth, lat, lon
(main <- paste(ver, "Data", part, "(", ilk, "of 2 )."))
(loc <- paste0(WD,"/Tables/",ver,"/EDA/",part,ilk,".tex"))
print(xtable(sum[c("Diameter", "h", "Lat", "Lon")], digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)

ilk <- "2"     # age, age error low, age error high
(main <- paste(ver, "Data", part, "(", ilk, "of 2 )."))
(loc <- paste0(WD,"/Tables/",ver,"/EDA/",part,ilk,".tex"))
print(xtable(sum[,c("Age", "AgeErrorLower", "AgeErrorUpper")], digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)

# ilk <- "3"
# (main <- paste(ver, "Data", part, "(", ilk, "of 3 )."))
# (loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
# print(xtable(sum[,c(9,10,12,13)], digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)


ilk <- "Type"     # diameter, lat, lon
(main <- paste(ver, "Data", part, "(", ilk, "of 2 )."))
(loc <- paste0(WD,"/Tables/",ver,"/EDA/",part,ilk,".tex"))
(tab <- with(X, FreqProp(substr(Type,1,1), "Type")))    # L = layer, R = radial
print(xtable(tab, digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)

ilk <- "Group"     # diameter, lat, lon
(main <- paste(ver, "Data", part, "(", ilk, "of 2 )."))
(loc <- paste0(WD,"/Tables/",ver,"/EDA/",part,ilk,".tex"))
(tab <- with(X, FreqProp(Group, "Group")))
print(xtable(tab, digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)

ilk <- "GroupXtype"     # diameter, lat, lon
(main <- paste(ver, "Data", part, "(", ilk, "of 2 )."))
(loc <- paste0(WD,"/Tables/",ver,"/EDA/",part,ilk,".tex"))
(tab <- with(X, FreqProp2(Group, substr(Type,1,1), "Group", "Type")))
print(xtable(tab, digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)


##########################################################
part <- "Scatter"
# Scatter plots and pairwise correlations
##########################################################

 (Ex <- "MDAP")
(ver <- "Pilot")    # change to Sep2020
(ver <- "202106jun")
(ver <- "202202feb")
(ver <- "202206jun")

(path <- paste0(Path, "/DataR"))
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
str(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


part <- "XYhistCorrP"
# install.packages("dafs")
# citation("dafs")
require("dafs")
ilk <- ""
(main <- paste(ver,"Data Histograms, \n Scatter Plots, and Pairwise Correlations"))
 (loc <- paste0(WD,"/Plots/",ver,"/EDA/",part,ilk,".png"))
png(loc)
	pairsDAFS(X[c("Diameter","h","Age","AgeErrorLower","AgeErrorUpper")], main=main) 
	dev.off()


require("ggplot2")
   yl <- "Age"
 ylab <- paste(yl, "(Ga)")
   xl <- "Diameter"
 xlab <- paste(xl, "(km)")
part <- "Scatter"
#	
 (loc <- paste0(WD, "/Plots/", ver, "/EDA/", part, xl, yl, ".png"))
(main <- paste(ylab,"vs.",xlab, "by Group Number"))
gp <- ggplot(X) + 
	geom_text(aes(x = Diameter, y = Age, label=Group), nudge_x=0.4, nudge_y=0.05, check_overlap=T) +
	geom_point(aes(x=Diameter, y=Age, color=ejecta)) +
	scale_color_manual(values = c("L" = "blue", "R"="red")) +
	ggtitle(main) +
	xlab(xlab) +
	ylab(ylab) +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=16)) +
	theme(plot.title=element_text(size=20,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92")) +
	theme(legend.position = "bottom")
    ggsave(loc)
gp


##########################################################
# Histograms, Q-Q plots
##########################################################

 (Ex <- "MDAP")
(ver <- "Pilot")    # change to Sep2020
(ver <- "202106jun")
(ver <- "202202feb")
(ver <- "202206jun")

(path <- paste0(Path, "/DataR"))
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
str(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


 part <- "QQ"
  ilk <- "log10Diameter"
    y <- log10(X$Diameter)
 (loc <- paste0(WD, "/Plots/", ver, "/EDA/", part, xl, yl, ".png"))
(main <- paste("log10(Diameter) Normal QQ Plot"))
v = quantile(y[!is.na(y)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
int <- v[1L] - slope * h[1L]
gp <- ggplot(X, aes(sample=y)) + 
	stat_qq(col='grey45') +
	geom_abline(slope = slope, intercept = int) + 
	ggtitle(main) + 
	xlab("Theoretical Quantiles") + 
	ylab("Observed Quantiles") +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=20)) +
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)
gp

    x <- log10(X$Diameter)
 hist <- hist(x,breaks="FD",plot=F)
breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
bwidth <- breaks[2]-breaks[1]
 part <- "Hist"
  ilk <- "log10Diameter"
   xl <- "log10(Diameter)"
(main <- paste("log10(Diameter) Histogram"))
 (loc <- paste0(WD, "/Plots/", ver, "/EDA/", part, xl, yl, ".png"))
gp <- ggplot(X, aes(log10(Diameter))) + 
	geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
	geom_density() +
	ggtitle(main) + 
	xlab(xl) + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=20)) +
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)
gp


 part <- "QQ"
  ilk <- "log10Depth"
    y <- log10(X$h)
 (loc <- paste0(WD, "/Plots/", ver, "/EDA/", part, xl, yl, ".png"))
(main <- paste("log10(Depth) Normal QQ Plot"))
v = quantile(y[!is.na(y)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
int <- v[1L] - slope * h[1L]
gp <- ggplot(X, aes(sample=y)) + 
	stat_qq(col='grey45') +
	geom_abline(slope = slope, intercept = int) + 
	ggtitle(main) + 
	xlab("Theoretical Quantiles") + 
	ylab("Observed Quantiles") +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=20)) +
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)
gp

    x <- log10(X$h)
 hist <- hist(x,breaks="FD",plot=F)
breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
bwidth <- breaks[2]-breaks[1]
 part <- "Hist"
  ilk <- "log10Depth"
   xl <- "log10(Depth)"
(main <- paste("log10(Depth) Histogram"))
 (loc <- paste0(WD, "/Plots/", ver, "/EDA/", part, xl, yl, ".png"))
gp <- ggplot(X, aes(log10(Diameter))) + 
	geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
	geom_density() +
	ggtitle(main) + 
	xlab(xl) + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=20)) +
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)
gp


 part <- "QQ"
  ilk <- "Age"
    y <- X$Age
 (loc <- paste0(WD, "/Plots/", ver, "/EDA/", part, xl, yl, ".png"))
(main <- paste("Age Normal QQ Plot"))
v = quantile(y[!is.na(y)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
int <- v[1L] - slope * h[1L]
gp <- ggplot(X, aes(sample=y)) + 
	stat_qq(col='grey45') +
	geom_abline(slope = slope, intercept = int) + 
	ggtitle(main) + 
	xlab("Theoretical Quantiles") + 
	ylab("Observed Quantiles") +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=20)) +
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)
gp

    x <- X$Age
 hist <- hist(x,breaks="FD",plot=F)
breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
bwidth <- breaks[2]-breaks[1]
 part <- "Hist"
  ilk <- "Age"
   xl <- "Age"
(main <- paste("Age Histogram"))
 (loc <- paste0(WD, "/Plots/", ver, "/EDA/", part, xl, yl, ".png"))
gp <- ggplot(X, aes(Age)) + 
	geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
	geom_density() +
	ggtitle(main) + 
	xlab(xl) + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=20)) +
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)
gp

    x <- log(X$Age)
 hist <- hist(x,breaks="FD",plot=F)
breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
bwidth <- breaks[2]-breaks[1]
 part <- "Hist"
  ilk <- "logAge"
   xl <- "log(Age)"
(main <- paste("log(Age) Histogram"))
 (loc <- paste0(WD, "/Plots/", ver, "/EDA/", part, xl, yl, ".png"))
gp <- ggplot(X, aes(Age)) + 
	geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
	geom_density() +
	ggtitle(main) + 
	xlab(xl) + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=20)) +
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)
gp


#########################################################
	part <- "Boxplots"
#########################################################

 (Ex <- "MDAP")
(ver <- "Pilot")    # change to Sep2020
(ver <- "202106jun")
(ver <- "202206feb")
(ver <- "202206jun")

(path <- paste0(Path, "/DataR"))
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
str(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


# library(lattice)
# y <- X$w
# x <- X$mon
# yl <- "w"
# xl <- "Month"
# (loc <- paste0("Plots/", ver, "/EDA/", part, xl, yl, ".png"))
# quartz(w=7, h=7)
# bwplot(x~y|X$year, main=paste(yl,"vs",xl), xlab=yl, ylab=xl, col="red", horizontal=TRUE)
# quartz.save(loc, type="png")

# age
   yl <- "Age"
 ylab <- paste(yl, "(Ga)")
   xl <- "Ejecta"
 xlab <- paste(xl, "Type")
(loc <- paste0("Plots/", ver, "/EDA/", part, xl, yl, ".png"))
(main <- paste(ylab,"vs.",xlab))
gp <- ggplot(X, aes(x=ejecta, y=Age)) + 
	geom_boxplot(notch=F, outlier.colour='grey45', fill='grey45') +
#	scale_y_continuous(trans=scales::log2_trans()) +
	stat_summary(fun=mean, geom="point", shape=18, size=5) +
	ggtitle(main) + 
	xlab(xlab) + #scale_x_discrete(labels=c("level1",. . . )) +
	ylab(ylab) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)
gp

   yl <- "Age"
 ylab <- paste(yl, "(Ga)")
   xl <- "Group"
 xlab <- paste(xl)
(loc <- paste0("Plots/", ver, "/EDA/", part, xl, yl, ".png"))
(main <- paste(ylab,"vs.",xlab))
gp <- ggplot(X, aes(x=factor(Group,levels=seq(1:max(as.numeric(21)))),y=Age)) + 
	geom_boxplot(notch=F,outlier.colour='grey45',fill='grey45') +
#	scale_y_continuous(trans=scales::log2_trans()) +
	stat_summary(fun=mean, geom="point", shape=18, size=5) +
	ggtitle(main) + 
	xlab(xl) + #scale_x_discrete(labels=c("level1",. . . )) +
	ylab(yl) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)
gp


# diameter
   yl <- "Diameter"
 ylab <- paste(yl, "(km)")
   xl <- "Ejecta"
 xlab <- paste(xl, "Type")
(loc <- paste0("Plots/", ver, "/EDA/", part, xl, yl, ".png"))
(main <- paste(ylab,"vs.",xlab))
gp <- ggplot(X, aes(x=ejecta, y= Diameter)) + 
	geom_boxplot(notch=F, outlier.colour='grey45', fill='grey45') +
#	scale_y_continuous(trans=scales::log2_trans()) +
	stat_summary(fun=mean, geom="point", shape=18, size=5) +
	ggtitle(main) + 
	xlab(xlab) + #scale_x_discrete(labels=c("level1",. . . )) +
	ylab(ylab) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)
gp

   yl <- "Diameter"
 ylab <- paste(yl, "(km)")
   xl <- "Group"
 xlab <- paste(xl)
 (loc <- paste0(WD, "/Plots/", ver, "/", Ex, ver, part, xl, yl, ".png"))
(main <- paste(ylab,"vs.",xlab))
gp <- ggplot(X, aes(x=factor(Group,levels=seq(1:11)),y= Diameter)) + 
	geom_boxplot(notch=F,outlier.colour='grey45',fill='grey45') +
#	scale_y_continuous(trans=scales::log2_trans()) +
	stat_summary(fun=mean, geom="point", shape=18, size=5) +
	ggtitle(main) + 
	xlab(xl) + #scale_x_discrete(labels=c("level1",. . . )) +
	ylab(yl) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)
gp


#########################################################
# part <- "Spatial data matrix"
#########################################################

 (Ex <- "MDAP")
(ver <- "Pilot")    # change to Sep2020
(ver <- "202106jun")
(ver <- "202202feb")
(ver <- "202204apr")
(ver <- "202206jun")

(path <- paste0(Path, "/DataR"))
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
str(X)
setwd(Path)
(WD <- getwd())
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
X <- B
rm(A,B)


# make spatial point process
G$Group <- factor(G$Group)
G$ejecta <- factor(G$ejecta)
G$Big.Bin <- factor(G$Big.Bin)
require(spatstat)
M <- ppp(G$x, G$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Big.Bin=G$Big.Bin,Type=G$ejecta))
# M <- ppp(G$x, G$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Type=G$ejecta))
summary(M)
str(M)

setwd(Path)
(WD <- getwd())
(outfile <- paste0(WD, "/DataR/", ver, "ppp.RData"))
save(M, file=outfile, ascii=FALSE)     # saves as X


#########################################################
	part <- "Quadrat"
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
(infile <- paste0(WD, "/",ver, "ppp.RData"))
	load(infile)
	str(M)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


# box
# 0N to -30S and 340E to 10E (across the meridian) -- or a 30x30 degree region (this is ~1700 km in x-direction and 1780 in y-direction, or 3,011,900  km^2 in area)

(kmpdlon <- 1700 / 30)
(kmpdlat <- 1780 / 30)

(lonkmminmax <- c(-20*kmpdlon,10*kmpdlon) )
(latkmminmax <- c(-1780,0) )


require(spatstat)
qc <- quadratcount(M, xbreaks=seq(lonkmminmax[1],lonkmminmax[2], 1700/15), ybreaks=seq(latkmminmax[1],latkmminmax[2],1780/15))
(main <- paste("Counts of ", ver, "in 30x30 Degree \n ~100km x 100km Quadrants"))
 (ilk <- "")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
	plot(qc, main=main)
	dev.off()


#########################################################
     part <- "Intensity"
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
(infile <- paste0(WD, "/",ver, "ppp.RData"))
	load(infile)
	str(M)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


# box
# 0N to -30S and 340E to 10E (across the meridian) -- or a 30x30 degree region (this is ~1700 km in x-direction and 1780 in y-direction, or 3,011,900  km^2 in area)

(kmpdlon <- 1700 / 30)
(kmpdlat <- 1780 / 30)

(lonkmminmax <- c(-20*kmpdlon,10*kmpdlon) )
(latkmminmax <- c(-1780,0) )


require(spatstat)

def_col <- Kovesi$values[[29]]
def_col_trans <- to.transparent(def_col, fraction = 0.7)
head(def_col_trans)

Z <- density.ppp(M)
(main <- paste("Mean Density of", ver, "Craters/Kilometer"))
 (ilk <- "")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
	plot(Z,main=main, col=def_col_trans, xlab="Longitude", ylab="Latitude")
	plot(M, add = TRUE)
	contour(Z, add = TRUE)
	dev.off()


#########################################################
     part <- "Kest"
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
(infile <- paste0(WD, "/",ver, "ppp.RData"))
	load(infile)
	str(M)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
# X <- H    # restore data
HM <- M    # hold data
# M <- HM    # restore data


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
X <- B
rm(A,B)

# make spatial point process
G$Group <- factor(G$Group)
G$ejecta <- factor(G$ejecta)
X <- G
str(X)

require("spatstat")

K <- Kest(M, correction=c("none"))
#K.bias <- Kest(M, correction="none")
K.env <- envelope(M, fun=Kest, nsim=199, global=F, verbose=F)
kmax <- 20
xx <- c(0, K.env$r, rev(K.env$r),0)
yy <- c(c(0, K.env$lo), rev(c(0, K.env$hi)))
(main <- paste("K-Function for", ver, "\n 95% CR (shaded)"))
 (ilk <- "")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
#	plot(K, xlim=c(0,kmax), xlab="r (degrees)", main=main, lwd=2)  # Kiso
	# plot(K, xlab="Distance (km)", main=main, lwd=2)  # Kiso
	# polygon(xx, yy, col="lightgray")
	# plot(K, lwd=2, add=T, legend=F)
#	plot(K.bias, lwd=1, lty=3, add=T, legend=F)  # uncorrected for border
	#axis(1,seq(0,50,5))
	plot(K.env, xlab="Distance (km)", ylab="K(r)", main=main, shadecol=alpha("salmon",0.5))    #  , ylim=ylim
	grid(col="gray")
	dev.off()


# Subset data from Jun2021 to southern region
H2 <- X
# X <- H2
X <- H2[H2$Lat < -10,]
str(X)

require("spatstat")

Me <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Ejecta=X$ejecta))
M <- Me
K <- Kcross(M, correction="Ripley")
K.env <- envelope(M, fun=Kcross, nsim=199, global=T, verbose=F)

(main <- paste("K-Function for Radial with Layer Reference", ver, "\n 95% CR (shaded)"))
 (ilk <- "CrossRbyL")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(K.env, main=main)
	grid(col="gray")
	dev.off()

Kall <- alltypes(rescale(M), Kcross, envelope=T)

(main <- paste("K-Function for Radial/Layer Pairs\n", ver, "(<-10 deg lat): 95% CR (shaded)"))
 (ilk <- "KcrossAll")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(Kall, main=main)
	grid(col="gray")
	dev.off()

Lall <- alltypes(rescale(M), Lcross, envelope=T)

(main <- paste("K-Function for Radial/Layer Pairs\n", ver, "(<-10 deg lat): 95% CR (shaded)"))
 (ilk <- "LcrossAll")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(Lall, main=main)
	grid(col="gray")
	dev.off()

n <- 60
U <- data.frame(r=head(Lall$fns[[1]][[3]],n=n), obs=head(Lall$fns[[1]][[2]],n=n), theo=head(Lall$fns[[1]][[3]],n=n))
U

#########################################################
     part <- "Lest"
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
(infile <- paste0(WD, "/",ver, "ppp.RData"))
	load(infile)
	str(M)
setwd(Path)
(WD <- getwd())
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
X <- B
rm(A,B)

# make spatial point process
G$Group <- factor(G$Group)
G$ejecta <- factor(G$ejecta)
X <- G
str(X)

# install.packages("spatstat")
require(spatstat)
# install.packages("onpoint")
# remotes::install_github("r-spatialecology/onpoint")
# require(onpoint)
# install.packages("scales")

L <- Lest(M, correction="Ripley")
L.bias <- Lest(M, correction="none")
n <- 199
p  <- 0.05 # Desired p significance level to display
L.env <- envelope(M, Lest, nsim=n, rank=(p*(n+1)), global=F, verbose=F)
L$iso  <- L$iso  - L$r
L$theo <- L$theo - L$r
x <- L$r
y <- L$iso
xx <- c(0, L$r, rev(L$r),0)
yy <- c(c(0, (L.env$lo-L$r)), rev(c(0, (L.env$hi-L$r))))
ylim <- c(round(min(y,yy)), round(max(y,yy)))
(main <- paste("L-Function for", ver, "Data \n 95% CR (shaded)"))
#(sub <- "Solid curve: Iso-corrected. Dotted curve: uncorrected border.")
 (ilk <- "")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
#	OP <- par(mar=c(5,5,4,4))
	# random region
	# plot(L$r, L$iso, ylim=ylim, ty="n", xaxt='n', yaxt='n', ylab="", xlab="")
	# polygon(xx, yy, col=alpha("salmon",0.5))
	# par(new=T)
	# # L function
	plot(L.env, .-r~r, xlab="Distance (km)", ylab="L(r)", main=main, shadecol=alpha("salmon",0.5))    #  , ylim=ylim
	# plot(L$r, L$iso, ylim=ylim, lwd=1, ty="l", xlab="Radial Distance (km)", ylab="L(r)", main=main)#, sub=sub)
	# # zero line (random)
	# lines(L$r, L$theo, lwd=1, lty=2)
	# # theoretical value for a stationary Poisson process
	# lines(L$r, (L.bias$un-L$r), lwd=1, lty=3)
	# text(300, 200, "Solid curve: Iso-corrected", font=2, cex=0.8, pos=2)
	# text(300, 120, "Dotted curve: uncorrected border", font=2, cex=0.8, pos=4)
	# text(100, 20, "Region of complete spatial randomized locations", font=2, cex=0.8, pos=4)
	# cl <- L.env$r[which(abs((L.env$hi-L.env$r)-L$iso)==min(abs((L.env$hi-L.env$r)-L$iso)))]
	# abline(v=cl, col="")
	# points(cl, max(L.env$hi-L.env$r), pch=1, col="red", lwd=3, cex=2)
	# text(cl, max(L.env$hi-L.env$r)+4, paste0("Clustering begins at ~", round(cl), " km radius"), font=2, cex=0.8, pos=4)
	grid(col="gray")
#	par(OP)
	dev.off()


# Subset data from Jun2021 to southern region
H2 <- X
# X <- H2
X <- H2[H2$Lat < -10,]
str(X)

require("spatstat")

Me <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Ejecta=X$ejecta))
M <- Me

Lall <- alltypes(rescale(M), Lcross, envelope=T)

(main <- paste("L-Function for Radial/Layer Pairs\n", ver, "(<-10 deg lat): 95% CR (shaded)"))
 (ilk <- "LcrossAll")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(Lall, main=main)
	grid(col="gray")
	dev.off()

#########################################################
part <- "PCF"    # Pairwise correlation
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
(infile <- paste0(WD, "/",ver, "ppp.RData"))
	load(infile)
	str(M)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


# box
# 0 to -30S and 10E to 340W (across the prime meridian) -- or a 30x30 degree region (~1700 km in longitude and 1780 in latitude, or 3,011,900 km^2 in area)

(kmpdlon <- 1700 / 30)
(kmpdlat <- 1780 / 30)

(lonkmminmax <- c(-20*kmpdlon,10*kmpdlon) )
(latkmminmax <- c(-1780,0) )


require(spatstat)

pcf <- pcf(Kest(M))
(main <- paste("Pairwise Correlation Function for", ver))
 (ilk <- "")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
	plot(pcf)
	grid(col="gray")
	dev.off()


#########################################################
     part <- "Kmarked"
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
(infile <- paste0(WD, "/",ver, "ppp.RData"))
	load(infile)
	str(M)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


# box
# 0N to -30S and 340E to 10E (across the meridian) -- or a 30x30 degree region (this is ~1700 km in x-direction and 1780 in y-direction, or 3,011,900  km^2 in area)

(kmpdlon <- 1700 / 30)
(kmpdlat <- 1780 / 30)

(lonkmminmax <- c(-20*kmpdlon,10*kmpdlon) )
(latkmminmax <- c(-1780,0) )


require(spatstat)

K <- Kmulti(M, I=marks(M)=="L", J=marks(M)=="R", correction="Ripley")
(main <- paste("K-function for Radials with Layers as Reference"))
(ilk <- "Kmulti")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
	plot(K, main=main, lwd=1)
	grid(col="gray")
	dev.off()

pc <- pcf(K)
(main <- paste("Probability of", part, "r degress from", base[b], "Diameters", mm, "km"))
 (ilk <- "pcf")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
	plot(pc, main=main, lwd=1)
	grid(col="gray")
	dev.off()


#########################################################
     part <- "PCFmarked"
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
(infile <- paste0(WD, "/",ver, "ppp.RData"))
	load(infile)
	str(M)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


# box
# 0N to -30S and 340E to 10E (across the meridian) -- or a 30x30 degree region (this is ~1700 km in x-direction and 1780 in y-direction, or 3,011,900  km^2 in area)

(kmpdlon <- 1700 / 30)
(kmpdlat <- 1780 / 30)

(lonkmminmax <- c(-20*kmpdlon,10*kmpdlon) )
(latkmminmax <- c(-1780,0) )


require(spatstat)

pc <- pcf(K)
(main <- paste("Pairwise Correlation Function for", ver))
 (ilk <- "")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
	plot(pc, main=main, lwd=1)
	grid(col="gray")
	dev.off()


#########################################################
     part <- "MarkCorr"
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
(infile <- paste0(WD, "/",ver, "ppp.RData"))
	load(infile)
	str(M)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
# X <- H    # restore data
HM <- M    # hold data
# M <- HM    # restore data


# box
# 0N to -30S and 340E to 10E (across the meridian) -- or a 30x30 degree region (this is ~1700 km in x-direction and 1780 in y-direction, or 3,011,900  km^2 in area)

(kmpdlon <- 1700 / 30)
(kmpdlat <- 1780 / 30)

(lonkmminmax <- c(-20*kmpdlon,10*kmpdlon) )
(latkmminmax <- c(-1780,0) )

# Subset data from Jun2021 to southern region
(X <- H[H$Lat < -10,])

# Convert from degrees to km
A <- data.frame(x=kmpdlon*ifelse(X$Lon>180,X$Lon-360,X$Lon), y=kmpdlat*X$Lat, z=X$h)
summary(A)
B <- X
# X <- B
G <- cbind(X,A)
str(G)
X <- G
rm(A,B,G)

# make spatial point process
X$Group <- factor(X$Group)
X$ejecta <- factor(X$ejecta)
require(spatstat)
M <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Diameter=X$Diameter))
summary(M)
str(M)

M <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Age=X$Age,Diameter=X$Diameter))
mc <- markcorr(M)
(main <- paste("Age (Ga) and Diameter (km) Spatial Correlation\n 95% CR (shaded)"))
 (ilk <- "AgeDia")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(mc,main=main)
	dev.off()

M <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Age=X$Age))
AgeCorr <- envelope(M, markcorr, nsim=39, simulate=expression(rlabel(M)))
(main <- paste("Age (Ga)", ver, "Spatial Correlation\n 95% CR (shaded)"))
 (ilk <- "Age")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(AgeCorr)
	dev.off()

M <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Diameter=X$Diameter))
DiameterCorr <- envelope(M, markcorr, nsim=39, simulate=expression(rlabel(M)))
(main <- paste("Diameter (km)", ver, "Spatial Correlation\n 95% CR (shaded)"))
 (ilk <- "Dia")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(DiameterCorr)
	dev.off()

C <- data.frame(r=AgeCorr$r,lo=AgeCorr$lo,Obs=AgeCorr$obs,hi=AgeCorr$hi)
C[C$Obs>C$hi,]


M <- ppp(X$x[X$ejecta=="L"], X$y[X$ejecta=="L"], lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Age=X$Age[X$ejecta=="L"]))
AgeCorr <- envelope(M, markcorr, nsim=39, simulate=expression(rlabel(M)))
(main <- paste("Age (Ga)", ver, "Spatial Correlation of Layer Ejecta\n 95% CR (shaded)"))
 (ilk <- "AgeLonly")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(AgeCorr)
	dev.off()

M <- ppp(X$x[X$ejecta=="R"], X$y[X$ejecta=="R"], lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Age=X$Age[X$ejecta=="R"]))
AgeCorr <- envelope(M, markcorr, nsim=39, simulate=expression(rlabel(M)))
(main <- paste("Age (Ga)", ver, "Spatial Correlation of Layer Ejecta\n 95% CR (shaded)"))
 (ilk <- "AgeRonly")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(AgeCorr)
	dev.off()

M <- ppp(X$x[X$ejecta=="L"], X$y[X$ejecta=="L"], lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Diameter=X$Diameter[X$ejecta=="L"]))
DiameterCorr <- envelope(M, markcorr, nsim=39, simulate=expression(rlabel(M)))
(main <- paste("Diameter (km)", ver, "Spatial Correlation of Layer Ejecta\n 95% CR (shaded)"))
 (ilk <- "DiaLonly")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(DiameterCorr)
	dev.off()

M <- ppp(X$x[X$ejecta=="R"], X$y[X$ejecta=="R"], lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Diameter=X$Diameter[X$ejecta=="R"]))
DiameterCorr <- envelope(M, markcorr, nsim=39, simulate=expression(rlabel(M)))
(main <- paste("Diameter (km)", ver, "Spatial Correlation of Layer Ejecta\n 95% CR (shaded)"))
 (ilk <- "DiaRonly")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(DiameterCorr)
	dev.off()


#########################################################
     part <- "nn"
#########################################################

 (Ex <- "MDAP")
(ver <- "Pilot")    # change to Sep2020
(ver <- "2021jun")
(ver <- "2022feb")
(ver <- "2022apr")
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
# X <- H    # restore data
HM <- M    # hold data
# M <- HM    # restore data


# box
# 0N to -30S and 340E to 10E (across the meridian) -- or a 30x30 degree region (this is ~1700 km in x-direction and 1780 in y-direction, or 3,011,900  km^2 in area)

(kmpdlon <- 1700 / 30)
(kmpdlat <- 1780 / 30)

(lonkmminmax <- c(-20*kmpdlon,10*kmpdlon) )
(latkmminmax <- c(-1780,0) )

# Subset data from Jun2021 to southern region
(X <- H[H$Lat < -10,])

# Convert from degrees to km
A <- data.frame(x=kmpdlon*ifelse(X$Lon>180,X$Lon-360,X$Lon), y=kmpdlat*X$Lat, z=X$h)
summary(A)
B <- X
# X <- B
G <- cbind(X,A)
str(G)
X <- G
rm(A,B,G)

# make spatial point process
X$Group <- factor(X$Group)
X$ejecta <- factor(X$ejecta)

require(spatstat)

Me <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Ejecta=X$ejecta))
M <- Me

d <- as.data.frame(nndist(M, by=marks(M)))
d <- cbind(X$ejecta,d)
names(d) <- c("Ejecta", "L", "R")


require("fBasics")
(dLLavg <- round(basicStats(d$L[d$Ejecta=="L"]),3) )
(dRRavg <- round(basicStats(d$R[d$Ejecta=="R"]),3) )
(dLRavg <- round(basicStats(d$L[d$Ejecta=="R"]),3) )

(df <- data.frame(dLLavg, dRRavg, dLRavg) )
names(df) <- c("LtoL", "RtoR", "LtoR")
Statistic <- rownames(df)
(df <- cbind(Statistic, df) )

require(xtable)
ilk <- "Summary"
(main <- paste(ver, "Data", part, ilk, "Statistics"))
(loc <- paste0(WD,"/Tables/",ver,"/EDA/",part,ilk,".tex"))
print(xtable(df, digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)



mtc <- marktable(M, N=1, collapse=T)
mtpt <- mtc
mtpt[1,1] <- 100*round(mtc[1,1]/sum(mtc),1)
mtpt[1,2] <- 100*round(mtc[1,2]/sum(mtc),1)
mtpt[2,1] <- 100*round(mtc[2,1]/sum(mtc),1)
mtpt[2,2] <- 100*round(mtc[2,2]/sum(mtc),1)
mtpt    # % of total
mtpm <- mtc
rs <- rowSums(mtc)
mtpm[1,1] <- 100*round(mtc[1,1]/rs[1],1)
mtpm[1,2] <- 100*round(mtc[1,2]/rs[1],1)
mtpm[2,1] <- 100*round(mtc[2,1]/rs[2],1)
mtpm[2,2] <- 100*round(mtc[2,2]/rs[2],1)

mtc     # counts
mtpt    # % of total
mtpm    # % of mark type

require(xtable)
ilk <- "Counts"
(main <- paste(ver, "Data", part, ilk))
(loc <- paste0(WD,"/Tables/",ver,"/EDA/",part,ilk,".tex"))
print(xtable(mtc, digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)

ilk <- "PerTotal"
(main <- paste(ver, "Data", part, "Pecent of Total"))
(loc <- paste0(WD,"/Tables/",ver,"/EDA/",part,ilk,".tex"))
print(xtable(mtpt, digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)

ilk <- "PerRow"
(main <- paste(ver, "Data", part, "Percent by Type"))
(loc <- paste0(WD,"/Tables/",ver,"/EDA/",part,ilk,".tex"))
print(xtable(mtpm, digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)





# unnormalized: nn corr = prob crater and nn same mark
# normalized: 1 = random, << 1 => different marks
(nncor <- nncorr(M))


# mark connect
(mc <- markconnect(M))
(main <- paste("Age (Ga)", ver, "Spatial Correlation\n 95% CR (shaded)"))
 (ilk <- "MarkConnect")
 (loc <- paste0(WD,"/Plots/",ver,"/","EDA/",part,ilk,".png"))
png(loc)
plot(alltypes(M, markconnect))
	dev.off()






# holding this code for possible use
(main <- paste("Age (Ga) and Diameter (km) Spatial Correlation\n 95% CR (shaded)"))
 (ilk <- "AgeDia")
 (loc <- paste0(WD,"/Plots/",ver,"/",Ex,ver,part,ilk,".png"))
png(loc)
plot(mc,main=main)
	dev.off()

M <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Age=X$Age))
AgeCorr <- envelope(M, markcorr, nsim=39, simulate=expression(rlabel(M)))
(main <- paste("Age (Ga)", ver, "Spatial Correlation\n 95% CR (shaded)"))
 (ilk <- "Age")
 (loc <- paste0(WD,"/Plots/",ver,"/",Ex,ver,part,ilk,".png"))
png(loc)
plot(AgeCorr)
	dev.off()

M <- ppp(X$x, X$y, lonkmminmax, latkmminmax, unitname=c("km","km"), marks=data.frame(Diameter=X$Diameter))
DiameterCorr <- envelope(M, markcorr, nsim=39, simulate=expression(rlabel(M)))
(main <- paste("Diameter (km)", ver, "Spatial Correlation\n 95% CR (shaded)"))
 (ilk <- "Dia")
 (loc <- paste0(WD,"/Plots/",ver,"/",Ex,ver,part,ilk,".png"))
png(loc)
plot(DiameterCorr)
	dev.off()

C <- data.frame(r=AgeCorr$r,lo=AgeCorr$lo,Obs=AgeCorr$obs,hi=AgeCorr$hi)
C[C$Obs>C$hi,]


#########################################################
# Parking lot
#########################################################

# Probabilities

a <- P$v
colnames(a) <- sub("-", "M", as.character(P$LonI))
a <- data.frame(LatI=P$LatI,a)
#str(a)

b <- melt(a, id.vars=1, value.name="p")
colnames(b)[2] <- "LonI"
b$LonI <- sub("X","",b$LonI)
b$LonI <- as.numeric(sub("M","-",b$LonI))
b$LonI <- round(b$LonI)
Y <- b
summary(Y)
rm(a,b)

# Merge P to X after forming merge key
X$key <- paste0( "(", as.character(X$LonI), ",", as.character(X$LatI), ")" )
X <- subset(X, select=-c(LonI,LatI,bin2,spl,BLANKET))
Y$key <- paste0( "(", as.character(Y$LonI), ",", as.character(Y$LatI), ")" )
Y <- subset(Y, select=-c(LonI,LatI))
nrow(X)
nrow(Y)
Z <- merge(X, Y, by="key", all=FALSE)
nrow(Z)
summary(Z)
head(Z,20)

W <- aggregate(p ~ CRATERID, data=Z, mean)
summary(W)
nrow(W)

V <- merge(X, W, by="CRATERID", all=FALSE)
summary(V)
nrow(V)
Xv <- X
X <- V


#########################################################
# Intensity estimation
#########################################################

 (Ex <- "MDAP")
(ver <- "Pilot")    # change to Sep2020
(ver <- "Jun2021")

(path <- paste0(Path, "/DataR"))
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
str(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


# box
# 0N to -30S and 340E to 10E (across the meridian) -- or a 30x30 degree region (this is ~1700 km in x-direction and 1780 in y-direction, or 3,011,900  km^2 in area)

(kmpdlon <- 1700 / 30)
(kmpdlat <- 1780 / 30)


# Convert from degrees to km
A <- data.frame(x=kmpdlon*ifelse(X$Lon>180,X$Lon-360,X$Lon), y=kmpdlat*X$Lat, z=X$h)
summary(A)

# b <- box3(c(-20*1700/30,10*1700/30),c(-30*1780/30,0),unitname=c("kilometer","kilometers"))
# str(b)

# X <- with(A, pp3(x,y,z,b,marks=data.frame(Age=H$Group,Ejecta=H$Ejecta)))
# str(X)


# Intensity 2-D
# X <- ppp(H$lonf, H$latf, c(-20,10), c(-30,0), unitname=c("degree","degrees"), marks=data.frame(factor(H$Group),factor(H$Ejecta)))
# summary(X)
# str(X)

B <- X
# X <- B
G <- cbind(X,A)
str(G)
rm(A)

X <- ppp(G$x, G$y, c(-850,850), c(-1780,0), unitname=c("km","km"), marks=data.frame(factor(G$Group),factor(G$Type)))
summary(X)
str(X)


den <- density.ppp(X, weights=X$h, sigma=bw.ppl)

(main <- paste("Density Plot Heat Map and Contour"))
(part <- "Density")
 (ilk <- "ColCon")
 (loc <- paste0(WD,"/Plots/",ver,"/",Ex,ver,part,ilk,".png"))
png(loc)
	plot(den)
	contour(den, add=TRUE)
	dev.off()

(main <- paste("Density Plot Heat Map"))
(part <- "Density")
 (ilk <- "Col")
 (loc <- paste0(WD,"/Plots/",ver,"/",Ex,ver,part,ilk,".png"))
png(loc)
	plot(den)
	#contour(den, add=TRUE)
	dev.off()

(main <- paste("Density Plot with Contours"))
(part <- "Density")
 (ilk <- "Con")
 (loc <- paste0(WD,"/Plots/",ver,"/",Ex,ver,part,ilk,".png"))
png(loc)
	plot(den, col=rgb(0,0,0,0))
	contour(den, add=TRUE)
	# abline(v=seq(-15,5,5))
	# abline(h=seq(-25,-5,5))
	dev.off()


######################################################
