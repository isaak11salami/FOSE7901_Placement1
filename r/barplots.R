getwd()

#SetUp Fonts and establish Times New Roman as the global font
library(extrafont)
fonts()
par(family = "Times New Roman")

#Start by setting the working directory

setwd("~/Uni/2025/Session 1/Placements_FOSE7901/Placement 1/Deliverables/r/data/raw")

#Generate clustered bar plots of nodule and bump counts for each plant species 
#(L4, L7 and L8)

#Start with L.rectus (L8)
library(readr)
L8nod <- read_csv("L8nodules.csv")
View(L8nod)
L8nod
#L8nod is a mix of numeric and non-numeric data. We need to convert to
#numeric data only for it to be recognised by the barplot() function

# Remove the first column (labels) and convert the remaining data to numeric
L8nod=apply(L8nod[, -1], 2, as.numeric)
L8nod

L8nod=matrix(L8nod, nrow = 2, byrow=FALSE)
L8nod
L8ID <- c("L8-1","L8-2","L8-3","L8+1","L8+2","L8+3","L8+4","L8+5","L8+6","L8+7")
L8ID
L8bar=barplot(L8nod,beside=T,names.arg=L8ID,col= c("red", "orange"),
             xlab= expression(paste(italic("L.rectus")," plants")), 
             ylab= "No. of observations",ylim=c(0,7), width=0.3, 
             cex.names=0.8)
legend("topright",legend= c("Nodules", "Bumps"),fill= c("red", "orange"),cex= 0.8)
box()

#Now do L. australis (L4)
library(readr)
L4nod <- read_csv("L4nodules.csv")
View(L4nod)
L4nod
#Convert to numeric data only for it to be recognized by barplot()

#Remove the first column (labels) and convert the remaining data to numeric
L4nod=apply(L4nod[, -1], 2, as.numeric)
L4nod

L4nod=matrix(L4nod, nrow = 2, byrow=FALSE)
L4nod

L4ID <- c("L4-1","L4-2","L4-3","L4+1","L4+2","L4+3","L4+5","L4+6","L4+7")
L4ID
L4bar=barplot(L4nod,beside=T,names.arg=L4ID, col= c("red", "orange"),
              xlab= expression(paste(italic("L.australis")," plants")), 
              ylab= "No. of observations",ylim=c(0,7), width=0.3, 
              cex.names=0.8)
legend("topright",legend= c("Nodules", "Bumps"),fill= c("red", "orange"),cex= 0.8)
box()

#Now do L. uliginosus (L7)
library(readr)
L7nod <- read_csv("L7nodules.csv")
View(L7nod)
L7nod
#Convert to numeric data only for it to be recognized by barplot()

#Remove the first column (labels) and convert the remaining data to numeric
L7nod=apply(L7nod[, -1], 2, as.numeric)
L7nod

L7nod=matrix(L7nod, nrow = 2, byrow=FALSE)
L7nod

L7ID <- c("L7-2","L7-3","L7-4","L7+1","L7+2","L7+3","L7+4","L4+5","L4+6")
L7ID
L7bar=barplot(L7nod,beside=T,names.arg=L7ID, col= c("red", "orange"),
              xlab= expression(paste(italic("L.uliginosus")," plants")), 
              ylab= "No. of observations",ylim=c(0,30), width=0.3, 
              cex.names=0.8)
legend("topright",legend= c("Nodules", "Bumps"),fill= c("red", "orange"),cex= 0.8)
box()

#OKay now let's graphically compare average nodule and bump counts for
#each group
#Start by finding average nodule counts
nod <- read_csv("nodules.csv")
View(nod)
nod

#Calculate the mean nodule and bump counts for each plant species
nodmeans <- tapply(nod$nodules,nod$species,mean)
nodmeans

#Calculate the SE for these nodule counts
nodsp.count <- tapply(nod$nodules,nod$species,length)
nodsp.count
nod.ses <- tapply(nod$nodules,nod$species,sd)/sqrt(nodsp.count)
nod.ses
#95% CI
#Calculate 95%CI
nod.lowerci <- nodmeans - nod.ses*qt(0.025,df=nodsp.count-1)
nod.lowerci
nod.upperci <- nodmeans + nod.ses*qt(0.975,df=nodsp.count-1)
nod.upperci


#Now let's do the same for the bump counts
bump <- read_csv("bumps.csv")
View(bump)
bump

#Calculate the mean nodule and bump counts for each plant species
bumpmeans <- tapply(bump$bumps,bump$species,mean)
bumpmeans

#Calculate the mean nodule and bump counts for each plant species
bumpmeans <- tapply(bump$bumps,bump$species,mean)
bumpmeans

#Calculate the SE for these bump counts
bumpsp.count <- tapply(bump$bumps,bump$species,length)
bumpsp.count
bump.ses <- tapply(bump$bumps,bump$species,sd)/sqrt(bumpsp.count)
bump.ses
#95% CI
#Calculate 95%CI
bump.lowerci <- bumpmeans - bump.ses*qt(0.025,df=bumpsp.count-1)
bump.lowerci
bump.upperci <- bumpmeans + bump.ses*qt(0.975,df=bumpsp.count-1)
bump.upperci

#Bind our two sets of data into the one matrix
nbmeans = cbind(nodmeans,bumpmeans)
nbmeans

spID=c("Nodules","Bumps")
spID

nbbar=barplot(nbmeans,beside=T,names.arg=spID, col= c("lightgreen", "green3", "darkgreen"),
              xlab= expression(paste("symbiosis indicator")), 
              ylab= "count",ylim=c(0,20), width=0.3, 
              cex.names=0.8)
legend("topright",c(expression(paste(italic("L. australis"))),
                    expression(paste(italic("L. uliginosus"))),
                    expression(paste(italic("L. rectus")))),
       fill=c("lightgreen", "green3", "darkgreen"),cex=0.5)
box()

# Add 95% CI bars for nodules (first 3 bars)
nod.pos = nbbar[1:3]
arrows(nod.pos, nodmeans - nod.ses, nod.pos, nodmeans + nod.ses,
       angle = 90, code = 3, length = 0.1)

# Add 95% CI bars for bumps (last 3 bars)
bump.pos = nbbar[4:6]
arrows(bump.pos, bumpmeans - bump.ses, bump.pos, bumpmeans + bump.ses,
       angle = 90, code = 3, length = 0.1)
       
#Now let's do something similar with the average weights. This will
#require weights to be sorted into 4 separate groups: no nodules, nodules+bumps,
#nodules, bumps only

#No Nodules
nonod <- read_csv("nonod_weights.csv")
View(nonod)
nonod

#Calculate the mean weights
nonod.means <- tapply(nonod$weight,nonod$species,mean)
nonod.means

#Calculate the SE for these weights
nonodsp.count <- tapply(nonod$weight,nonod$species,length)
nonodsp.count
nonod.ses <- tapply(nonod$weight,nonod$species,sd)/sqrt(nonodsp.count)
nonod.ses
#95% CI
#Calculate 95%CI
nonod.lowerci <- nonod.means - nonod.ses*qt(0.025,df=nonodsp.count-1)
nonod.lowerci
nonod.upperci <- nonod.means + nonod.ses*qt(0.975,df=nonodsp.count-1)
nonod.upperci

#Nodules+Bumps
nodbump <- read_csv("nodbump_weights.csv")
View(nodbump)
nodbump

#Calculate the mean weights
nodbump.means <- tapply(nodbump$weight,nodbump$species,mean)
nodbump.means

#Calculate the SE for these weights
nodbumpsp.count <- tapply(nodbump$weight,nodbump$species,length)
nodbumpsp.count
nodbump.ses <- tapply(nodbump$weight,nodbump$species,sd)/sqrt(nodbumpsp.count)
nodbump.ses
#95% CI
#Calculate 95%CI
nodbump.lowerci <- nodbump.means - nodbump.ses*qt(0.025,df=nodbumpsp.count-1)
nodbump.lowerci
nodbump.upperci <- nodbump.means + nodbump.ses*qt(0.975,df=nodbumpsp.count-1)
nodbump.upperci

#Nodule-presenting plants
nodpl <- read_csv("nod_weights.csv")
View(nodpl)
nodpl

#Calculate the mean weights
nodpl.means <- tapply(nodpl$weight,nodpl$species,mean)
nodpl.means

#Calculate the SE for these weights
nodplsp.count <- tapply(nodpl$weight,nodpl$species,length)
nodplsp.count
nodpl.ses <- tapply(nodpl$weight,nodpl$species,sd)/sqrt(nodplsp.count)
nodpl.ses
#95% CI
#Calculate 95%CI
nodpl.lowerci <- nodpl.means - nodpl.ses*qt(0.025,df=nodplsp.count-1)
nodpl.lowerci
nodpl.upperci <- nodpl.means + nodpl.ses*qt(0.975,df=nodplsp.count-1)
nodpl.upperci

#Bumps only
bumpo <- read_csv("bumponly_weights.csv")
View(bumpo)
bumpo

#Calculate the mean weights
bumpo.means <- tapply(bumpo$weight,bumpo$species,mean)
bumpo.means

#Calculate the SE for these weights
bumposp.count <- tapply(bumpo$weight,bumpo$species,length)
bumposp.count
bumpo.ses <- tapply(bumpo$weight,bumpo$species,sd)/sqrt(bumposp.count)
bumpo.ses
#95% CI
#Calculate 95%CI
bumpo.lowerci <- bumpo.means - bumpo.ses*qt(0.025,df=bumposp.count-1)
bumpo.lowerci
bumpo.upperci <- bumpo.means + bumpo.ses*qt(0.975,df=bumposp.count-1)
bumpo.upperci

#Bind our four sets of data into the one matrix
weight.means = cbind(nonod.means,nodbump.means,nodpl.means,bumpo.means)
weight.means

#Set up the barplot!
wID=c("No Nodules","Nodules &/or Bumps","Nodules","Bumps Only")
wID

wbar=barplot(weight.means,beside=T,names.arg=wID, 
             col= c("lightgreen", "green3", "darkgreen"),
              xlab= expression(paste("nodulation state")), 
              ylab= "dry foliage weight (mg)",ylim=c(0,8), width=0.3, 
              cex.names=0.8)
legend("topright",c(expression(paste(italic("L. australis"))),
                    expression(paste(italic("L. uliginosus"))),
                    expression(paste(italic("L. rectus")))),
       fill=c("lightgreen", "green3", "darkgreen"),cex=0.5)
box()

# Add 95% CI bars for No Nodules (first 3 bars)
nonod.pos = wbar[1:3]
arrows(nonod.pos, nonod.means - nonod.ses, nonod.pos, nonod.means + nonod.ses,
       angle = 90, code = 3, length = 0.1)

# Add 95% CI bars for Nodules+Bumps (next 3 bars)
nodbump.pos = wbar[4:6]
arrows(nodbump.pos, nodbump.means - nodbump.ses, nodbump.pos, nodbump.means
       + nodbump.ses, angle = 90, code = 3, length = 0.1)

# Add 95% CI bars for Nodules (3rd set of 3 bars)
nodpl.pos = wbar[7:9]
arrows(nodpl.pos, nodpl.means - nodpl.ses, nodpl.pos, nodpl.means
       + nodpl.ses, angle = 90, code = 3, length = 0.1)

# Add 95% CI bars for Bumps Only (last set of 3 bars)
bumpo.pos = wbar[10:12]
arrows(bumpo.pos, bumpo.means - bumpo.ses, bumpo.pos, bumpo.means
       + bumpo.ses, angle = 90, code = 3, length = 0.1)

#Of course, there's no CI for L. australis as there was only ONE plant with only
#bumps. Hence, no variation/error in the mean 

