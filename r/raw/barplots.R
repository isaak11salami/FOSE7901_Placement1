#SetUp Fonts
library(extrafont)
fonts()


#Start by setting the working directory
setwd("/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Placements_FOSE7901/
      Placement 1/Deliverables/r/raw")

#Generate clustered bar plots of nodule and bump counts for each plant species 
#(L4, L7 and L8)

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

sampleID <- c("L8-1","L8-2","L8-3","L8+1","L8+2","L8+3","L8+4","L8+5","L8+6","L8+7")
sampleID
L8bar=barplot(L8nod,beside=T,names.arg=sampleID,col= c("red", "orange"),
             xlab= expression(paste(italic("L.rectus")," subjects")), 
             ylab= "No. of observations",ylim=c(0,7), width=0.3, 
             cex.names=0.8, family="Times New Roman")
legend("topright",legend= c("Nodules", "Bumps"),fill= c("red", "orange"),cex= 0.8)
box()




