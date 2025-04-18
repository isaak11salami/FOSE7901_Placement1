#Let's conduct an anova to look for differences in average weight between 
#treatment groups
setwd("~/Uni/2025/Session 1/Placements_FOSE7901/Placement 1/Deliverables/r/data/raw")

library(readr)
groupedweights <- read_csv("weightsforaov.csv")
View(groupedweights)
groupedweights


#Let's start with a ONE-Way ANOVA (only consider nodulation group)

nod.model=lm(weight~group, groupedweights)
nod.model

#Test the model for ANOVA assumptions
#1) Homogeneity of Variance, via...
#Levene Test
library(car)

leveneTest(nod.model)
#Plot Residuals against Fitted values
plot(nod.model)
#Look to the Q-Q plot to assess normality of variance 

#Mostly okay, though not perfect. Let's run the ANOVA and see how it looks
anova(nod.model)






