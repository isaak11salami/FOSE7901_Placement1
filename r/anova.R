#Let's conduct an anova to look for differences in average weight between 
#treatment groups
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

#Let's do another ONE-Way, but now sort by species
sp.model=lm(weight~species, groupedweights)
sp.model
#Test the assumptions
#Levene Test
leveneTest(sp.model)
#Plot Residuals against Fitted values
plot(sp.model)
#Look to the Q-Q plot to assess normality of variance 

#Again, just mostly okay. Run the ANOVA anyway
anova(sp.model)




