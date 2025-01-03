# R-Script associated to 

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
##                      Elephant fences result in limited impacts on movement of non-target species
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Original Research Article
# Submitted to KOEDOE 2024-12
# Authors: Timo Jaeger, Trevor McIntyre, Jesse M. Kalwij
# Corresponding author: Jesse M. Kalwij 
# E-mail: jessek@uj.ac.za

# LELE - Lapalala Elephant Landscape Experiment
# Study region: South Africa - Limpopo; Bushveld-Savannah
# Investigation period: 2021-11 to 2022-12

# R-Script
# Orignial Analysis: 2022-12
# Actual Script version: 2025-01
# Author: Timo Jaeger (M.Sc.)
# Karlsruhe Institute of Technology
# timo_jaeger@web.de



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#---------------------------------------------Droppings Analyses at Plot Level---------------------------------------------
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



#-------------------------------------------------------Overview-------------------------------------------------------


# 1. General Data Preparation
#   1.1 Install and Load Packages
#   1.2 Set Working Directory
#   1.3 Load Data
#   1.4 Groups for Analysis
#   1.5 Species for Analysis
#   1.6 Combine Data
#   1.7 Create Count Data
# 2. Visualization
#   2.1 Boxplots
#   2.2 Interaction Plots
# 3. Models
#   3.1 GLMMs
#   3.2 Pairwise Comparison
#   3.3 Tables with Results
#   3.4 Check Assumptions
#     3.4.1 Uniformity and Residuals
#     3.4.2 Overdispersion and Zeroinflation
#     3.4.3 Random Effects





#---------------------------------------------1 General Data Preparation-----------------------------------------------

#--------------------------------------------1.1  Install and Load Packages--------------------------------------------


# if a package is not installed yet, use (with "#" removed):
# install.packages("stringr")

library(stringr) # for str_split_fix
library(glmmTMB) # for negbin
library(xlsx) # write to excel
library(DHARMa) # to check assumptions of GLMM
library(car) # to use qqPlots



#------------------------------------------1.2 Set Working Directory---------------------------------------------------


setwd ("C:/workspace/")
dir()


#------------------------------------------------1.3 Load Data---------------------------------------------------------


# Data must be stored in the working directory!

## Droppings Data
load(file="Droppings.RData")

# The field data was collected with ArcGIS Field Maps, then exported as table
# This exported table is used in this script after some data clearing


load(file="PlotData.RData")



#-------------------------------------------1.4 Groups for Analysis----------------------------------------------------


# >split Droppings Data into subsets depending on the Animal Group<


# Group Overview 
Groupsoverview <- aggregate(IndNumbers ~ AnimalGroup, data = Droppings, FUN = sum, na.rm = TRUE); Groupsoverview

# Groups used for Analysis
Groups <- c("s. Bovids", "m. Bovids", "l. Bovids", "Eland", "Mega-Herbivores", "Pigs", "Monkeys", "Carnivores", "Others")

# Create Lists to store dataframes for Groups
ListDroppings_grp = list()

for (i in Groups) {
  Droppings_grp <- Droppings[Droppings$AnimalGroup == i,]
  
  # save data in list
  NAME <- paste("Droppings",i, sep = "_")
  ListDroppings_grp[[NAME]]<- Droppings_grp
  rm(Droppings_grp)
}


# additional groups (consisting of subgroups)
NAME <- c("Droppings_OtherMammals", "Droppings_ntHerbivores")

# group "Carnivores", "Others", "Monkeys" to "Other Mammals"
ListDroppings_grp[[NAME[1]]]<- Droppings[(Droppings$AnimalGroup %in% c("Carnivores", "Others", "Monkeys")), ]

# all non-target herbivores
ListDroppings_grp[[NAME[2]]]<- Droppings[(Droppings$AnimalGroup %in% c("s. Bovids", "m. Bovids", "l. Bovids", "Eland", "Pigs", "Rhinos")), ]



#-------------------------------------------1.5 Species for Analysis---------------------------------------------------


# >split Droppings Data into subsets depending on the Species<


# Species Overview 
Speciesoverview <- aggregate(IndNumbers ~ Species, data = Droppings, FUN = sum, na.rm = TRUE); Speciesoverview

# Only species with sufficient data, not redundant with groups and relevant for project chosen:
# Duiker, Impala, Kudu, Wildebeest, Giraffe, Elephant, Zebra
Species <- c("Duiker", "Impala", "Kudu", "Wildebeest", "Giraffe", "Elephant", "Zebra")

# Create Lists to store dataframes for Species
ListDroppings_spc = list()

for (i in Species) {
  Droppings_spc <- Droppings[Droppings$Species == i,]
  
  # save data in list
  NAME <- paste("Droppings", i, sep = "_")
  ListDroppings_spc[[NAME]]<- Droppings_spc
  rm(Droppings_spc)
}



#------------------------------------------------1.6 Combine Data------------------------------------------------------


# Combine data for species and groups
ListDroppings_all <- append(ListDroppings_spc, ListDroppings_grp)



#---------------------------------------------1.7 Create Count Data ---------------------------------------------------


# The following is necessary to transform the single mapping events of Droppings to Dung count data!
# It is also necessary to add the 0 counts!



# Study design: 
#       Blocks: 8
#       Plots per Block: 6
#       Plots total: 8x6 = 48
#       Treatments: 3 (full exclosure, mega-herbivore exclosure, control)
#       Plots per Treatment: 48/3 = 16
#       Plots per Treatment and Block: 48/8/3 = 2



# Create Lists to store dataframes
ListallData = list()
i=1
# Create the data for all species/groups and store it in list
for (i in 1:length(ListDroppings_all)) {
  
  Droppings_all <- ListDroppings_all[[i]]
  
  # exclude "cf" (uncertain Droppings) for species (does not matter for groups!)
  if(i %in% c(1:length(ListDroppings_spc))){
    Droppings_all <- Droppings_all[Droppings_all$Uncertainty != "cf",]}
  
  # sum up Droppings per Plot
  Droppings_all <- aggregate(x = Droppings_all$IndNumbers, by = list(Droppings_all$PlotID), 
                          FUN = sum, na.rm = TRUE)
  colnames(Droppings_all) <- c("PlotID", "AnimalNumber")
  
  # merge the two dataframes
  Droppingsanalysis <- merge(PlotData, Droppings_all, all.x = TRUE)
  
  # replace NA's in "IndNumber" column with 0 (except for Block 2)
  Droppingsanalysis[is.na(Droppingsanalysis$AnimalNumber) & Droppingsanalysis$Block != 2, 9] <- 0
  
  # mean of the 48 Blocks
  Droppingsanalysis <- aggregate(x = Droppingsanalysis$AnimalNumber, by = list(Droppingsanalysis$Block, 
                                                                         Droppingsanalysis$Plottype, 
                                                                         Droppingsanalysis$PlotID),
                              FUN = mean, na.rm = FALSE)
  
  colnames(Droppingsanalysis) <- c("Block", "Plottype","PlotID", "AnimalNumber")
  
  # save data in a list
  Split <- str_split_fixed(names(ListDroppings_all)[i], "_",2)[2]
  NAME <- paste("Droppingsanalysis", Split, sep = "_")
  ListallData[[NAME]]<- Droppingsanalysis
  
  rm(i, NAME, Split, Droppings_all, Droppingsanalysis)
}


# NAMES of ListallData
NAMES <- str_split_fixed(names(ListallData), "_",2)[,2] # Names





# delete objects that are not necessary anymore
rm(Groups, Species, Speciesoverview, Groupsoverview)





#----------------------------------------------2 Data Visualization----------------------------------------------------

#-------------------------------------------------2.1 Boxplots --------------------------------------------------------


# >To make data comparable, we need to adjust the data for unequal numbers of different border types (=Treatment)<
# >Then, we can create boxplots<



# Boxplots to get overview of data

# save as pdf (remove "#" in following line)
# pdf(file="Boxplots_Droppings_Plots.pdf",width=16,height=9)
for (i in 1:length(ListallData)){
  
  if(i %in% c(1,10)){par(mfrow=c(3,3))}
  Droppingsanalysis <- ListallData[[i]]
  
  plot(Droppingsanalysis$AnimalNumber ~ Droppingsanalysis$Plottype, ylab = "Number of Droppings", xlab= "Plottype", 
       main = paste(NAMES[i], "Plots", sep =" "))
  stripchart(AnimalNumber ~ Plottype,
             data = Droppingsanalysis,
             method = "stack", # stack to line up points, jitter is more chaotic
             offset = 0.3, # offset gives distance between points
             pch = 19, cex = 1.2,
             col = c("blue", "green", "red"),
             vertical = TRUE,
             add = TRUE)
  
  # summary of data (remove "#" in following lines)
  #print(paste("#######################", NAMES[i], "########## f c mh ##########"))
  #print(summary(Droppingsanalysis[Droppingsanalysis$Plottype == "fullex", 3]))
  #print(summary(Droppingsanalysis[Droppingsanalysis$Plottype == "control", 3]))
  #print(summary(Droppingsanalysis[Droppingsanalysis$Plottype == "mhex", 3]))
  
  rm(Droppingsanalysis)
}
dev.off()



#---------------------------------------------2.2 Interaction Plots ---------------------------------------------------


# >The boxplots do not show if there are general differences between blocks. Therefore, interaction plots can be useful!<


# Animal Number vs Blocks (interaction plot)

# x11(16,9)
# save as pdf (remove "#" in following line)
# pdf(file="Interaction_Droppings_Plots.pdf",width=16,height=9)
for (i in 1:length(ListallData)){
  if(i %in% c(1,10)){par(mfrow=c(3,3))}
  
  Split <- paste(str_split_fixed(names(ListallData)[i], "_",2)[2], "Plots", sep=" ")
  plot(AnimalNumber ~ as.integer(Block), data=ListallData[[i]], type="p", pch = 16, 
       col=ifelse(Plottype == "control", "blue", ifelse(Plottype=="fullex", "green", "red")), 
       ylab = "Number of Droppings", xlab="Block", main = Split)
  legend("topleft",  
         legend = c("mhex", "control", "fullex"),
         col = c("red", "blue", "green"),
         pch = 16)
}
dev.off()





# delete objects that are not necessary anymore
rm(i, Split, ListDroppings_grp, ListDroppings_spc)





#------------------------------------------------------3 Models--------------------------------------------------------

#-----------------------------------------------------3.1 GLMMs--------------------------------------------------------

# >Generalized Linear Mixed-effects Models (GLMM) were used for data analyses with fence type as fixed effect and block as a random variable<

# Model specifications:
#   dependent variable: dropping counts (AnimalNumber)
#   count data and possible overdispersion --> negative binomial error distribution (nbinom2)
#   plot type as fixed effect
#   Block as random variable (random intercept)
#   function used: glmmTMB

# pairwise comparison afterwards to test fence effect (function glht)



# Create lists to save models
ListallglmmTMB <- list()

i=14
# Create Models
for (i in c(1:length(ListallData))){
  Data <- ListallData[[i]]
  
  # Glmm with glmmTMB
  TMB.nb <- glmmTMB(AnimalNumber ~ Plottype +(1|Block), 
                    data = Data, family = "nbinom2")
  
  NAME <- paste("TMB.nb",NAMES[i], sep = "_")
  ListallglmmTMB[[NAME]]<- TMB.nb
  
  rm(Data, TMB.nb, i, NAME)
}


# convergence problems: Monkeys because of few data


# Check model results
i = 3 # can be 1 to 18
names(ListallglmmTMB)[i]; summary(ListallglmmTMB[[i]])



#----------------------------------------------3.2 Pairwise Comparison-------------------------------------------------


# Create list to save test results
ListsummaryGLMMM <- list()

for (i in 1:length(ListallData)){
  TMB.nb <- ListallglmmTMB[[i]]
  
  NAME <- c(paste(NAMES[i], "summary.TMB", sep = "_"), paste(NAMES[i], "pairwise.TMB", sep = "_"))
  ListsummaryGLMMM[[NAME[1]]] <- summary(TMB.nb)
  
  pairwise <- glht(TMB.nb, linfct = mcp(Plottype = "Tukey"), alternative= "two.sided")
  ListsummaryGLMMM[[NAME[2]]] <- summary(pairwise)
  
  rm(TMB.nb, NAME)
}

# Check model and test results
i = 6 # can be 1 to 36
ListsummaryGLMMM[i]



#----------------------------------------3.3 Tables Summarizing Results -----------------------------------------------


# Create a data table as overview of data means and sd

Split <- str_split_fixed(names(ListallData), "_",2)[,2]

# create dataframe
meanstrack <- data.frame(Split)

# add the values
for (i in 1:(length(ListallData))){
  data <- ListallData[[i]]
  agg <- aggregate(data$AnimalNumber, by = list(data$Plottype), FUN=mean, na.rm=TRUE)
  agg2 <- aggregate(data$AnimalNumber, by = list(data$Plottype), FUN=sd, na.rm=TRUE) # sd
  
  meanstrack[i,2] <- agg[1,2]
  meanstrack[i,3] <- agg2[1,2]
  meanstrack[i,4] <- agg[3,2] #fullex
  meanstrack[i,5] <- agg2[3,2]
  meanstrack[i,6] <- agg[3,2]/agg[1,2] * 100 
  meanstrack[i,7] <- agg[2,2] #mhex
  meanstrack[i,8] <- agg2[2,2]
  meanstrack[i,9] <- agg[2,2]/agg[1,2] * 100
  
  rm(data, agg, agg2, i)
}

colnames(meanstrack) <- c("Sp/Gr","control", "control_sd","mhex","mhex_sd" ,"% mhex/co","fullex","fullex_sd" ,"% fu/co")
meanstrack[,2:9] <- round(meanstrack[,2:9], digits = 2); meanstrack

# save as xlsx (remove "#" in following line)
# write.xlsx(meanstrack, file = "dataoverview.xlsx", sheetName="droppings", append=TRUE)





# delete objects that are not necessary anymore
rm(Split)





#----------------------------------------------3.4 Check Assumptions---------------------------------------------------

#----------------------------------------3.4.1 Uniformity and Residuals------------------------------------------------


# DHARMa Package by Florian Hartig (2024):

# "The ‘DHARMa’ package uses a simulation-based approach to create readily interpretable scaled (quantile) residuals for fitted (generalized) linear mixed models
# The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression. The package also provides a number of plot and test functions for typical model misspecification problems, such as over/underdispersion, zero-inflation, and residual spatial and temporal autocorrelation.
# cited from Florian Hartig (2024) https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#poisson-data


# best to test models individually
i = 1 # can be 1 to 18
names(ListallglmmTMB[i])
Data <- ListallData[[i]]
TMB.nb <- ListallglmmTMB[[i]]

# It is highly recommended to first calculate the residuals once, using the simulateResiduals() function
sim_TMB.nb <- simulateResiduals(fittedModel = TMB.nb,plot = F)

par(mfrow=c(1,2))
testUniformity(sim_TMB.nb, plot=T)
testQuantiles(sim_TMB.nb, plot=T) 


# for interpretation see following notes:
# cited from Florian Hartig (2022) https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#poisson-data

# plotQQunif (left panel):a qq-plot to detect overall deviations from the expected distribution
# by default with added tests for correct distribution (KS test), dispersion and outliers.
# plotResiduals (right panel): plot of the residuals against the predicted value (or alternatively, other variable)
# simulation outliers (data points that are outside the range of simulated values) are highlighted as red stars
# outliers interpreted carefully
# under H0 (perfect model), we would expect the boxes to range homogenously from 0.25-0.75
# uniformity per box, and a test for homogeneity of variances between boxes calculated
# A positive test will be highlighted in red
# residuals should be uniform(=flat) for each predictor alone!

# close graphics
dev.off()



#--------------------------------------3.4.2 Overdispersion and Zeroinflation------------------------------------------


# >test for OVERDISPERSION and ZEROINFLATION from DHARMa package for all Species/Groups<


dispzerofunction <- function(s1){
  dispzero <- data.frame(NAMES)
  for (i in 1:length(ListallglmmTMB)){
    # Data and Models
    Data <- ListallData[[i]]
    TMB.nb <- ListallglmmTMB[[i]]
    
    # overdispersion
    dispzero[i,2]<-testDispersion(TMB.nb)$statistic
    dispzero[i,3]<-testDispersion(TMB.nb)$p.value

    # zeroinflation
    dispzero[i,4]<-testZeroInflation(TMB.nb)$statistic
    dispzero[i,5]<-testZeroInflation(TMB.nb)$p.value
  }
  
  colnames(dispzero) <- c("Sp/Gr","rationbTMB","p-TMB","zeroTMB","p-zeroTMB")
  dispzero[,2:5] <- round(dispzero[,2:5], digits = 2); dispzero
  # replace values above 0.05 with NA for better visuality
  dispzero[,c(3,5)][dispzero[,c(3,5)]>0.05] <- NA
  dispzero
}

# remove "#" in following line to check for overdispersion and zeroinflation
# dispzerofunction(s1)
# --> no significant overdispersion; zeroinflation only in few cases but not sever



#-----------------------------------------------3.4.3 Random Effects---------------------------------------------------


# >check the RANDOM EFFECTS for all Groups/Species<


# Comment from Florian Hartig (2022) https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#poisson-data :
# check normality of random effects
# just extract the REs, and then run e.g.a Shapiro test.


Randomeffnormal <- data.frame(NAMES)
for (i in 1:length(ListallglmmTMB)){
  # Data and Models
  Data <- ListallData[[i]]
  TMB.nb <- ListallglmmTMB[[i]]
  
  # qqplots
  par(mfrow=c(1,2))
  qqPlot(ranef(TMB.nb)$cond$Block[,1])

  # shapiro normality test
  Randomeffnormal[i,2] <- shapiro.test(as.numeric(ranef(TMB.nb)$cond$Block[,1]))$p.value
}
colnames(Randomeffnormal) <- c("Sp/Gr","p-TMB")
Randomeffnormal[,2] <- round(Randomeffnormal[,2], digits = 2); Randomeffnormal
# RE not normal for Elephant, Carnivores

dev.off()

