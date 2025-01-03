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
#---------------------------------------------Track Analyses at Plot Level---------------------------------------------
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



#-------------------------------------------------------Overview-------------------------------------------------------


# 1. General Data Preparation
#   1.1 Install and Load Packages
#   1.2 Set Working Directory
#   1.3 Load Data
#   1.4 Groups for Analysis
#   1.5 Species for Analysis
#   1.6 Combine Data
#   1.7 Transect Suitability
#   1.8 Create Count Data
# 2. Visualization
#   2.1 Boxplots
#   2.2 Interaction Plots
# 3. Models
#   3.1 Exclude full exclosures
#   3.2 GLMMs
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

## Track Data
load(file="Tracks.RData")

# The field data was collected with ArcGIS Field Maps, then exported as table
# This exported table is used in this script after some data clearing


## Transect Suitability Data
load(file="TransectSuitability.RData")



#-------------------------------------------1.4 Groups for Analysis----------------------------------------------------


# >split Track Data into subsets depending on the Animal Group<


# Group Overview 
Groupsoverview <- aggregate(IndNumbers ~ AnimalGroup, data = Tracks, FUN = sum, na.rm = TRUE); Groupsoverview

# Groups used for Analysis
Groups <- c("s. Bovids", "m. Bovids", "l. Bovids", "Eland", "Mega-Herbivores", "Pigs", "Monkeys", "Carnivores", "Others")

# Create Lists to store dataframes for Groups
ListTracks_grp = list()

for (i in Groups) {
  Tracks_grp <- Tracks[Tracks$AnimalGroup == i,]
  
  # save data in list
  NAME <- paste("Tracks",i, sep = "_")
  ListTracks_grp[[NAME]]<- Tracks_grp
  rm(Tracks_grp)
}


# additional groups (consisting of subgroups)
NAME <- c("Tracks_OtherMammals", "Tracks_ntHerbivores")

# group "Carnivores", "Others", "Monkeys" to "Other Mammals"
ListTracks_grp[[NAME[1]]]<- Tracks[(Tracks$AnimalGroup %in% c("Carnivores", "Others", "Monkeys")), ]

# all non-target herbivores
ListTracks_grp[[NAME[2]]]<- Tracks[(Tracks$AnimalGroup %in% c("s. Bovids", "m. Bovids", "l. Bovids", "Eland", "Pigs", "Rhinos")), ]



#-------------------------------------------1.5 Species for Analysis---------------------------------------------------


# >split Track Data into subsets depending on the Species<


# Species Overview 
Speciesoverview <- aggregate(IndNumbers ~ Species, data = Tracks, FUN = sum, na.rm = TRUE); Speciesoverview

# Only species with sufficient data, not redundant with groups and relevant for project chosen:
# Duiker, Impala, Kudu, Wildebeest, Giraffe, Elephant, Zebra
Species <- c("Duiker", "Impala", "Kudu", "Wildebeest", "Giraffe", "Elephant", "Zebra")

# Create Lists to store dataframes for Species
ListTracks_spc = list()

for (i in Species) {
  Tracks_spc <- Tracks[Tracks$Species == i,]
  
  # save data in list
  NAME <- paste("Tracks", i, sep = "_")
  ListTracks_spc[[NAME]]<- Tracks_spc
  rm(Tracks_spc)
}



#------------------------------------------------1.6 Combine Data------------------------------------------------------


# Combine data for species and groups
ListTracks_all <- append(ListTracks_spc, ListTracks_grp)



#--------------------------------------------1.7 Transect Suitability--------------------------------------------------


# >create a small dataframe with the block information and "Transect Suitability" (=>TransSuit)<
# This will be necessary for later steps

# calculate mean of identical "BorderAbsolute" (to prevent double counting of borders)
TransSuit <- aggregate(x = TransectSuitability$TransectSuit, 
                       by = list(TransectSuitability$Block,
                                 TransectSuitability$PlotID,
                                 TransectSuitability$Plottype), 
                       FUN = mean, na.rm = TRUE)
colnames(TransSuit) <- c("Block", "PlotID", "Plottype", "TransectSuit")

# exclude fullex Borders as not included in models for tracks
TransSuit <- TransSuit[!TransSuit$Plottype == "fullex",]



#---------------------------------------------1.8 Create Count Data ---------------------------------------------------


# The following is necessary to transform the single mapping events of tracks to Track count data!
# It is also necessary to add the 0 counts!



# Study design: 
#       Blocks: 8
#       Plots per Block: 6
#       Plots total: 8x6 = 48
#       Treatments: 3 (full exclosure, mega-herbivore exclosure, control)
#       Plots per Treatment: 48/3 = 16
#       Plots per Treatment and Block: 48/8/3 = 2

#       Borders per Plot: 4
#       Borders per Block: (10 outer borders) + (7 shared inner borders) = 17 borders
#       Borders total: 8x17 = 136
#       Borders per Treatment: 60 full exclosure borders, 47 mega-herbivore fence borders, 32 open control borders
#       Borders per Treatment and Block: varies depending on the plot position and the neighboring plots!

# inner block borders are shared --> a control plot bordering a full exclosure plot has a full exclosure fence as border!! --> unequal numbers per treatment --> additional border transect-based analyses

## Notes regarding Track direction
# Track direction can be "Ingoing", "Outgoing" and "Other"
# Directions used for Plot-based Analyses are "Ingoing" and "Outgoing". All tracks crossing the fence lines independent of their direction were used for analysis to decrease the probability of missing individuals due to low track visibility. Double counting of the same individual was not considered a problem, as an analysis carried out with only the ingoing tracks showed similar results




# Create Lists to store dataframes
ListallData = list()

# Create the data for all species/groups and store it in list
for (i in 1:length(ListTracks_all)) {
  
  Tracks_all <- ListTracks_all[[i]]
  
  # exclude direction "Other"
  Tracks_all <- Tracks_all[Tracks_all$DirectionTrackName != "Other",]
  
  # exclude "cf" (uncertain tracks) for species (does not matter for groups!)
  if(i %in% c(1:length(ListTracks_spc))){
    Tracks_all <- Tracks_all[Tracks_all$Uncertainty != "cf",]}
  
  # sum up Tracks per Plot
  Tracks_all <- aggregate(x = Tracks_all$IndNumbers, by = list(Tracks_all$PlotID), 
                          FUN = sum, na.rm = TRUE)
  colnames(Tracks_all) <- c("PlotID", "AnimalNumber")
  
  # merge the two dataframes
  Tracksanalysis <- merge(TransectSuitability, Tracks_all, all.x = TRUE)
  
  # replace NA's in "IndNumber" column with 0
  Tracksanalysis[is.na(Tracksanalysis$AnimalNumber), 12] <- 0
  
  # mean of the 48 Blocks
  Tracksanalysis <- aggregate(x = Tracksanalysis$AnimalNumber, by = list(Tracksanalysis$Block, 
                                                                         Tracksanalysis$Plottype, 
                                                                         Tracksanalysis$PlotID),
                              FUN = mean, na.rm = FALSE)
  
  colnames(Tracksanalysis) <- c("Block", "Plottype","PlotID", "AnimalNumber")
  
  # save data in a list
  Split <- str_split_fixed(names(ListTracks_all)[i], "_",2)[2]
  NAME <- paste("Tracksanalysis", Split, sep = "_")
  ListallData[[NAME]]<- Tracksanalysis
  
  rm(i, NAME, Split, Tracks_all, Tracksanalysis)
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
# pdf(file="Boxplots_Tracks_Plots.pdf",width=16,height=9)
for (i in 1:length(ListallData)){
  
  if(i %in% c(1,10)){par(mfrow=c(3,3))}
  Tracksanalysis <- ListallData[[i]]
  
  plot(Tracksanalysis$AnimalNumber ~ Tracksanalysis$Plottype, ylab = "Number of Tracks", xlab= "Plottype", 
       main = paste(NAMES[i], "Plots", sep =" "))
  stripchart(AnimalNumber ~ Plottype,
             data = Tracksanalysis,
             method = "stack", # stack to line up points, jitter is more chaotic
             offset = 0.3, # offset gives distance between points
             pch = 19, cex = 1.2,
             col = c("blue", "green", "red"),
             vertical = TRUE,
             add = TRUE)
  
  # summary of data (remove "#" in following lines)
  #print(paste("#######################", NAMES[i], "########## f c mh ##########"))
  #print(summary(Tracksanalysis[Tracksanalysis$Plottype == "fullex", 3]))
  #print(summary(Tracksanalysis[Tracksanalysis$Plottype == "control", 3]))
  #print(summary(Tracksanalysis[Tracksanalysis$Plottype == "mhex", 3]))
  
  rm(Tracksanalysis)
}
dev.off()



#---------------------------------------------2.2 Interaction Plots ---------------------------------------------------


# >The boxplots do not show if there are general differences between blocks. Therefore, interaction plots can be useful!<


# Animal Number vs Blocks (interaction plot)

# x11(16,9)
# save as pdf (remove "#" in following line)
# pdf(file="Interaction_Tracks_Plots.pdf",width=16,height=9)
for (i in 1:length(ListallData)){
  if(i %in% c(1,10)){par(mfrow=c(3,3))}
  
  Split <- paste(str_split_fixed(names(ListallData)[i], "_",2)[2], "Plots", sep=" ")
  plot(AnimalNumber ~ as.integer(Block), data=ListallData[[i]], type="p", pch = 16, 
       col=ifelse(Plottype == "control", "blue", ifelse(Plottype=="fullex", "green", "red")), 
       ylab = "Number of Tracks", xlab="Block", main = Split)
  legend("topleft",  
         legend = c("mhex", "control", "fullex"),
         col = c("red", "blue", "green"),
         pch = 16)
}
dev.off()





# delete objects that are not necessary anymore
rm(i, Split, ListTracks_grp, ListTracks_spc)





#------------------------------------------------------3 Models--------------------------------------------------------

#-------------------------------------------3.1 Exclude Full exclosures------------------------------------------------


# >For analysis of track data, full exclosure plots were excluded as apart from a few exceptional cases almost always zero tracks were found, precluding any modelling<

# store a version with fullex included for later
ListallDatafull <- ListallData

# exclude "fullex" Borders for analysis
for (i in 1:length(ListallData)){
  ListallData[[i]] <- ListallData[[i]][!ListallData[[i]]$Plottype == "fullex",]
}



#---------------------------------------------------3.2 GLMMs----------------------------------------------------------

# >Generalized Linear Mixed-effects Models (GLMM) were used for data analyses with fence type as fixed effect and block as a random variable<

# Model specifications:
#   dependent variable: track counts (AnimalNumber)
#   count data and possible overdispersion --> negative binomial error distribution (nbinom2)
#   fence type as fixed effect
#   Block as random variable (random intercept)
#   Transect suitability was included as a fixed effect only if it improved model fit.
#   function used: glmmTMB


# Create lists to save models
ListallglmmTMB <- list()
ListallglmmTMBTsuit <- list()


# Create Models
for (i in c(1:length(ListallData))){
  Data <- ListallData[[i]]
  
  # Glmm with glmmTMB
  TMB.nb <- glmmTMB(AnimalNumber ~ Plottype +(1|Block), 
                    data = Data, family = "nbinom2")
  
  NAME <- paste("TMB.nb",NAMES[i], sep = "_")
  ListallglmmTMB[[NAME]]<- TMB.nb
  
  # with Transect Suitability included
  Data <- merge(ListallData[[i]], TransSuit)
  TMB.nb2 <- glmmTMB(AnimalNumber ~ TransectSuit + Plottype + (1|Block), 
                     data = Data, family = "nbinom2")
  summary(TMB.nb2)
  
  # model comparison and selection
  anv <- anova(TMB.nb,TMB.nb2) # model comparison
  if(anv$Pr[2] > 0.05 | is.na(anv$Pr[2])){ # if models not sign different -> use simpler model TMB.nb
    m <- TMB.nb
  }else{ # if sign different --> use the one with lower AIC
    if(anv[rownames(anv) == "TMB.nb",2] < anv[rownames(anv) == "TMB.nb2",2]){ #AIC(m1) < AIC(m2)
      m <- TMB.nb
    }else{
      m <- TMB.nb2
    }
  }
  NAME <- paste("TMBTsuit",NAMES[i], sep = "_")
  ListallglmmTMBTsuit[[NAME]] <- m
  
  rm(anv, Data, m, TMB.nb, TMB.nb2, i, NAME)
}


# convergence problems: Mega-Herbivores (TMB, TMB2)



# Check model results
i = 3 # can be 1 to 18
names(ListallglmmTMB)[i]; summary(ListallglmmTMB[[i]])
names(ListallglmmTMBTsuit)[i]; summary(ListallglmmTMBTsuit[[i]])






#----------------------------------------3.3 Tables Summarizing Results -----------------------------------------------


# Create a data table as overview of p-values
# a pairwise test is not necessary here

pvaluesallmodels <- data.frame(NAMES)
for (i in 1:length(NAMES)){
  TMB.nb <- ListallglmmTMB[[i]]
  TMBTsuit.nb <- ListallglmmTMBTsuit[[i]]
  
  pvaluesallmodels[i,2] <- summary(TMB.nb)$coefficients$cond[rownames(summary(TMB.nb)$coefficients$cond) =="Plottypemhex",4]
  pvaluesallmodels[i,3] <- summary(TMBTsuit.nb)$coefficients$cond[rownames(summary(TMBTsuit.nb)$coefficients$cond) =="Plottypemhex",4]
  
  if("TransectSuit" %in% rownames(summary(TMBTsuit.nb)$coefficients$cond)){ 
    pvaluesallmodels[i,4] <- summary(TMBTsuit.nb)$coefficients$cond[rownames(summary(TMBTsuit.nb)$coefficients$cond) =="TransectSuit",4]
  }else{ 
    pvaluesallmodels[i,4] <- 2
  }
  rm(TMB.nb, TMBTsuit.nb)
}

colnames(pvaluesallmodels) <- c("Sp/Gr","p-TMBnb", "p-TMBbil.nb","p-Tsuit")
pvaluesallmodels[,2:4] <- round(pvaluesallmodels[,2:4], digits = 4); pvaluesallmodels
# p-Tsuit = 2 shows that TransectSuit was not important and thus not included in the model

# save as xlsx (remove "#" in following line)
# write.xlsx(pvaluesallmodels, file = "pvaluesallmodels.xlsx", sheetName="pvalues")


# replace values with sympbols (>0.1 is NA)
pvaluesallmodels[,2:4][pvaluesallmodels[,2:4]<0.001] <- "***"
pvaluesallmodels[,2:4][pvaluesallmodels[,2:4]>0.1 & pvaluesallmodels[,2:4]<1.1] <- NA
pvaluesallmodels[,2:4][pvaluesallmodels[,2:4]>0.05 & pvaluesallmodels[,2:4]<0.1] <- "."
pvaluesallmodels[,2:4][pvaluesallmodels[,2:4]>0.01 & pvaluesallmodels[,2:4]<0.05] <- "*"
pvaluesallmodels[,2:4][pvaluesallmodels[,2:4]>0.001 & pvaluesallmodels[,2:4]<0.01] <- "**"
pvaluesallmodels
# save as xlsx (remove "#" in following line)
# write.xlsx(pvaluesallmodels, file = "pvaluesallmodels.xlsx", sheetName="pvalues_symb", append=TRUE)




# Create a data table as overview of data means and sd

Split <- str_split_fixed(names(ListallDatafull), "_",2)[,2]

# create dataframe
meanstrack <- data.frame(Split)

# add the values
for (i in 1:(length(ListallDatafull))){
  data <- ListallDatafull[[i]]
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
# write.xlsx(meanstrack, file = "dataoverview.xlsx", sheetName="tracksPlots", append=TRUE)





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
names(ListallglmmTMBTsuit[i])
Data <- ListallData[[i]]
TMBTsuit.nb <- ListallglmmTMBTsuit[[i]]

# It is highly recommended to first calculate the residuals once, using the simulateResiduals() function
sim_TMBTsuit.nb <- simulateResiduals(fittedModel = TMBTsuit.nb,plot = F)

par(mfrow=c(1,2))
testUniformity(sim_TMBTsuit.nb, plot=T)
testQuantiles(sim_TMBTsuit.nb, plot=T) 


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
    
    # for the cases where TransectSuit is used in model: (did not work with lists)
    if(i %in% c(4,7,13,16)){
      Data <- merge(Data,TransSuit)
      TMBTsuit.nb <- glmmTMB(AnimalNumber ~  TransectSuit + Plottype + (1|Block), 
                             data = Data, family = "nbinom2")
    }else{
      TMBTsuit.nb <- TMB.nb
    }
    
    # overdispersion
    dispzero[i,2]<-testDispersion(TMB.nb)$statistic
    dispzero[i,3]<-testDispersion(TMB.nb)$p.value
    dispzero[i,4]<-testDispersion(TMBTsuit.nb)$statistic
    dispzero[i,5]<-testDispersion(TMBTsuit.nb)$p.value
    
    # zeroinflation
    dispzero[i,6]<-testZeroInflation(TMB.nb)$statistic
    dispzero[i,7]<-testZeroInflation(TMB.nb)$p.value
    dispzero[i,8]<-testZeroInflation(TMBTsuit.nb)$statistic
    dispzero[i,9]<-testZeroInflation(TMBTsuit.nb)$p.value
    
    
  }
  colnames(dispzero) <- c("Sp/Gr","rationbTMB","p-TMB","rationbTMBTsuit","p-TMBTsuit","zeroTMB","p-zeroTMB","zeroTMBTsuit","p-zeroTMBTsuit")
  dispzero[,2:9] <- round(dispzero[,2:9], digits = 2); dispzero
  # replace values above 0.05 with NA for better visuality
  dispzero[,c(3,5,7,9)][dispzero[,c(3,5,7,9)]>0.05] <- NA
  dispzero
}

# remove "#" in following line to check for overdispersion and zeroinflation
# dispzerofunction(s1)
# --> NO significant dispersion or zeroinflation detected!!



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
  
  # for the cases where TransectSuit is used in model: (did not work with list content)
  if(i %in% c(4,7,13,16)){
    Data <- merge(Data,TransSuit)
    TMBTsuit.nb <- glmmTMB(AnimalNumber ~  TransectSuit + Plottype + (1|Block), 
                           data = Data, family = "nbinom2")
  }else{
    TMBTsuit.nb <- TMB.nb
  }
  
  # qqplots
  par(mfrow=c(1,2))
  qqPlot(ranef(TMB.nb)$cond$Block[,1])
  qqPlot(ranef(TMBTsuit.nb)$cond$Block[,1])
  
  # shapiro normality test
  Randomeffnormal[i,2] <- shapiro.test(as.numeric(ranef(TMB.nb)$cond$Block[,1]))$p.value
  Randomeffnormal[i,3] <- shapiro.test(as.numeric(ranef(TMBTsuit.nb)$cond$Block[,1]))$p.value
}
colnames(Randomeffnormal) <- c("Sp/Gr","p-TMB","p-TMBTsuit")
Randomeffnormal[,2:3] <- round(Randomeffnormal[,2:3], digits = 2); Randomeffnormal
# RE not normal for Elephant, Giraffe, Eland

dev.off()

















