
# Original Resarch Article

## Testing the Impact of Experimental Mega-Herbivore Fences on the Movement of Large Non-Target Mammalian Species

# Submitted to KOEDOE
# Authors: Timo Jaeger, Trevor McIntyre, Jesse M. Kalwij
# Corresponding author: Jesse M. Kalwij 
# E-mail: jessek@uj.ac.za

# LELE - Lapalala Elephant Landscape Experiment
# Study region: South Africa - Limpopo; Bushveld-Savannah
# Investigation period: 2021-11 to 2022-12

# R-Script
# Orignial Analysis: 2022-12
# Actual Script version: 2023-09
# Author: Timo Jaeger (M.Sc.)
# Karlsruhe Institute of Technology
# timo_jaeger@web.de



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#-----------------------------------------------Analysis of Tracks per Edge--------------------------------------------
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



#-------------------------------------------------------Overview-------------------------------------------------------

# 1. General Data Preparation
#   1.1 Install and Load Packages
#   1.2 Set Working Directory
#   1.3 Load Data
#   1.4 Groups for Analysis
#   1.5 Species for Analysis
#   1.6 Combine Data
#   1.7 Trackability
# 2. Visualization
#   2.1 Create Data
#   2.2 Boxplots
#   2.3 Interaction Plots
# 3. Models
#   3.1 Exclude Fullex
#   3.2 GLMER
#   3.3 Tables with Results
#   3.4 Check Assumptions



#------------------------------------------------1 General Preparation-------------------------------------------------
#--------------------------------------------1.1  Install and Load Packages--------------------------------------------

# if a package is not installed yet, use (remove "#"):
# install.packages("stringr")

library(stringr) # for str_split_fix

library(glmmTMB) # for negbin

library(xlsx) # write to excel

library(DHARMa) # to check assumptions of Glmm

library(car) # to use qqPlots



#------------------------------------------1.2 Set Working Directory---------------------------------------------------


setwd ("C:/Users/tijag/Documents/Studium/Masterarbeit_Suedafrika/Masterarbeit/Publication/R/")
dir()



#------------------------------------------------1.3 Load Data---------------------------------------------------------


# The data is cleared already



#load(file="C:/Users/tijag/Documents/Studium/Masterarbeit_Suedafrika/Masterarbeit/Publication/R/Data/Trackability.RData")
#Trackability
#names(Trackability)[names(Trackability) == "EdgeAbsolut"] <- "EdgeAbsolute"
#Trackability <- Trackability[,-11]
#names(Trackability)[names(Trackability) == "Trackabilitymod"] <- "TrackVis"

# rename
#Trackability$Plottype <- as.character(Trackability$Plottype)
#Trackability$Plottype <- ifelse(Trackability$Plottype == "elefence", "mhex", Trackability$Plottype)
#Trackability$Plottype <- as.factor(Trackability$Plottype)

#Trackability$Fencetype <- as.character(Trackability$Fencetype)
#Trackability$Fencetype <- ifelse(Trackability$Fencetype == "elefence", "mhex", Trackability$Fencetype)
#Trackability$Fencetype <- as.factor(Trackability$Fencetype)

#save(Trackability, file = "Trackability.RData")

load(file="Trackability.RData")




#load(file="C:/Users/tijag/Documents/Studium/Masterarbeit_Suedafrika/Masterarbeit/Publication/R/Data/Tracks.RData")
#Tracks

# rename
#Tracks$Plottype <- as.character(Tracks$Plottype)
#Tracks$Plottype <- ifelse(Tracks$Plottype == "elefence", "mhex", Tracks$Plottype)
#Tracks$Plottype <- as.factor(Tracks$Plottype)


#Tracks$Fencetype <- as.character(Tracks$Fencetype)
#Tracks$Fencetype <- ifelse(Tracks$Fencetype == "elefence", "mhex", Tracks$Fencetype)
#Tracks$Fencetype <- as.factor(Tracks$Fencetype)

#Tracks$AnimalGroup <- as.character(Tracks$AnimalGroup)
#Tracks$AnimalGroup <- ifelse(Tracks$AnimalGroup == "gigantic", "Mega-Herbivores", Tracks$AnimalGroup)
#Tracks$AnimalGroup <- ifelse(Tracks$AnimalGroup == "huge", "Rhinos", Tracks$AnimalGroup)
#Tracks$AnimalGroup <- ifelse(Tracks$AnimalGroup == "medium", "m. Bovids", Tracks$AnimalGroup)
#Tracks$AnimalGroup <- ifelse(Tracks$AnimalGroup == "large", "l. Bovids", Tracks$AnimalGroup)
#Tracks$AnimalGroup <- ifelse(Tracks$AnimalGroup == "small", "s. Bovids", Tracks$AnimalGroup)
#Tracks$AnimalGroup <- ifelse(Tracks$AnimalGroup == "very large", "Eland", Tracks$AnimalGroup)
#Tracks$AnimalGroup <- as.factor(Tracks$AnimalGroup)

#names(Tracks)[names(Tracks) == "EdgeAbsolut"] <- "EdgeAbsolute"

#str(Tracks)
#head(Tracks)

# save(Tracks, file = "Tracks.RData")

load(file="Tracks.RData")



#-------------------------------------------1.4 Groups for Analysis----------------------------------------------------


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



#------------------------------------------------1.7 Trackability------------------------------------------------------


# create a small dataframe with the block information and Track Visibility (=Trackabilityshort)

# calculate mean of identical EdgeAbsolute (to prevent double counting)
Trackabilityshort <- aggregate(x = Trackability$TrackVis, by = list(Trackability$Block, Trackability$EdgeAbsolute,
                                                                    Trackability$Fencetype), 
                               FUN = mean, na.rm = TRUE)
colnames(Trackabilityshort) <- c("Block", "EdgeAbsolute", "Fencetype", "TrackVis")

# add 1 for Fencenumber
Trackabilityshort$Fencenumber <- 1

# sum up data
Trackabilityshort <- aggregate(x = cbind(Trackabilityshort$TrackVis, Trackabilityshort$Fencenumber), 
                               by = list(Trackabilityshort$Block, Trackabilityshort$Fencetype), 
                               FUN = sum, na.rm = TRUE)
colnames(Trackabilityshort) <- c("Block",  "Fencetype", "TrackVis", "Fencenumber")

# mean of TrackVis (per Edge)
Trackabilityshort$TrackVismean <- Trackabilityshort$TrackVis/Trackabilityshort$Fencenumber

# exclude fullex edges as not included in models for tracks
Trackabilityshort <- Trackabilityshort[!Trackabilityshort$Fencetype == "fullex",]



#----------------------------------------------2 Data Visualization----------------------------------------------------

#-------------------------------------------------2.1 Create Data -----------------------------------------------------


# Edge analysis only makes sense for n = 24

# Track direction can be "Inwards", "Outwards" and "Other"
# Directions used for Edges Analysis are Inwards and Outwards (both used as hard to separate for edges inside block)
# For inner edges: Using inward and outward going tracks, animal activity along inner edges needs to be cut in half to prevent double counting!! This is not important for the plot analysis


# Create Lists to store dataframes
ListallData = list()

# Create the data for all species/groups and store it in list
for (i in 1:length(ListTracks_all)) {
  
  Tracks_all <- ListTracks_all[[i]]
  
  # exclude direction "Other"
  Tracks_all <- Tracks_all[Tracks_all$DirectionTrackName != "Other",]
  
  # exclude "cf" (uncertain tracks) for species
  if(i %in% c(1:length(ListTracks_spc))){
      Tracks_all <- Tracks_all[Tracks_all$Uncertainty != "cf",]}
  
  # sum up tracks per edge --> inner edges have all values double!! 
  Tracks_all <- aggregate(IndNumbers ~ EdgeAbsolute, data = Tracks_all, FUN = sum, na.rm = TRUE)
  
  # merge the two dataframes
  Tracksanalysis <- merge(Trackability, Tracks_all, all.x = TRUE)
  
  # replace NA's in AnimalNumber column with 0
  Tracksanalysis[is.na(Tracksanalysis$IndNumbers), 12] <- 0
  
  # delete rows where EdgeAbsolute is double in the data
  Tracksanalysis <- aggregate(x = Tracksanalysis$IndNumbers, by = list(Tracksanalysis$Block, 
                                                                       Tracksanalysis$EdgeAbsolute,
                                                                       Tracksanalysis$Fencetype,
                                                                       Tracksanalysis$EdgeNumber),
                              FUN = mean, na.rm = FALSE)
  colnames(Tracksanalysis) <- c("Block", "EdgeAbsolute", "Fencetype", "EdgeNumber", "AnimalNumber")
  
  # cut values of inner edges in half to prevent double counting
  Tracksanalysis[Tracksanalysis$EdgeNumber %in% c(11:17),5] <- Tracksanalysis[Tracksanalysis$EdgeNumber %in% c(11:17),5]/2
  
  # how many edges for each Block-Fencetype combination?
  Tracksanalysis$Fencenumber <- 1
  
  # calculate sum for each Block and Treatment
  Tracksanalysis <- aggregate(x = cbind(Tracksanalysis$AnimalNumber, Tracksanalysis$Fencenumber),
                              by = list(Tracksanalysis$Block, Tracksanalysis$Fencetype),
                              FUN = sum, na.rm = FALSE)
  colnames(Tracksanalysis) <- c("Block", "Fencetype", "AnimalNumber", "Fencenumber")
  
  # save data in a list
  Split <- str_split_fixed(names(ListTracks_all)[i], "_",2)[2]
  NAME <- paste("Tracksanalysis", Split, sep = "_")
  ListallData[[NAME]]<- Tracksanalysis
  
  rm(i, NAME, Split, Tracks_all, Tracksanalysis)
}


# NAMES of ListallData
NAMES <- str_split_fixed(names(ListallData), "_",2)[,2] # Names



#-------------------------------------------------2.2 Boxplots --------------------------------------------------------

# calculate mean per edge
ListallDatamean <- list()
ListallDatamean <- ListallData

for (i in 1:length(ListallData)){
  ListallDatamean[[i]]$AnimalNumber <- ListallDatamean[[i]]$AnimalNumber/ListallDatamean[[i]]$Fencenumber
}


# Boxplots to get overwiev of data

# save as pdf (remove "#" in following line)
# pdf(file="Boxplots_Edges.pdf",width=16,height=9)
for (i in 1:length(ListallDatamean)){
  
  if(i %in% c(1,10)){par(mfrow=c(3,3))}
  Tracksanalysis <- ListallDatamean[[i]]
  
  plot(Tracksanalysis$AnimalNumber ~ Tracksanalysis$Fencetype, ylab = "Number of Tracks", xlab= "Fencetype", 
       main = paste(NAMES[i], "allEdges", sep =" "))
  stripchart(AnimalNumber ~ Fencetype,
             data = Tracksanalysis,
             method = "stack", # stack to line up points, jitter is more chaotic
             offset = 0.3, # offset gives distance between points
             pch = 19, cex = 1.2,
             col = c("blue", "red", "green"),
             vertical = TRUE,
             add = TRUE)

  # summary of data (remove "#" in following lines)
  #print(paste("#######################", NAMES[i], "########## f c mh ##########"))
  #print(summary(Tracksanalysis[Tracksanalysis$Fencetype == "fullex", 3]))
  #print(summary(Tracksanalysis[Tracksanalysis$Fencetype == "control", 3]))
  #print(summary(Tracksanalysis[Tracksanalysis$Fencetype == "mhex", 3]))
  
  rm(Tracksanalysis)
}
dev.off()



#---------------------------------------------2.3 Interaction Plots ---------------------------------------------------


# Animal Number vs Blocks (interaction plot)

# x11(16,9)
# save as pdf (remove "#" in following line)
# pdf(file="Interaction_Edges.pdf",width=16,height=9)
for (i in 1:length(ListallDatamean)){
  if(i %in% c(1,10)){par(mfrow=c(3,3))}
  
  Split <- paste(str_split_fixed(names(ListallDatamean)[i], "_",2)[2], "allEdges", sep=" ")
  plot(AnimalNumber ~ as.integer(Block), data=ListallDatamean[[i]], type="p", pch = 16, 
       col=ifelse(Fencetype == "control", "blue", ifelse(Fencetype=="fullex", "green", "red")), 
       ylab = "Number of Tracks", xlab="Block", main = Split)
  lines(ListallDatamean[[i]][ListallDatamean[[i]]$Fencetype == "control", 3], type = "l", col = c("blue"))
  lines(ListallDatamean[[i]][ListallDatamean[[i]]$Fencetype == "fullex", 3], type = "l", col = c("green"))
  lines(ListallDatamean[[i]][ListallDatamean[[i]]$Fencetype == "mhex", 3], type = "l", col = c("red"))
  legend("topleft",  
         legend = c("mhex", "control", "fullex"),
         col = c("red", "blue", "green"),
         lty = 1)
}
dev.off()


# do some clearing
rm(i, Split, Groups, Species, Speciesoverview, Groupsoverview, ListTracks_grp, ListTracks_spc)



#------------------------------------------------------3 Models--------------------------------------------------------

#-----------------------------------------------3.1 Exclude Fullex-----------------------------------------------------


# exclude "fullex" edges for analysis
for (i in 1:length(ListallData)){
  ListallData[[i]] <- ListallData[[i]][!ListallData[[i]]$Fencetype == "fullex",]
}



#---------------------------------------------------3.2 GLMER----------------------------------------------------------


# Create lists to save models
ListallglmmTMB <- list()
ListallglmmTMBTbil <- list()


# GLMER

# an offset for the number of edges is needed to make values comparable!!
# as link funktion for nb is in log, it is necessary to use "offset(log(value))" (log is natural logarithm)


# Create Models
for (i in c(1:length(ListallData))){
  Data <- ListallData[[i]]
  
  # Glmm with glmmTMB
  TMB.nb <- glmmTMB(AnimalNumber ~ offset(log(Fencenumber)) + Fencetype +(1|Block), 
                    data = Data, family = "nbinom2")
 
  NAME <- paste("TMB.nb",NAMES[i], sep = "_")
  ListallglmmTMB[[NAME]]<- TMB.nb
  
  # with Track Visibility included
  Data <- merge(ListallData[[i]], Trackabilityshort)
  TMB.nb2 <- glmmTMB(AnimalNumber ~ offset(log(Fencenumber)) + TrackVis + Fencetype + (1|Block), 
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
  NAME <- paste("TMBTBil",NAMES[i], sep = "_")
  ListallglmmTMBTbil[[NAME]] <- m
}


# convergence problems: Duiker (TMB2), Wildebeest (TMB,TMB2), Giraffe (TMB2), small (TMB), Eland (all), Mega-Herbivores (TMB2)



# Check model results
i = 3 # can be 1 to 18
names(ListallglmmTMB)[i]; summary(ListallglmmTMB[[i]])
names(ListallglmmTMBTbil)[i]; summary(ListallglmmTMBTbil[[i]])


# do some clearing
rm(anv, m, TMB.nb, TMB.nb2, i, NAME)



#-------------------------------------------3.3 Tables with Results ---------------------------------------------------


# Create a data table as overview of p-values
# a pairwise test is not necessary here

NAMES2 <- NAMES[c(1:length(ListallData))]
pvaluesallmodels <- data.frame(NAMES2)
for (i in 1:length(NAMES2)){
  TMB.nb <- ListallglmmTMB[[i]]
  TMBTbil.nb <- ListallglmmTMBTbil[[i]]

  pvaluesallmodels[i,2] <- summary(TMB.nb)$coefficients$cond[rownames(summary(TMB.nb)$coefficients$cond) =="Fencetypemhex",4]
  pvaluesallmodels[i,3] <- summary(TMBTbil.nb)$coefficients$cond[rownames(summary(TMBTbil.nb)$coefficients$cond) =="Fencetypemhex",4]
  
  if("TrackVis" %in% rownames(summary(TMBTbil.nb)$coefficients$cond)){ 
    pvaluesallmodels[i,4] <- summary(TMBTbil.nb)$coefficients$cond[rownames(summary(TMBTbil.nb)$coefficients$cond) =="TrackVis",4]
  }else{ 
    pvaluesallmodels[i,4] <- 2
  }
}

colnames(pvaluesallmodels) <- c("Sp/Gr","p-TMBnb", "p-TMBbil.nb","p-Tbil")
pvaluesallmodels[,2:4] <- round(pvaluesallmodels[,2:4], digits = 4); pvaluesallmodels
# p-Tbil = 2 shows that TrackVis was not important and thus not included in the model

# save as xlsx (remove "#" in following line)
# write.xlsx(pvaluesallmodels, file = "pvaluesallmodels.xlsx", sheetName="pvalues")


# replace values with sympbols (>0.1 is NA)
pvaluesallmodels[,2:4][pvaluesallmodels[,2:4]<0.001] <- "***"
pvaluesallmodels[,2:4][pvaluesallmodels[,2:4]>0.1 & pvaluesallmodels[,2:4]<1.1] <- NA
pvaluesallmodels[,2:4][pvaluesallmodels[,2:4]>0.05 & pvaluesallmodels[,2:4]<0.1] <- "."
pvaluesallmodels[,2:4][pvaluesallmodels[,2:4]>0.01 & pvaluesallmodels[,2:4]<0.05] <- "*"
pvaluesallmodels[,2:4][pvaluesallmodels[,2:4]>0.001 & pvaluesallmodels[,2:4]<0.01] <- "**"
# save as xlsx (remove "#" in following line)
# write.xlsx(pvaluesallmodels, file = "pvaluesallmodels.xlsx", sheetName="pvalues_symb", append=TRUE)




# Create a data table as overview of data means and sd

Split <- str_split_fixed(names(ListallDatamean), "_",2)[,2]

# create dataframe
meanstrack <- data.frame(Split)

# add the values
for (i in 1:(length(ListallDatamean))){
  data <- ListallDatamean[[i]]
  agg <- aggregate(data$AnimalNumber, by = list(data$Fencetype), FUN=mean, na.rm=TRUE)
  agg2 <- aggregate(data$AnimalNumber, by = list(data$Fencetype), FUN=sd, na.rm=TRUE) # sd
  
  meanstrack[i,2] <- agg[1,2]
  meanstrack[i,3] <- agg2[1,2]
  meanstrack[i,4] <- agg[3,2] #fullex
  meanstrack[i,5] <- agg2[3,2]
  meanstrack[i,6] <- agg[3,2]/agg[1,2] * 100 
  meanstrack[i,7] <- agg[2,2] #mhex
  meanstrack[i,8] <- agg2[2,2]
  meanstrack[i,9] <- agg[2,2]/agg[1,2] * 100
  
}
colnames(meanstrack) <- c("Sp/Gr","control", "control_sd","mhex","mhex_sd" ,"% mhex/co","fullex","fullex_sd" ,"% fu/co")
meanstrack[,2:9] <- round(meanstrack[,2:9], digits = 2); meanstrack

# save as xlsx (remove "#" in following line)
# write.xlsx(meanstrack, file = "dataoverview.xlsx", sheetName="tracksedges", append=TRUE)



#----------------------------------------------3.4 Check Assumptions---------------------------------------------------


# DHARMa Package by Florian Hartig (2022):

# "The ‘DHARMa’ package uses a simulation-based approach to create readily interpretable scaled (quantile) residuals for fitted (generalized) linear mixed models
# The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression. The package also provides a number of plot and test functions for typical model misspecification problems, such as over/underdispersion, zero-inflation, and residual spatial and temporal autocorrelation.
# cited from Florian Hartig (2022) https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#poisson-data

i = 1
names(ListallglmmTMB[i])
TMB.nb <- ListallglmmTMB[[i]]
sim_TMB.nb <- simulateResiduals(fittedModel = TMB.nb,plot = F)

# best to test models individually
i = 1 # can be 1 to 18
names(ListallglmmTMBTbil[i])
Data <- ListallData[[i]]
TMBTbil.nb <- ListallglmmTMBTbil[[i]]

# It is highly recommended to first calculate the residuals once, using the simulateResiduals() function
sim_TMBTbil.nb <- simulateResiduals(fittedModel = TMBTbil.nb,plot = F)

par(mfrow=c(1,2))
testUniformity(sim_TMBTbil.nb, plot=T)
testQuantiles(sim_TMBTbil.nb, plot=T) 


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



# test for OVERDISPERSION and ZEROINFLATION from DHARMa package
# for all Species/Groups

dispzerofunction <- function(s1){
  dispzero <- data.frame(NAMES2)
  for (i in 1:length(ListallglmmTMB)){
    # Data and Models
    Data <- ListallData[[i]]
    TMB.nb <- ListallglmmTMB[[i]]
    
    # for the cases where TrackVis is used in model: (did not work with listcontent)
    if(i %in% c(4,7,13,16)){
      Data <- merge(Data,Trackabilityshort)
      TMBTbil.nb <- glmmTMB(AnimalNumber ~  offset(log(Fencenumber)) + TrackVis + Fencetype + (1|Block), 
                            data = Data, family = "nbinom2")
    }else{
      TMBTbil.nb <- TMB.nb
    }
    
    # overdispersion
    dispzero[i,2]<-testDispersion(TMB.nb)$statistic
    dispzero[i,3]<-testDispersion(TMB.nb)$p.value
    dispzero[i,4]<-testDispersion(TMBTbil.nb)$statistic
    dispzero[i,5]<-testDispersion(TMBTbil.nb)$p.value
    
    # zeroinflation
    dispzero[i,6]<-testZeroInflation(TMB.nb)$statistic
    dispzero[i,7]<-testZeroInflation(TMB.nb)$p.value
    dispzero[i,8]<-testZeroInflation(TMBTbil.nb)$statistic
    dispzero[i,9]<-testZeroInflation(TMBTbil.nb)$p.value
    

  }
  colnames(dispzero) <- c("Sp/Gr","rationbTMB","p-TMB","rationbTMBTbil","p-TMBTbil","zeroTMB","p-zeroTMB","zeroTMBTbil","p-zeroTMBTbil")
  dispzero[,2:9] <- round(dispzero[,2:9], digits = 2); dispzero
  # replace values above 0.05 with NA for better visuality
  dispzero[,c(3,5,7,9)][dispzero[,c(3,5,7,9)]>0.05] <- NA
  dispzero
}
# remove "#" in following line to check for overdispersion and zeroinflation
# dispzerofunction(s1)
# --> NO significant dispersion or zeroinflation detected!!



# check the RANDOM EFFECTS
# for all Groups/Speciese

# Comment from Florian Hartig (2022) https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#poisson-data :
# check normality of random effects
# just extract the REs, and then run e.g.a Shapiro test.


Randomeffnormal <- data.frame(NAMES2)
for (i in 1:length(ListallglmmTMB)){
  # Data and Models
  Data <- ListallData[[i]]
  TMB.nb <- ListallglmmTMB[[i]]
  
  # for the cases where TrackVis is used in model: (did not work with list content)
  if(i %in% c(4,7,13,16)){
    Data <- merge(Data,Trackabilityshort)
    TMBTbil.nb <- glmmTMB(AnimalNumber ~  offset(log(Fencenumber)) + TrackVis + Fencetype + (1|Block), 
                          data = Data, family = "nbinom2")
  }else{
    TMBTbil.nb <- TMB.nb
  }
  
  # qqplots
  par(mfrow=c(1,2))
  qqPlot(ranef(TMB.nb)$cond$Block[,1])
  qqPlot(ranef(TMBTbil.nb)$cond$Block[,1])
  
  # shapiro normality test
  Randomeffnormal[i,2] <- shapiro.test(as.numeric(ranef(TMB.nb)$cond$Block[,1]))$p.value
  Randomeffnormal[i,3] <- shapiro.test(as.numeric(ranef(TMBTbil.nb)$cond$Block[,1]))$p.value
}
colnames(Randomeffnormal) <- c("Sp/Gr","p-TMB","p-TMBTbil")
Randomeffnormal[,2:3] <- round(Randomeffnormal[,2:3], digits = 2); Randomeffnormal
# RE not normal for Giraffe, Eland

dev.off()





