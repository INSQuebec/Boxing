# In this file, R-code to analyse data

######################################
#PACKAGES
######################################
library(knitr)
library(rmarkdown)
library(tinytex)
library(here)
library(tidyverse)
library(rmdformats)
library(kableExtra)
library(eyetrackingR)
library(data.table)
library(itsadug)
library(lme4)
library(multcomp)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(openxlsx)
library(ggpubr) #graphique pour créer & customiser ggplot2
library(caret)
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  cache = TRUE,
  comment = NA,
  prompt = FALSE,
  tidy = TRUE
)
opts_knit$set(width=75)
options(stringsAsFactors = F)          # no automatic data transformation
options("scipen" = 100, "digits" = 10) # suppress math annotation

######################################
#DATA
######################################

## Modify these path lines : 45, 305, 348, 370

#Data path
datapath <- here::here("path", "Data_frame")

#Sequence time
#Calculate the total duration of each testing sequence
MinMax<-data %>%
  dplyr::select (Video.Name, Timeline, mode)%>%
  group_by(Video.Name, mode) %>%
  summarise(
    Max = max(Timeline, na.rm = T),
    Min = min(Timeline, na.rm = T)
    ) %>%
  arrange(Video.Name)
MinMax<-MinMax %>%
  mutate(DureeVideo=Max-Min)

######################################
#EVENTS
######################################

# Oculomotor events
data$Combine.Eye.Gaze.Point..x = as.numeric(as.character(data$Combine.Eye.Gaze.Point..x))
data$Combine.Eye.Gaze.Point..y = as.numeric(as.character(data$Combine.Eye.Gaze.Point..y))
                                            
#Event2D
event2D <- data %>%
  filter(mode== "2D")
#Event360VR
event360VR <- data %>% 
  filter(mode== "360")
                                            
######### Event2D  ######### 
# Velocity : d/t
event2D <- event2D %>%
  dplyr::mutate(distancex = Combine.Eye.Gaze.Point..x - lag(Combine.Eye.Gaze.Point..x)) %>%
  dplyr::mutate(distancey = Combine.Eye.Gaze.Point..y - lag(Combine.Eye.Gaze.Point..y)) %>%
  dplyr::mutate(distance = (distancex)^2 + (distancey)^2)  %>%
  dplyr::mutate(temps = Timeline - lag(Timeline)) %>%
  dplyr::mutate(velocity=distance/temps)
data_velocity2D <- filter(event2D, velocity>0)
#Threshold
valeur_seuil_2D<- 5*median(data_velocity2D$velocity,na.rm=TRUE)
data_velocity2D <- data_velocity2D %>%
  mutate(seuil = ifelse(velocity > valeur_seuil_2D, valeur_seuil_2D, 0))
###Fixations
data_velocity2D <- data_velocity2D %>%
  mutate(duree = ifelse(seuil==0, 1,0))
data_velocity2D$duree2 <- with(data_velocity2D, ave(duree, cumsum(duree==0), FUN=cumsum, NA.rm=FALSE))
data_velocity2D$dureefix <- data_velocity2D$duree2*50
data_velocity2D_fixations <- data_velocity2D %>%
  filter (lead(data_velocity2D$dureefix)==0)
data_velocity2D_fixations <- data_velocity2D_fixations %>%
  filter (data_velocity2D_fixations$dureefix>0)
data_velocity2D_fixations <- data_velocity2D_fixations %>%
  filter (data_velocity2D_fixations$dureefix>=100)
nb_fixation2D<-nrow(data_velocity2D_fixations)
###Saccades
data_velocity2D$duree3 <- ifelse(data_velocity2D$duree==0, 1,0)
data_velocity2D$duree4 <- with(data_velocity2D, ave(duree3, cumsum(duree3==0), FUN=cumsum, NA.rm=FALSE))
data_velocity2D$dureesacc <- data_velocity2D$duree4*50
data_velocity2D_saccades <- data_velocity2D %>%
  filter (lead(data_velocity2D$dureesacc)==0)
data_velocity2D_saccades<- data_velocity2D_saccades %>%
  filter (data_velocity2D_saccades$dureesacc>0)
data_velocity2D_saccades <- data_velocity2D_saccades %>%
  filter (data_velocity2D_saccades$dureesacc<=50)
                                            
######### Event360VR #########
event360VR <- event360VR %>%
  mutate(distancex = Combine.Eye.Gaze.Point..x - lag(Combine.Eye.Gaze.Point..x)) %>%
  mutate(distancey = Combine.Eye.Gaze.Point..y - lag(Combine.Eye.Gaze.Point..y)) %>%
  mutate(distance = (distancex)^2 + (distancey)^2)  %>%
  mutate(temps = Timeline - lag(Timeline)) %>%
  mutate(velocity=distance/temps)
data_velocity360VR <- filter(event360VR, velocity>0)
valeur_seuil_360<- 5*median(data_velocity360VR$velocity,na.rm=TRUE)
data_velocity360VR <- data_velocity360VR %>%
  mutate(seuil = ifelse(velocity > valeur_seuil_360, valeur_seuil_360, 0))
###Fixations
data_velocity360VR <- data_velocity360VR %>% 
  mutate(duree = ifelse(seuil==0, 1,0))
data_velocity360VR$duree2 <- with(data_velocity360VR, ave(duree, cumsum(duree==0), FUN=cumsum, NA.rm=FALSE))
data_velocity360VR$dureefix <- data_velocity360VR$duree2*50
data_velocity360VR_fixations <- data_velocity360VR %>%
  filter (lead(data_velocity360VR$dureefix)==0)
data_velocity360VR_fixations <- data_velocity360VR_fixations %>%
  filter (data_velocity360VR_fixations$dureefix>0)
data_velocity360VR_fixations <- data_velocity360VR_fixations %>%
  filter (data_velocity360VR_fixations$dureefix>=100)
nb_fixation360VR<-nrow(data_velocity360VR_fixations)
###Saccades
data_velocity360VR$duree3 <- ifelse(data_velocity360VR$duree==0, 1,0)
data_velocity360VR$duree4 <- with(data_velocity360VR, ave(duree3, cumsum(duree3==0), FUN=cumsum, NA.rm=FALSE))
data_velocity360VR$dureesacc <- data_velocity360VR$duree4*50
data_velocity360VR_saccades <- data_velocity360VR %>%
  filter (lead(data_velocity360VR$dureesacc)==0)
data_velocity360VR_saccades<- data_velocity360VR_saccades %>%
  filter (data_velocity360VR_saccades$dureesacc>0)
data_velocity360VR_saccades <- data_velocity360VR_saccades %>%
  filter (data_velocity360VR_saccades$dureesacc<=50)
                                            
#Normalize data from fixations & saccades calculation by Sequence and by Participant
fixation_mean_2D<-data_velocity2D_fixations %>%
  group_by(participant, mode, sexe, numero.video, Video.Name)%>%
  summarize(mean(dureefix))
fixation_mean_2D<-rename(fixation_mean_2D,dureefix="mean(dureefix)")
fixation_nb_2D<-data_velocity2D_fixations %>%
  group_by(participant, mode, sexe, numero.video, Video.Name)%>%
  summarize(nbfixation=n())
fixation_mean_360VR<-data_velocity360VR_fixations %>%
  group_by(participant, mode, sexe, numero.video, Video.Name)%>%
  summarize(mean(dureefix))
fixation_mean_360VR<-rename(fixation_mean_360VR,dureefix="mean(dureefix)")
fixation_nb_360VR<-data_velocity360VR_fixations %>%
  group_by(participant, mode, sexe, numero.video, Video.Name)%>%
  summarize(nbfixation=n())
saccade_nb_2D<-data_velocity2D_saccades %>%
  group_by(participant, mode, sexe, numero.video, Video.Name)%>%
  summarize(nbsaccade=n())
saccade_nb_360VR<-data_velocity360VR_saccades %>%
  group_by(participant, mode, sexe, numero.video, Video.Name)%>%
  summarize(nbsaccade=n())

fixation_mean_2D <- merge(fixation_mean_2D, MinMax, by = c("Video.Name","mode"))
fixation_mean_2D <- fixation_mean_2D %>%
  group_by(Video.Name,participant,sexe) %>%
  mutate(Dureefix_normalized = dureefix / DureeVideo)

fixation_nb_2D <- merge(fixation_nb_2D, MinMax, by = c("Video.Name","mode"))
fixation_nb_2D <- fixation_nb_2D %>%
  group_by(Video.Name,participant,sexe) %>%
  mutate(Nbfix_normalized = nbfixation / DureeVideo)
                                            
fixation_mean_360VR <- merge(fixation_mean_360VR, MinMax, by = c("Video.Name","mode"))
fixation_mean_360VR <- fixation_mean_360VR %>%
  group_by(Video.Name,participant,sexe) %>%
  mutate(Dureefix_normalized = dureefix / DureeVideo)
                                            
fixation_nb_360VR <- merge(fixation_nb_360VR, MinMax, by = c("Video.Name","mode"))
fixation_nb_360VR <- fixation_nb_360VR %>%
  group_by(Video.Name,participant,sexe) %>%
  mutate(Nbfix_normalized = nbfixation / DureeVideo)
                                            
saccade_nb_2D <- merge(saccade_nb_2D, MinMax, by = c("Video.Name","mode"))
saccade_nb_2D <- saccade_nb_2D %>%
  group_by(Video.Name,participant,sexe) %>%
  mutate(Nbsaccade_normalized = nbsaccade / DureeVideo)
                                            
saccade_nb_360VR <- merge(saccade_nb_360VR, MinMax, by = c("Video.Name","mode"))
saccade_nb_360VR <- saccade_nb_360VR %>%
  group_by(Video.Name,participant,sexe) %>%
  mutate(Nbsaccade_normalized = nbsaccade / DureeVideo)

## Fixations
### Number of fixations
#calculate the average number of fixations per sequence per participant
nb_fixation2D_Participant<- fixation_nb_2D %>%
  group_by(participant, mode, sexe) %>%
  summarize(mean(Nbfix_normalized))
nb_fixation360VR_Participant<- fixation_nb_360VR %>%
  group_by(participant,mode, sexe) %>%
  summarize(mean(Nbfix_normalized))
#Fusion
Nbfixation<-merge(nb_fixation2D_Participant,nb_fixation360VR_Participant,by=c("participant","mode","sexe", "mean(Nbfix_normalized)"),all.x=TRUE,all.y=TRUE)
Nbfixation<-rename(Nbfixation,nbfixation="mean(Nbfix_normalized)")

### Fixation durations
#calculate the average duration of fixations per participant
duree_fixation2D_Participant<- fixation_mean_2D %>%
  group_by(participant, mode, sexe) %>%
  summarize(mean(dureefix))
duree_fixation360VR_Participant<- fixation_mean_360VR %>%
  group_by(participant, mode, sexe) %>%
  summarize(mean(dureefix))
Dureefixation<-merge(duree_fixation2D_Participant,duree_fixation360VR_Participant,by=c("participant","mode", "sexe", "mean(dureefix)"),all.x=TRUE,all.y=TRUE)
Dureefixation<-rename(Dureefixation,dureefixation="mean(dureefix)")

### Search rate
duree_totale_fixation2D_Participant<- fixation_mean_2D %>%
  group_by(participant,mode,sexe) %>%
  summarize(sum(dureefix))
search_rate_2D<- merge(nb_fixation2D_Participant,duree_totale_fixation2D_Participant,by=c("participant","mode", "sexe"),all.x=TRUE,all.y=TRUE)
search_rate_2D$searchrate = search_rate_2D$`mean(Nbfix_normalized)`/search_rate_2D$`sum(dureefix)`
duree_totale_fixation360VR_Participant<- fixation_mean_360VR %>%
  group_by(participant,mode,sexe) %>%
  summarize(sum(dureefix))
search_rate_360VR<- merge(nb_fixation360VR_Participant,duree_totale_fixation360VR_Participant, by=c("participant","mode","sexe"),all.x=TRUE,all.y=TRUE)
search_rate_360VR$searchrate = search_rate_360VR$`mean(Nbfix_normalized)`/search_rate_360VR$`sum(dureefix)`
Searchrate<-merge(search_rate_2D,search_rate_360VR,by=c("participant","mode","sexe","searchrate"),all.x=TRUE,all.y=TRUE)
Searchrate <- Searchrate %>%
  dplyr::select (participant, mode, sexe, searchrate)
                                            
### Saccades
nb_saccade2D_Participant<- saccade_nb_2D %>%
 group_by(participant,mode,sexe) %>%
  summarize(mean(Nbsaccade_normalized))
nb_saccade360VR_Participant<- saccade_nb_360VR %>%
 group_by(participant,mode,sexe) %>%
  summarize(mean(Nbsaccade_normalized))
Nbsaccade<-merge(nb_saccade2D_Participant,nb_saccade360VR_Participant,by=c("participant","mode","mean(Nbsaccade_normalized)","sexe"),all.x=TRUE,all.y=TRUE)
Nbsaccade<-rename(Nbsaccade,nbsaccade="mean(Nbsaccade_normalized)")


## Create a common table
GB <- merge(Nbfixation, Dureefixation) %>%
  merge(Searchrate) %>%
  merge (Nbsaccade)
write.xlsx(GB, "path+name.xlsx", sep="\t")
                                            
# Head excursions
HeadRot_Data<-data %>%
dplyr::select(participant, mode, sexe, numero.video, Video.Name, Head.Rotation..x, Head.Rotation..y, Head.Rotation..z)
#Remove 360 from all values greater than 180 (those passing axis 0, as the calculation is performed at left +1° and right -359°).
HeadRot_Data$Head.Rotation..x <- ifelse(HeadRot_Data$Head.Rotation..x > 180, HeadRot_Data$Head.Rotation..x - 360, HeadRot_Data$Head.Rotation..x)
HeadRot_Data$Head.Rotation..y <- ifelse(HeadRot_Data$Head.Rotation..y > 180, HeadRot_Data$Head.Rotation..y - 360, HeadRot_Data$Head.Rotation..y)
HeadRot_Data$Head.Rotation..z <- ifelse(HeadRot_Data$Head.Rotation..z > 180, HeadRot_Data$Head.Rotation..z - 360, HeadRot_Data$Head.Rotation..z)
#Search for head movement for each axis between each measurement
HeadRot_Data <- HeadRot_Data %>%
  group_by(Video.Name) %>%
  mutate(
    HRot_x = ifelse(Video.Name == lag(Video.Name), c(NA, diff(Head.Rotation..x)), NA),
    HRot_y = ifelse(Video.Name == lag(Video.Name), c(NA, diff(Head.Rotation..y)), NA),
    HRot_z = ifelse(Video.Name == lag(Video.Name), c(NA, diff(Head.Rotation..z)), NA)
    )
#Normalize
HeadRot_Data <- HeadRot_Data |> mutate(
  HRot_x = sqrt(HRot_x^2),
  HRot_y = sqrt(HRot_y^2),
  HRot_z = sqrt(HRot_z^2),
  )
HeadRot_Results <- HeadRot_Data %>%
  filter(!is.na(HRot_x)) %>%
  filter(!is.na(HRot_y)) %>%
  filter(!is.na(HRot_z)) %>%
  group_by(Video.Name,participant,mode,sexe) %>%
  summarize(HRot_Value_x = sum(HRot_x, na.rm = TRUE),
            HRot_Value_y = sum(HRot_y, na.rm = TRUE),
            HRot_Value_z = sum(HRot_z, na.rm = TRUE))
#Normalize by the time of each video sequence
HeadRot_Results <- merge(HeadRot_Results, MinMax, by = c("Video.Name","mode"))
HeadRot_Results <- HeadRot_Results %>%
  group_by(Video.Name,participant,mode,sexe) %>%
  mutate(HRot_Value_x_normalized = HRot_Value_x / DureeVideo,
         HRot_Value_y_normalized = HRot_Value_y / DureeVideo,
         HRot_Value_z_normalized = HRot_Value_z / DureeVideo)
HeadRot_Final <- HeadRot_Results %>%
  group_by(participant, mode, sexe) %>%
  summarise(Headrot_x = mean(HRot_Value_x),
            Headrot_y = mean(HRot_Value_y),
            Headrot_z = mean(HRot_Value_z))
write.xlsx(HeadRot_Final, "path+name.xlsx", sep="\t")

                                          
######################################
#STATISTICAL ANALYSIS
######################################
library(skimr) #stats descriptives
library(lme4)
library(MASS) #glmm pour proportion 
library(tidyverse)
library(dplyr)
library(car)
library(Rmisc)
library(multcomp) #post hoc comparaison 2*2
library(lmerTest)
library(ez) #sphéricité
library(questionr)
library(PMCMRplus) #post hoc Conover - après test Friedman 
library(devtools)
library(report)

#Data path
datapath <- here::here("path", "Data_frame")

# Gaze behavior
GB<-read.xlsx("GB.xlsx",sheet=1, startRow=1,colNames=TRUE,rowNames=FALSE)
GBLong <- melt(GB, id = c("mode","participant","sexe"))
##Test
GBTest<-lmerTest::lmer(value~variable*mode*sexe+(1|participant),data=GBLong)
summary(GBTest)
reportGB<-report(GBTest)
report_parameters(GBTest)
report_effectsize(GBTest)
##Posthoc
GBLong$gpe<-interaction(GBLong$mode,GBLong$variable, sep="_")
mod_gpe_GB<-lmerTest::lmer(value~gpe+(1|participant),data=GBLong)
compa<-glht(mod_gpe_GB, linfct=mcp(gpe="Tukey"))
summary(compa)


# Head excursions
TRot<-read.xlsx("TeteRotation.xlsx",sheet=1, startRow=1,colNames=TRUE,rowNames=FALSE)
TRotLong <- melt(TRot, id = c("mode","participant","sexe"))
##Test
TRotTest<-lmerTest::lmer(value~variable*mode*sexe+(1|participant),data=TRotLong)
summary(TRotTest)
reportGB<-report(TRotTest)
report_parameters(TRotTest)
report_effectsize(TRotTest)
##Posthoc
TRotLong$gpe<-interaction(TRotLong$mode,TRotLong$variable, sep="_")
mod_gpe_TRot<-lmerTest::lmer(value~gpe+(1|participant),data=TRotLong)
compa<-glht(mod_gpe_TRot, linfct=mcp(gpe="Tukey"))
summary(compa)
