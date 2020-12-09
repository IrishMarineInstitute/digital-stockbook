###########Renaming Quota pie chart images############
######################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### USING FILEPATH IN 2020_GIT ############################################
#Original code written by Oksana Kalinina Oct 2019 for TAC assessment areas
#Modified by Siobhan Moran Oct 2020, for VMS - Irishlandings###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())
library(png)

# The csv file 'VMS_Rename.csv' lists the incorrect saved png names,(format 'Xxx' e.g. Cod) against the required names for app.R 
# i.e. 'LandXXX' - LandCOD
#=B2&""&C2
# The following code resaves the png images in a new folder, using the required names.

##  2020 Background  ## 
##~~~~~~~~~~~~~~~~~~~~~
#In 2020, the VMS plots were saved in 'Z:\StockBooks\_StockBook2020\maps\VMS\2019'
#1. Copy this folder across to the Informatics folder: 'Z:\InformaticsProject\Phase1\Stockbook Handover\2020_Git\www\Irishlandings\2020_wrongNames'
#2. Create a new folder named with the current year
#3. Update the year in the loop in the R script 
#4. The png files are read from the edited script and pasted into the current year folder with the required names. 
#5. The extra folder can be deleted: '2020_wrongNames' 

##  2020 Specifics  ## 
##~~~~~~~~~~~~~~~~~~~~
#There is no plot for spurdog 
#SOme maps will not be shown in the Shiny app - Jax, 

##  2020 Run Code  ## 
##~~~~~~~~~~~~~~~~~~~~
#if 'setwd'doesn't work, do 'Session/Set Working Directory/Choose Directory' and browse to the folder
setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/Irishlandings")

#read in csvfile with TACcode and corresponding Stock names 
n<-read.csv("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/Irishlandings/VMS_Rename.csv")

#Go to the edited folder to pull the png's 
setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/Irishlandings/2020_wrongNames")

#create a folder for the current year and run the loop to save the plots with the required names 
for(i in 1:dim(n)[1]){
  img<-readPNG(list.files(pattern=paste(n[1][i,])))
  writePNG(img,paste0("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/Irishlandings/2020/",n[4][i,],".png"))}