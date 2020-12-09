###########Renaming Quota pie chart images############
######################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### USING FILEPATH IN 2020_GIT ############################################
#Original code written by Oksana Kalinina Oct 2019 
#Modified by Siobhan Moran Oct 2020, for 2020 issues ######################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())
library(png)

# The csv file 'QSimagesNames.csv' lists the incorrect saved png names,(format 'COD 5BE6A') against the required names for app.R 
# i.e. 'cod.27.6a' 

# The following code resaves the png images in a new folder, using the required names.

##  2020 Background  ## 
##~~~~~~~~~~~~~~~~~~~~~
#In 2020, the pie plots were saved in 'Z:\StockBooks\_StockBook2020\Plots\Quota\Quota pie plots\Ireland Relevant Quota Pie Charts'
#1. Copy this folder across to the Informatics folder: 'Z:\InformaticsProject\Phase1\Stockbook Handover\2020_Git\www\Quota'
#2. Make a copy of this folder for editing - '..Ireland Relevant Quota Pie Charts_edited'
#3. Cross reference the list of plots in this folder against the list in the csv file - 'QSimagesNames.csv'. Progress can be tracked in an Excel file similar to 'QSimagesNames_SMOctEdits.xlsx' 
#4. Add plot names if necessary (in 2020 a spurdog plot was present) and re-save the csv file
#5. Create a new folder named with the current year
#6. Update the year in the loop in the R script 
#7. The png files are read from the edited script and pasted into the current year folder with the required names. 
#8. The extra folder can be deleted: '2020_wrongNames' 

##  2020 Specifics  ## 
##~~~~~~~~~~~~~~~~~~~~
#There is a new plot for spurdog 
#

##  2020 Run Code  ## 
##~~~~~~~~~~~~~~~~~~~~
#if 'setwd'doesn't work, do 'Session/Set Working Directory/Choose Directory' and browse to the folder
setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/Quota")

#read in csvfile with TACcode and corresponding Stock names 
n<-read.csv("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/Quota/QSimagesNames.csv")

#Go to the edited folder to pull the png's 
setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/Quota/Ireland Relevant Quota Pie Charts_edited")

#create a folder for the current year and run the loop to save the plots with the required names 
for(i in 1:dim(n)[1]){
  img<-readPNG(list.files(pattern=paste(n[1][i,])))
  writePNG(img,paste0("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/Quota/2020/",n[4][i,],".png"))}