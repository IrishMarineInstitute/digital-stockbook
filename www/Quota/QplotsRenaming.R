###########Renaming Quota pie chart images############
######################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### USING FILEPATH IN 2021_GIT ############################################
#Original code written by Oksana Kalinina Oct 2019 
#Modified by Siobhan Moran Sep 2021, for 2021 issues ######################
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
#3.1SAve the csv file with only the common plots, remove anything thats not in the list or the code will stop outputting png's at that point
#4. Add plot names if necessary (in 2020 a spurdog plot was present) and re-save the csv file
#5. Create a new folder named with the current year
#6. Update the year in the loop in the R script 
#7. The png files are read from the edited script and pasted into the current year folder with the required names. 
#8. The extra folder can be deleted: '2020_wrongNames' 

##  2021 Specifics  ## 
##~~~~~~~~~~~~~~~~~~~~
## 15/09/21: From HG
## "The pie plots of the quota are now updated so for stocks where there is no full coastal states agreement (mac, whb, her12, cod12 and albacore), 
## we only show EU quota F:\StockBooks\_Stockbook2021\Plots\Quota pie plots\plots
## 
# Strangely, there are quite a few stocks where the sum of the quota does not add up to the TAC (e.g. celtic sea whiting) - this is probably to do with the timing of agreeing the TAC and the EU and UK shares but quite odd
# 3: HAD 7X7A34 sum of quota:14383 not same as TAC:15000
# 6: LEZ 07. sum of quota:18139 not same as TAC:18365
# 8: PLE 07A. sum of quota:2632 not same as TAC:2846
# 9: PLE 7FG. sum of quota:1728 not same as TAC:1911
# 10: SOL 07A. sum of quota:754 not same as TAC:768
# 11: SOL 7FG. sum of quota:1388 not same as TAC:1413
# 13: WHG 07A. sum of quota:720 not same as TAC:721
# 14: WHG 7X7A-C sum of quota:9826 not same as TAC:10259



##  2020 Specifics  ## 
##~~~~~~~~~~~~~~~~~~~~
##There is a new plot for spurdog 
##

##  2021 Run Code  ## 
##~~~~~~~~~~~~~~~~~~~~
#if 'setwd'doesn't work, do 'Session/Set Working Directory/Choose Directory' and browse to the folder
setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/Quota")

#read in csvfile with TACcode and corresponding Stock names 
n<-read.csv("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/Quota/QSimagesNames.csv")
#2021 53 stocks
#2020 59 stocks

#Go to the edited folder to pull the png's 
#setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/Quota/Ireland Relevant Quota Pie Charts_edited")
#2021
setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/Quota/plots")

#create a folder for the current year and run the loop to save the plots with the required names 
for(i in 1:dim(n)[1]){
  img<-readPNG(list.files(pattern=paste(n[1][i,])))
  writePNG(img,paste0("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/Quota/2021/",n[2][i,],".png"))} 
#the original code had [4] but gave errors of: Error in `[.data.frame`(n, 4) : undefined columns selected