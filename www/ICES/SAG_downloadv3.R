##  Downloading your stock from Standarad Graphs
##           Ver 0.1 Colm Lordan


# install from CRAN
#install.packages("icesSAG")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2020 warning  !!   ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Warning in install.packages :
#  package icesSAG is not available (for R version 4.0.2)

#Package icesSAG was removed from the CRAN repository.
#Formerly available versions can be obtained from the archive.
#Archived on 2020-08-07 as it requires archived package 'icesVocab'.

#STEP 1
#downloaded the latest tar.gz from https://cran.r-project.org/src/contrib/Archive/icesSAG/
#open 'install'  - whan the pop-up box opens, select 'Package Archive File', from the first drop down box.
#Browse to the downloaded tar.gx file location and install. 

#Got an error message - ERROR: dependency 'png' is not available for package 'icesSAG'
#* removing 'C:/Users/smoran/Documents/R/win-library/4.0/icesSAG'

#STEP 2
#install 'png' package, then repeat STEP 1 and continue
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(icesSAG)

#setwd("H:/Stockbook/2018/2018_V1/www/ICES")

#update wd Oct 2019 - SM
#setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2019_V1/www/ICES")
setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/ICES")

IrishStocks=read.csv("stock_lookupV2.csv", header=TRUE)
dim(IrishStocks)
head(IrishStocks)
colnames(IrishStocks) <- c("ecoregion","Old","species type" ,"New","x")

#~~ NOTE ~~
#update wd Oct 2019 - SM - and 2020
#create a new loop, for the new years plot generation - 
#            2020 lines 247 to 300
#            2019 lines 172 to 220  
#skip down to the start of this new section (for 2020 - line 247), to run the code to generate the required plots
#make sure the path ends in '/' so the plots go into the required folder
#~~~~~~~~~~

#icesSAG graphs
#Define Species
Years=c(2015:2016)

#Stock Development Over Time
for(i in  IrishStocks$Old){
  for(j in Years){
  graphs <- getSAGGraphs(findAssessmentKey(i, j)[1])
  png(file=paste("H:/Stockbook/shiny/WIP/www/ICES/SAG/", j, "/", i, ".png", sep=""),
      width = 850, height = 650)
  plot(graphs)
  dev.off()
  }
}

for(i in  IrishStocks$New){
    graphs <- getSAGGraphs(findAssessmentKey(i, 2017)[1])
    png(file=paste("H:/Stockbook/2018/2018_V1/www/ICES/SAG/2017/", i, ".png", sep=""),
        width = 850, height = 650)
    plot(graphs)
    dev.off()
}

for(i in  IrishStocks$New){
  graphs <- getSAGGraphs(findAssessmentKey(i, 2018)[1])
  png(file=paste("H:/Stockbook/2018/2018_V1/www/ICES/SAG/2018/", i, ".png", sep=""),
      width = 850, height = 650)
  plot(graphs)
  dev.off()
}


#Stock and Exploitation status
Years=c(2015:2016)
for(i in  IrishStocks$Old){
  for(j in Years){
    graphs <- getStockStatusTable(findAssessmentKey(i, j)[1])
    png(file=paste("H:/Stockbook/shiny/WIP/www/ICES/Status/", j, "/", i, ".png", sep=""),
        width = 950, height = 215)
    plot(graphs)
    dev.off()
  }
}


for(i in  IrishStocks$New){
    graphs <- getStockStatusTable(findAssessmentKey(i, 2017)[1])
    png(file=paste("H:/Stockbook/2018/2018_V1/www/ICES/Status/2017/", i, ".png", sep=""),
        width = 950, height = 215)
    plot(graphs)
    dev.off()
}

for(i in  IrishStocks$New){
  graphs <- getStockStatusTable(findAssessmentKey(i, 2018)[1])
  png(file=paste("H:/Stockbook/2018/2018_V1/www/ICES/Status/2018/", i, ".png", sep=""),
      width = 950, height = 215)
  plot(graphs)
  dev.off()
}


#Quality of Assessment
Years=c(2015:2016)
for(i in  IrishStocks$Old){
  for(j in Years){
    graphs <- getSSBHistoricalPerformance(findAssessmentKey(i, j)[1])
    png(file=paste("H:/Stockbook/shiny/WIP/www/ICES/SSB/", j, "/", i, ".png", sep=""),
        width = 450, height = 350)
    plot(graphs)
    dev.off()
  }
}

for(i in  IrishStocks$New){
  graphs <- getSSBHistoricalPerformance(findAssessmentKey(i, 2017)[1])
  png(file=paste("H:/Stockbook/2018/2018_V1/www/ICES/SSB/2017/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}

for(i in  IrishStocks$New){
  graphs <- getSSBHistoricalPerformance(findAssessmentKey(i, 2018)[1])
  png(file=paste("H:/Stockbook/2018/2018_V1/www/ICES/SSB/2018/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}

#Fishing mortality
Years=c(2015:2016)
for(i in  IrishStocks$Old){
  for(j in Years){
    graphs <- getFishingMortalityHistoricalPerformance(findAssessmentKey(i, j)[1])
    png(file=paste("H:/Stockbook/shiny/WIP/www/ICES/Fishmort/", j, "/", i, ".png", sep=""),
        width = 450, height = 350)
    plot(graphs)
    dev.off()
  }
}

for(i in  IrishStocks$New){
  graphs <- getFishingMortalityHistoricalPerformance(findAssessmentKey(i, 2017)[1])
  png(file=paste("H:/Stockbook/2018/2018_V1/www/ICES/Fishmort/2017/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}

for(i in  IrishStocks$New){
  graphs <- getFishingMortalityHistoricalPerformance(findAssessmentKey(i, 2018)[1])
  png(file=paste("H:/Stockbook/2018/2018_V1/www/ICES/Fishmort/2018/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}

#RecruitHist
Years=c(2015:2016)
for(i in  IrishStocks$Old){
  for(j in Years){
    graphs <- getRecruitmentHistoricalPerformance(findAssessmentKey(i, j)[1])
    png(file=paste("H:/Stockbook/shiny/WIP/www/ICES/RecruitHist/", j, "/", i, ".png", sep=""),
        width = 450, height = 350)
    plot(graphs)
    dev.off()
  }
}

for(i in  IrishStocks$New){
  graphs <- getRecruitmentHistoricalPerformance(findAssessmentKey(i, 2017)[1])
  png(file=paste("H:/Stockbook/2018/2018_V1/www/ICES/RecruitHist/2017/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}

for(i in  IrishStocks$New){
  graphs <- getRecruitmentHistoricalPerformance(findAssessmentKey(i, 2018)[1])
  png(file=paste("H:/Stockbook/2018/2018_V1/www/ICES/RecruitHist/2018/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}


#~~~~~~  2019  ~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~
#Nov 01st - changed the file path from '....2019_V1' to '....2019_Git' 
#Stock Development Over Time
for(i in  IrishStocks$New){
  graphs <- getSAGGraphs(findAssessmentKey(i, 2019)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2019_Git/www/ICES/SAG/2019/", i, ".png", sep=""),
      width = 850, height = 650)
  plot(graphs)
  dev.off()
}


#Stock and Exploitation status
for(i in  IrishStocks$New){
  graphs <- getStockStatusTable(findAssessmentKey(i, 2019)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2019_Git/www/ICES/Status/2019/", i, ".png", sep=""),
      width = 950, height = 215)
  plot(graphs)
  dev.off()
}


#Quality of Assessment
for(i in  IrishStocks$New){
  graphs <- getSSBHistoricalPerformance(findAssessmentKey(i, 2019)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2019_Git/www/ICES/SSB/2019/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}

#Fishing mortality
for(i in  IrishStocks$New){
  graphs <- getFishingMortalityHistoricalPerformance(findAssessmentKey(i, 2019)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2019_Git/www/ICES/Fishmort/2019/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}

#RecruitHist
for(i in  IrishStocks$New){
  graphs <- getRecruitmentHistoricalPerformance(findAssessmentKey(i, 2019)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2019_Git/www/ICES/RecruitHist/2019/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}

#~~~~~~  2020  ~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~
#Oct 15th - changed the file path from '....2019_Git' to '....2020_Git' 
#Manually add a 2020 folder in each ICES folder, for the downloaded png's to be saved into

#Stock Development Over Time
for(i in  IrishStocks$New){
  graphs <- getSAGGraphs(findAssessmentKey(i, 2020)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/ICES/SAG/2020/", i, ".png", sep=""),
      width = 850, height = 650)
  plot(graphs)
  dev.off()
}


#Stock and Exploitation status
for(i in  IrishStocks$New){
  graphs <- getStockStatusTable(findAssessmentKey(i, 2020)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/ICES/Status/2020/", i, ".png", sep=""),
      width = 950, height = 215)
  plot(graphs)
  dev.off()
}


#Quality of Assessment
for(i in  IrishStocks$New){
  graphs <- getSSBHistoricalPerformance(findAssessmentKey(i, 2020)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/ICES/SSB/2020/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}

#Fishing mortality
for(i in  IrishStocks$New){
  graphs <- getFishingMortalityHistoricalPerformance(findAssessmentKey(i, 2020)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/ICES/Fishmort/2020/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}

#RecruitHist
for(i in  IrishStocks$New){
  graphs <- getRecruitmentHistoricalPerformance(findAssessmentKey(i, 2020)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/ICES/RecruitHist/2020/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
}



