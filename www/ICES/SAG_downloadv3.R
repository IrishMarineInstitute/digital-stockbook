##  Downloading your stock from Standarad Graphs
##           Ver 0.1 Colm Lordan


# install from CRAN
#install.packages("icesSAG")
library(icesSAG)

setwd("H:/Stockbook/2018/2018_V1/www/ICES")

IrishStocks=read.csv("stock_lookupV2.csv", header=TRUE)
dim(IrishStocks)
head(IrishStocks)
colnames(IrishStocks) <- c("ecoregion","Old","sepcies type" ,"New","x")


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
