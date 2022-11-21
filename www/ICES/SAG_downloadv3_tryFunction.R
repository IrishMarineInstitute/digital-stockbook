##  Downloading your stock from Standarad Graphs
##           Ver 0.1 Colm Lordan


# install from CRAN
#install.packages("icesSAG")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2021  ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Installed package 'icesVocab'
#click on 'icesVocab' in the list to the right to give
    #library("icesVocab", lib.loc="~/R/win-library/4.0")
#click on 'png' in the list to the right to give
    #library("png", lib.loc="~/R/win-library/4.0")
#click on 'icesSAG' in the list to the right to give
    #library("icesSAG", lib.loc="~/R/win-library/4.0")
#OR See Step 1 below. 


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
library(icesVocab)
library(png)
library(icesSAG)

#setwd("H:/Stockbook/2018/2018_V1/www/ICES")
#setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/ICES/_NephropsNov")

#update wd Oct 2019 - SM
#setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2019_V1/www/ICES")
#setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2020_Git/www/ICES")
#setwd("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/ICES")
setwd("Z::/InformaticsProject/Phase1/Stockbook Handover/2021_Git/digital-stockbook/www/ICES")

IrishStocks=read.csv("stock_lookupV2.csv", header=TRUE)
dim(IrishStocks)
head(IrishStocks)
colnames(IrishStocks) <- c("ecoregion","Old","species type" ,"New","x")

#~~ SM NOTE ~~
#update wd Oct 2019, and 2020, and 2021
#create a new loop, for the new years plot generation - 
#            2022 lines 485 to  
#            2021 lines 318 to 368
#            2020 lines 264 to 314
#            2019 lines 172 to 220  
#skip down to the start of this new section (for 2022 - line 485), to run the code to generate the required plots
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



#~~~~~~  2021  ~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~
#Sep 06th - changed the file path to '....2021_Git' 
#Manually add a 2021 folder in each ICES folder, for the downloaded png's to be saved into

#Stock Development Over Time 
#completed Oct 19th and committed to GitHub. Added 8 manual graphs from ICES
for(i in  IrishStocks$New){
  graphs <- getSAGGraphs(findAssessmentKey(i, 2021)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/ICES/SAG/2021/", i, ".png", sep=""),
      width = 850, height = 650)
  plot(graphs)
  dev.off()
}
#19/10
#An extra 6 graphs were downloaded (26 vs 20 on 22nd Sep)
#8 more were added manually. 
#list now matches what is in the ICES site

#22/09
#Long list of GETing...., then
#Error in png::readPNG(out) : libpng error: IDAT: CRC error
#In addition: There were 50 or more warnings (use warnings() to see the first 50)

#Graphs in folder but a lot are blank


#06/09
# GETing ... http://sg.ices.dk/StandardGraphsWebServices.asmx/getListStocks?year=2021
# Error in UseMethod("as.raster") : 
#   no applicable method for 'as.raster' applied to an object of class "c('ices_standardgraph_list', 'list')"
# In addition: Warning messages:
#   1: http status message: Server error: (500) Internal Server Error 
# 2: in SAG API - Missing parameter: token.


#Stock and Exploitation status
#completed Oct 19th and committed to GitHub. Added 2 manual graphs from ICES
for(i in  IrishStocks$New){
  graphs <- getStockStatusTable(findAssessmentKey(i, 2021)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/ICES/Status/2021/", i, ".png", sep=""),
      width = 950, height = 215)
  plot(graphs)
  dev.off()
}
#19/10 Was 27 pngs in Sept now 33
#2 more were added manually. 
#list now matches what is in the ICES site

#22/09
#Long list of GETing...., then a warning
#Graphs in folder but a lot are blank



#Quality of Assessment
for(i in  IrishStocks$New){
  print (i)
  graphs <- NA
  try (graphs <- getSSBHistoricalPerformance(findAssessmentKey(i, 2021)[1]))
  if (! is.na(graphs)) {
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/ICES/SSB/2021/", i, ".png", sep=""),
      width = 450, height = 350)
  plot(graphs)
  dev.off()
    }
}



#http://sg.ices.dk/StandardGraphsWebServices.asmx/getSSBHistoricalPerformance?assessmentKey=14534
#19/10
# Error in png::readPNG(out) : libpng error: IDAT: CRC error
# In addition: Warning messages:
#   1: http status message: Server error: (500) Internal Server Error 
# 2: in SAG API - Cannot convert NA to System.Int32.
# Parameter name: type ---> Input string was not in a correct format.

## SM note:  check ?getSSBHistoricalPerformance()

#22/09
#Long list of GETing...., then
# Error in png::readPNG(out) : libpng error: IDAT: CRC error
# In addition: Warning messages:
#   1: http status message: Server error: (500) Internal Server Error 
# 2: in SAG API - Cannot convert NA to System.Int32.
# Parameter name: type ---> Input string was not in a correct format

#Only 4 graphs in folder - one is blank


#Fishing mortality
# for(i in  IrishStocks$New){
#   graphs <- getFishingMortalityHistoricalPerformance(findAssessmentKey(i, 2021)[1])
#   png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/ICES/Fishmort/2021/", i, ".png", sep=""),
#       width = 450, height = 350)
#   plot(graphs)
#   dev.off()
# }

for(i in  IrishStocks$New){
  print (i)
  graphs <- NA
  try (graphs <- getFishingMortalityHistoricalPerformance(findAssessmentKey(i, 2021)[1]))
  if (! is.na(graphs)) {
    png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/ICES/Fishmort/2021/", i, ".png", sep=""),
        width = 450, height = 350)
    plot(graphs)
    dev.off()
  }
}


#22/09
#Error in png::readPNG(out) : libpng error: IDAT: CRC error
#Only 2 graphs in folder 

#RecruitHist
# for(i in  IrishStocks$New){
#   graphs <- getRecruitmentHistoricalPerformance(findAssessmentKey(i, 2021)[1])
#   png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/ICES/RecruitHist/2021/", i, ".png", sep=""),
#       width = 450, height = 350)
#   plot(graphs)
#   dev.off()
# }
#There were 11 warnings (use warnings() to see them)   warnings()


for(i in  IrishStocks$New){
  print (i)
  graphs <- NA
  try (graphs <- getRecruitmentHistoricalPerformance(findAssessmentKey(i, 2021)[1]))
  if (! is.na(graphs)) {
    png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2021_Git/www/ICES/RecruitHist/2021/", i, ".png", sep=""),
        width = 450, height = 350)
    plot(graphs)
    dev.off()
  }
}


#19/10
#An extra 6 graphs were downloaded (26 vs 20 on 22nd Sep)

#23/09?
#GETing ... http://sg.ices.dk/StandardGraphsWebServices.asmx/getRecruitmentHistoricalPerformance?assessmentKey=14557
#GETing ... http://sg.ices.dk/StandardGraphsWebServices.asmx/getListStocks?year=2021
# is this address correct??

#22/09
#Long list of GETing...., then a warning
#Graphs in folder but a ALL BLANK



assessmentKeys <- findAssessmentKey("had", 2015)
landings_img <- getLandingsGraph(assessmentKeys[1])
plot(landings_img)

landings_plots <- getLandingsGraph(assessmentKeys)
plot(landings_plots)



#~~~~~~  2022  ~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~
#Sep 02nd - changed the file path to '....2022_Git\digital-stockbook\' 
#Manually add a 2022 folder in each ICES folder, for the downloaded png's to be saved into

#UPDATE PATHS

#Stock Development Over Time (SAG) 
#completed Oct xxth and committed to GitHub. Added x manual graphs from ICES 8
for(i in  IrishStocks$New){
  graphs <- getSAGGraphs(findAssessmentKey(i, 2022)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2022_Git/digital-stockbook/www/ICES/SAG/2022/", i, ".png", sep=""),
      width = 850, height = 650)
  plot(graphs)
  dev.off()
}

#Adapted from HG code
if(F){
  cat("# Standard Graphs personal access token",
      "SG_PAT=cfab6390-10fd-421e-9512-398f9908be38",
      sep = "\n",
      file = "~/.Renviron_SG")
}
options(icesSAG.use_token = TRUE)

for(i in  IrishStocks$New){
  cat(IrishStocks$New,'\n')
  year <- 2022
  key <- findAssessmentKey(IrishStocks$New, year, regex = TRUE, full = FALSE)
  if(length(key)>0) {
    a <- try(getStockStatusTable(key))
    #HG png(paste0('./SAG plots/status_',stock,'.png'),width=dim(a[[1]])[2],height=dim(a[[1]])[1])
    #SM png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2022_Git/digital-stockbook/www/ICES/SAG/2022/", i, ".png", sep=""),
        #width = 850, height = 650)
    png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2022_Git/digital-stockbook/www/ICES/SAG/2022/", i, ".png", sep=""),
        width=dim(a[[1]])[2],height=dim(a[[1]])[1])
    
    plot(a)
    dev.off()}}
    
#     b <- try(getSAGGraphs(key))
#     if(class(b)=='try-error') next()
#     try(png(paste0('./SAG plots/summary_',stock,'.png'),width=dim(b[[1]])[2]*4,height=dim(b[[1]])[1]*4))
#     plot(b)  
#     dev.off()
#   }
# }



#Stock and Exploitation status (Status)
#completed Oct xxth and committed to GitHub. Added x manual graphs from ICES 2
for(i in  IrishStocks$New){
  graphs <- getStockStatusTable(findAssessmentKey(i, 2022)[1])
  png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2022_Git/digital-stockbook/www/ICES/Status/2022/", i, ".png", sep=""),
      width = 950, height = 215)
  plot(graphs)
  dev.off()
}



#Quality of Assessment (SSB) 
for(i in  IrishStocks$New){
  print (i)
  graphs <- NA
  try (graphs <- getSSBHistoricalPerformance(findAssessmentKey(i, 2022)[1]))
  if (! is.na(graphs)) {
    png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2022_Git/digital-stockbook/www/ICES/SSB/2022/", i, ".png", sep=""),
        width = 450, height = 350)
    plot(graphs)
    dev.off()
  }
}



for(i in  IrishStocks$New){
  print (i)
  graphs <- NA
  try (graphs <- getFishingMortalityHistoricalPerformance(findAssessmentKey(i, 2022)[1]))
  if (! is.na(graphs)) {
    png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2022_Git/digital-stockbook/www/ICES/Fishmort/2022/", i, ".png", sep=""),
        width = 450, height = 350)
    plot(graphs)
    dev.off()
  }
}




for(i in  IrishStocks$New){
  print (i)
  graphs <- NA
  try (graphs <- getRecruitmentHistoricalPerformance(findAssessmentKey(i, 2022)[1]))
  if (! is.na(graphs)) {
    png(file=paste("Z:/InformaticsProject/Phase1/Stockbook Handover/2022_Git/digital-stockbook/www/ICES/RecruitHist/2022/", i, ".png", sep=""),
        width = 450, height = 350)
    plot(graphs)
    dev.off()
  }
}




assessmentKeys <- findAssessmentKey("had", 2015)
landings_img <- getLandingsGraph(assessmentKeys[1])
plot(landings_img)

landings_plots <- getLandingsGraph(assessmentKeys)
plot(landings_plots)
