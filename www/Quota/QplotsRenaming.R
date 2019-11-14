###########Renaming Quata pie chart images##########
#########################################################
rm(list=ls())
library(png)

setwd('L:/StockBooks/_StockBook2019/Plots/Quota')#fishdata folder with Quota pie charts

n<-read.csv("C:/Users/okalinina/Desktop/QSimagesNames.csv")#read in TAC and corresponding Stock names

for(i in 1:dim(n)[1]){
img<-readPNG(list.files(pattern=paste(n[1][i,])))
writePNG(img,paste0("C:/Users/okalinina/Desktop/2019/",n[2][i,],".png"))}
