library(RODBC)
library(RColorBrewer)
library(dplyr)
library(DataCombine)

setwd('F:\\StockBooks\\_StockBook2018\\Plots\\LandingsByGear\\Nephrops')


pal <- brewer.pal(9, 'Blues')


qry = "
            select b.FU
                     ,[Year]
                     ,foCatEu6
                     ,sum(landwt) as LiveWt
            from COST_CL_ops_Vw a
                  left join StatrectFU b
                  on a.rect = b.icesrectangle
                      where taxon = 'Nephrops norvegicus'
            group by b.FU
               ,a.[year]
               ,foCatEu6
"
channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_20180305")
  met=sqlQuery(channel,qry)
close(channel)

met$gear1 <- substring(met$foCatEu6,1,3)
table(met$gear1)
met$gear <- NULL
met$gear <- ifelse(met$gear1%in%c('DRB','FPO','LHP','MIS'),'Others',met$gear)
met$gear <- ifelse(met$gear1%in%c('SSC'),'Seines',met$gear)
met$gear <- ifelse(met$gear1%in%c('PTM'),'Midwater trawls',met$gear)
met$gear <- ifelse(met$gear1%in%c('GNS'),'Gillnets',met$gear)
met$gear <- ifelse(met$gear1%in%c('OTB'),'Bottom otter trawls',met$gear)
met$gear <- ifelse(met$gear1%in%c('TBB'),'Beam trawls',met$gear)
sum(is.null(met$gear)) # should be zero

met$gear <- ordered(met$gear, c("Others", "Seines", "Midwater trawls", "Gillnets", "Bottom otter trawls", 
"Beam trawls")
)

# changing NAs for NotFU
levels(met$FU) <- c(levels(met$FU), "NotFU")
met$FU[is.na(met$FU)] <- "NotFU"

#scale due to unallocated catches in 2017 /FU16

dat <- read.csv("nep_fus_landings.csv")


lan <- met %>% filter(Year=="2017") %>% group_by(FU) %>% summarise(tot=sum(LiveWt)/1000)

lan <- subset(lan, FU %in% c(14,15,16,17,19,22,2021,"NotFU"))
dat <- subset(dat, FU %in% c(14,15,16,17,19,22,2021,"NotFU"))


lan1 <- merge(dat,lan,by="FU")
lan1 <- lan1 %>% group_by(FU) %>% mutate(scale=X2017/tot)

scale <-lan1[ , c(1, 7)]

#2017 only
land=filter(met, Year=="2017")
land <- merge(land, scale, by="FU")

land <- land %>% mutate(LiveWt1= LiveWt*scale)

table(land$gear)

for(s in levels(factor(land$FU))) {
  t <- with(subset(land,FU==s), tapply(LiveWt1,gear,sum))
  t <- ifelse(is.na(t),0,t)
  cat(s,'\n')
  png(paste0(s,'.png'),4,1.75,'in',8,'white',600)
    par(mar=c(4,8.5,2,1))
    barplot(sort(t)/1000,xlim=extendrange(c(0,max(t)/1000),f=0.02),horiz=T,col=pal[7],border=pal[9],space=0.5,las=1,xlab='Tonnes',main = 'Irish landings by gear type')
    box()
  dev.off()
}


