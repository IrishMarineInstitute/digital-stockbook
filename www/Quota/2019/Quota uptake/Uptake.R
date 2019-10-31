library(RODBC)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_current")
  land <- sqlQuery(channel,readChar('Decs.sql',1e6))
close(channel)

#quota <- read.csv('//Galwayfs03/FishData/StockBooks/_StockBook2018/Reference files/quota files/EU_Quota_1980_2018.csv')
quota0 <- read.csv('//Galwayfs03/FishData/Data for ICESWG/2018/MARE FIDES quota databse/downloads/EU_Quota_1980_2018.csv')

filename <- '//Galwayfs03/FishData/StockBooks/_StockBook2019/Reference files/Quota files/qfexport2019.xls'
channel <- odbcConnectExcel(filename)
  quota <- sqlFetch(channel,'Quotas')
close(channel)

names(quota) <- gsub(" ",".",names(quota))
quota1 <- rbind(quota,quota0)
rm(quota)
quota <- quota1

quota$Stock <- with(quota,(paste(Species.Code,Area.Code)))
quota <- subset(quota,!is.na(Initial.Quantity) & Level.Code=='IRL')

unique(subset(quota,Species.Code=='COD')$Stock)
stocks <- unique(subset(quota,Definition.Year==2018)$Stock)

s <- "ANF 07."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,4)=='27.7' & species=='Monkfish')

tempfun <- function(s,a,l) {
  a <- l %>% group_by(year,week) %>% summarise(landWt=sum(landWt))
  a <- left_join(a,q,c('year'='Definition.Year'))
  a <- a %>% arrange(year,week) %>% group_by(year) %>% mutate(landCumSum=cumsum(landWt))
  a <- a %>% mutate(uptake=landCumSum/Initial.Quantity/1000)
  a$lwd <- ifelse(a$year==max(a$year),1.5,0.5)
  b <- a %>% group_by(year) %>% mutate(maxweek=max(week))
  b <- subset(b,week==maxweek)
  p1 <- ggplot() + geom_line(aes(week,uptake,col=factor(year)),a,size=a$lwd) + 
    geom_text(aes(week,uptake,label=year,col=factor(year)),b,hjust=0,size=3) +
    geom_hline(yintercept=1) + ggtitle(s) + ylab('Proportion uptake initial quota')
  p1

  maxweek <- max(subset(a,year==max(year))$week)
  a1 <- subset(a,week<=maxweek)
  a2 <- a1 %>% group_by(year,Initial.Quantity,Adapted.Quota) %>% summarise(landCumSum=max(landCumSum))
  a3 <- with(a2,rbind(data.frame(year,variable='Adjusted Quota',tonnes=Adapted.Quota)
                      ,data.frame(year,variable='Initial Quota',tonnes=Initial.Quantity)
        ))
  uptake <- paste('Uptake week',maxweek)
  p2 <- ggplot(a3) + geom_bar(aes(year,tonnes,col=variable),stat='identity',position='identity',bg='grey') +
    geom_line(data=a2,aes(year,landCumSum/1000,colour=uptake)) +
    geom_point(data=a2,aes(year,landCumSum/1000,colour=uptake)) +
    ylim(0,max(a3$tonnes)) +
    ggtitle(s)
    
  
  out <- grid.arrange(p1,p2,nrow=1)
  
  png(paste0(sub('[*]','',sub('/','',s)),'.png'),960,480)
    plot(out)
  dev.off()
  return(out)
}

tempfun(s,a,l)

s <- "ALB AN05N"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,2)=='27' & species=='Tuna')
tempfun(s,a,l)

s <- "ANF 07."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,4)=='27.7' & species=='Monkfish')
tempfun(s,a,l)

s <- "ANF 56-14"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,4)=='27.6' & species=='Monkfish')
tempfun(s,a,l)

s <- "BOR 678-"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,4)%in%c('27.6','27.7','27.8') & species=='Boarfish')
tempfun(s,a,l)

s <- "COD 07A."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.a") & species=='Cod')
tempfun(s,a,l)

s <- "COD 5BE6A"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
q$Initial.Quantity <- 1 # uptake in tonnes, not relative to quota
l <- subset(land,area%in%c("27.6.a") & species=='Cod')
tempfun(s,a,l)

s <-  "COD 5W6-14" 
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.6.b") & species=='Cod')
tempfun(s,a,l)

s <- "COD 7XAD34"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.b","27.7.c","27.7.e","27.7.f","27.7.g","27.7.h","27.7.j","27.7.k") & 
              species=='Cod')
tempfun(s,a,l)

s <- "HAD 07A."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.a") & species=='Haddock')
tempfun(s,a,l)

s <- "HAD 5BC6A."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.6.a") & species=='Haddock')
tempfun(s,a,l)

s <- "HAD 6B1214"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.6.b") & species=='Haddock')
tempfun(s,a,l)

s <- "HAD 7X7A34"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.b","27.7.c","27.7.e","27.7.d","27.7.f","27.7.g","27.7.h","27.7.j","27.7.k") & 
              species=='Haddock')
tempfun(s,a,l)

s <- "HER 07A/MM"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.a") & 
              species=='Herring')
tempfun(s,a,l)

s <- "HER 6AS7BC"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.6.a","27.7.b","27.7.c") & 
              species=='Herring')
tempfun(s,a,l)

s <- "HER 7G-K."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.g","27.7.h","27.7.j","27.7.k") & 
              species=='Herring')
tempfun(s,a,l)

s <- "HKE 571214" 
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,2)=='27' & species=='Hake')
tempfun(s,a,l)

s <- "JAX 2A-14"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,2)=='27' & species=='Horse Mackerel')
tempfun(s,a,l)

s <- "LEZ 07."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.a","27.7.b","27.7.c","27.7.e","27.7.d","27.7.f","27.7.g","27.7.h","27.7.j","27.7.k") & 
              species=='Megrim')
tempfun(s,a,l)

s <- "LEZ 56-14"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.6.a","27.6.b") & 
              species=='Megrim')
tempfun(s,a,l)

s <- "LIN 6X14."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,2)=='27' & species=='Ling')
tempfun(s,a,l)

s <- "MAC *4A-EN"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,4)=='27.4' & species=='Mackerel')
tempfun(s,a,l)

s <- "MAC 2CX14-"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,4)!='27.4' & species=='Mackerel')
tempfun(s,a,l)

s <- "NEP 07."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.a","27.7.b","27.7.c","27.7.e","27.7.d","27.7.f","27.7.g","27.7.h","27.7.j","27.7.k") & 
              species=='Nephrops')
tempfun(s,a,l)

s <- "PLE 07A."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.a") & species=='Plaice')
tempfun(s,a,l)

s <- "PLE 7BC."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.b","27.7.c") & species=='Plaice')
tempfun(s,a,l)

s <- "PLE 7FG."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.f","27.7.g") & species=='Plaice')
tempfun(s,a,l)

s <- "PLE 7HJK."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.h","27.7.j","27.7.k") & species=='Plaice')
tempfun(s,a,l)

s <- "POK 56-14"           
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,4)=="27.6" & species=='Saithe')
tempfun(s,a,l)

s <- "POK 7/3411"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,4)=="27.7" & species=='Saithe')
tempfun(s,a,l)

s <- "POL 07."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,4)=="27.7" & species=='Pollack')
tempfun(s,a,l)

s <- "POL 56-14"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,4)=="27.6" & species=='Pollack')
tempfun(s,a,l)

s <- "SOL 07A."            
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.a") & species=='Sole Black')
tempfun(s,a,l)

s <- "SOL 7BC."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.b","27.7.c") & species=='Sole Black')
tempfun(s,a,l)

s <- "SOL 7FG."            
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.f","27.7.g") & species=='Sole Black')
tempfun(s,a,l)

s <- "SOL 7HJK."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.h","27.7.j","27.7.k") & species=='Sole Black')
tempfun(s,a,l)

s <- "WHB 1X14"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,substring(area,1,2)=="27" & species=='Blue Whiting')
tempfun(s,a,l)

s <- "WHG 56-14"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.6.a","27.6.b") & species=='Whiting')
tempfun(s,a,l)

s <- "WHG 07A."
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.a") & species=='Whiting')
tempfun(s,a,l)

s <- "WHG 7X7A-C"
q <- subset(quota,Stock==s)[,c('Definition.Year','Initial.Quantity','Adapted.Quota')]
l <- subset(land,area%in%c("27.7.b","27.7.c","27.7.e","27.7.d","27.7.f","27.7.g","27.7.h","27.7.j","27.7.k") & 
              species=='Whiting')
tempfun(s,a,l)

