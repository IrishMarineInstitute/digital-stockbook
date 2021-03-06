library(RODBC)
library(RColorBrewer)
library(mapplots)

setwd("//Galwayfs03/FishData/StockBooks/_StockBook2018/Plots/Quota")
file <- "//Galwayfs03/FishData/StockBooks/_StockBook2018/Reference files/2018 FIDES Quota June18.xlss"

tab <- "Quotas"

#channel <- odbcConnectExcel(file)
#quota <- sqlFetch(channel,tab)
#close(channel)

quota <- read.csv('//Galwayfs03/FishData/StockBooks/_StockBook2018/Reference files/2018 FIDES Quota June18.csv')

names(quota) <- gsub(" ",".",names(quota))
quota$Stock <- with(quota,(paste(Species.Code,Area.Code)))

quota$q <- formatC(quota$Initial.Quantity, big.mark=",", digits = 0, format = "f")

stocks <- unique(quota$Stock)

quota1 <- subset(quota,!is.na(Initial.Quantity))

levels(quota1$Country.Code)[levels(quota1$Country.Code)=="AMS"] <- "OTH"


# check which countries never occur together
# they can be given the same colour
out <- NULL
for(s in stocks)
out <- rbind(out,with(subset(quota1,Stock==s),expand.grid(as.character(Country.Code),as.character(Country.Code))))
table(out)

#'AMS' = others
eu <- c('OTH','BEL','DEU','DNK','ESP','EST','FIN','FRA','GBR','GRC','IRL','LTU','LVA','NLD','POL','PRT','SWE')


pal <- data.frame(Country.Code=sort(unique(as.character(quota1$Country.Code))))
pal$Index <- NA
pal$Index[pal$Country.Code%in%c('OTH','BEL')] <- 1
pal$Index[pal$Country.Code%in%c('DEU')] <- 2
pal$Index[pal$Country.Code%in%c('DNK','GRC')] <- 3
pal$Index[pal$Country.Code%in%c('NLD')] <- 4
pal$Index[pal$Country.Code%in%c('EST','FIN')] <- 5
pal$Index[pal$Country.Code%in%c('POL')] <- 6
pal$Index[pal$Country.Code%in%c('FRO')] <- 7
pal$Index[pal$Country.Code%in%c('NOR')] <- 8
pal$Index[pal$Country.Code%in%c('PRT')] <- 9
pal$Index[pal$Country.Code%in%c('LTU')] <- 10
pal$Index[pal$Country.Code%in%c('LVA')] <- 11
pal$Index[pal$Country.Code%in%c('ESP')] <- 12
pal$Index[pal$Country.Code%in%c('GBR')] <- 13
pal$Index[pal$Country.Code%in%c('FRA')] <- 14
pal$Index[pal$Country.Code%in%c('IRL')] <- 15
pal$Index[pal$Country.Code%in%c('SWE')] <- 16

col <- c(brewer.pal(12,'Set3'),brewer.pal(8,'Set1')[c(1,2,3,7)])
plot(1)
legend('center',legend=1:16,fill=col,ncol=2)

pal$col <- col[pal$Index]

legend('center',legend=pal$Country.Code,fill=pal$col,ncol=3)

s<-"MAC 2CX14-" 
s <- "NEP 5BC6."

for(s in stocks){
  cat(s)
  s1 <- sub('[*]','#',s)
  s1 <- sub('[/]','_',s1)
  
  png(paste0(s1,'.png'),3,2.5,'in',8,res=600)
    par(mar=c(0,0,1,0))
    q <- subset(quota1,Stock==s)
    q <- merge(q,pal)
    q1 <- subset(q,!Country.Code%in%c('TAC','EEC') & Initial.Quantity>0 & Country.Code%in%eu)
    q2 <- subset(q,Country.Code%in%c('EEC'))
    q3 <- subset(q,Country.Code%in%c('TAC'))
    if(round(sum(q1$Initial.Quantity))!=round(sum(q2$Initial.Quantity))) warning(paste(s,'',sum(q1$Initial.Quantity),sum(q2$Initial.Quantity)))
    plot.new()
    plot.window(c(0,1.5),0:1)
    if(sum(q1$Initial.Quantity)>0) with(q1, draw.pie(0.5,0.5,array(Initial.Quantity,c(1,length(q))),0.4,col=col)) else plot.window(0:1,0:1)
    leg <- with(q1,paste0(Country.Code,' ',q,' t'))
    if(nrow(q3)==0)  tit <- paste0('EU ',q2$q,' t') else 
    if(abs(q2$Initial.Quantity-q3$Initial.Quantity)<=1) tit <- paste0('TAC ',q3$q,' t') else
    tit <- paste0('EU ',q2$q,' t','\nTAC ',q3$q,' t')
#    plot.new()
    plot.window(0:1,0:1)
    legend('right',legend=leg,fill=q1$col,title=tit,bty='n')
    mtext(q1$Area.Short[1],3,-1,outer=T,cex=1)
  dev.off()
}


warnings()
