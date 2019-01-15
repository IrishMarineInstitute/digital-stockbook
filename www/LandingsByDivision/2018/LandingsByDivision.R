library(RODBC)
library(RColorBrewer)

setwd('y:\\StockBooks\\_Stockbook2018\\Plots\\LandingsByDivision')

pal <- brewer.pal(9, 'Blues')

Q <- "
   select year
      ,species
      ,Division
      ,sum(landWt) as landWt 
   from(
      select datepart(year,d.LandingDate) as year
         ,i.IcesDivisionCorrect as Division
	      ,d.EstKgWeightSUM as landWt
         ,case when FAQDesc in ('Tuna Albacore','Tuna Northern Bluefin') then FAQDesc else FSS_SpeciesName end as species
      from Declarations as d 
         join SpeciesLookup as s 
            on d.SpeciesID = s.SpeciesID 
         join Vessels as v 
            on d.VesselID = v.VesselID 
         left join IcesLookup i
            on d.Division = i.IcesDivision
      where v.VesselProvenance='Ireland' 

      union all select datepart(year,d.EndDate) as year
         ,i.IcesDivisionCorrect as Division
	      ,d.EstLiveWeightSUM as landWt
         ,case when FAQDesc in ('Tuna Albacore','Tuna Northern Bluefin') then FAQDesc else FSS_SpeciesName end as species
      from Under10MeterLandings as d 
         join SpeciesLookup as s 
            on d.SpeciesID = s.SpeciesID
         left join IcesLookup i
            on d.Division = i.IcesDivision
   )x
   where species is not null
      and year between 2015 and 2017
   group by year, species ,Division
   order by year, species ,Division

"

channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_20180305")
  land <- sqlQuery(channel,Q)
close(channel)

table(land$species)
table(land$Division)

div_order <- c("IIa", "IIb", "IVa", "IVb", "IVc","Vb1", "VIa", "VIb", 
"VIIa", "VIIb", "VIIc", "VIId", "VIIe", "VIIf", "VIIg", "VIIh", 
"VIIj", "VIIk","VIIIa", "VIIIb", "VIIIc", "VIIId", "VIIIe", "IXb", "Xb", "XIIa", 
"XIIb", "XIIc",'Other')

div_order1 <- c("2a", "2b", "4a", "4b", "4c","5b1", "6a", "6b", 
"7a", "7b", "7c", "7d", "7e", "7f", "7g", "7h", 
"7j", "7k","8a", "8b", "8c", "8d", "8e", "9b", "10b", "12a", 
"12b", "12c",'other')

data.frame(div_order,div_order1)

#check that i have all divisions
unique(land$Division)[!unique(land$Division)%in%div_order]

unlink('IrishLandings2015-17.txt')
sink('IrishLandings2015-17.txt')
for(s in levels(land$species)) {
  land0 <- subset(land,species==s)
  t0 <- with(land0, tapply(landWt,list(as.character(Division)),sum))
  t0 <- ifelse(is.na(t0),0,t0)
  t1 <- cumsum(sort(t0,F))
  d <- rev(names(t1)[t1 > 0.01 * sum(t0)])[1:6]
  if(length(t1>5)) land0$division <- ifelse(land0$Division%in%d,as.character(land0$Division),'Other')
  land0$division <- ordered(land0$division,div_order)

  land0 <- merge(land0,data.frame(division=div_order,division1=div_order1))
  land0$division1 <- ordered(land0$division1,div_order1)
    
  
  t <- with(droplevels(land0), tapply(landWt,list(year,division1),sum))
  t <- ifelse(is.na(t),0,t)
  S <- ifelse(s=='Sole Black','Sole',s)
  S <- ifelse(s=='Monkfish','Anglerfish',S)
  S <- ifelse(s=='Tuna Northern Bluefin','Bluefin tuna',S)

  png(paste0(sub('/','-',S),'.png'),3,2,'in',8,'white',600)
#    par(mar=c(3.5,5,3,1))
#    barplot(t/1000,beside=T,las=3,col=pal[c(3,5,7)],ylab='Tonnes',legend=T,main=paste(S,'landings by Irish vessels'))
    par(mar=c(2.5,4,3,0))
    barplot(t/1000,beside=T,col=pal[c(3,5,7)],ylab='Tonnes',legend=T,main=paste(S,'landings by Irish vessels'))
  dev.off()
  cat(s,round(rowSums(t)/1000),'\n')
}
sink(NULL)


# nephrops

nep <- read.csv('\\\\galwayfs03\\fishdata\\StockBooks\\_Stockbook2018\\Plots\\nep_fus_landings.csv')
nep$FU1 <- ifelse(nep$FU%in%c('15','16','17','19','22','2021'),as.character(nep$FU),'other')

t <- tapply(nep$X2015,nep$FU1,sum)
t <- rbind(t,tapply(nep$X2016,nep$FU1,sum))
t <- rbind(t,tapply(nep$X2017,nep$FU1,sum))
rownames(t) <- 2015:2017

png('NephropsFU.png',3,2,'in',8,'white',600)
    par(mar=c(2.5,4.1,3,0.1))
    barplot(t/1000,beside=T,col=pal[c(3,5,7)],ylab='Tonnes',legend=T,main=paste('Nephrops','landings by Irish vessels'),args.legend=list(x='top'))
dev.off()
