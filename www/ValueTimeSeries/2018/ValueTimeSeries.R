

# ```{r}
# clear all memory
rm(list = ls())
# ```


library(RODBC)
library(RColorBrewer)
library(ggplot2)

setwd("//galwayfs03/FishData/StockBooks/_StockBook2018/Plots/ValueTimeSeries")

pal <- brewer.pal(9, 'Blues')

Q <- "
with decs as (
  select datepart(yy,a.LandingDate) as Year
      ,e.IcesDivisionCorrect as IcesDivision
     ,case when FAQDesc like ('%Tuna%') then 'Tuna' 
          when FAQDesc = 'Pelagic shark' then 'Pelagic Shark'
          else FSS_SpeciesName end as species
     ,sum(a.EstKgWeightSUM) / 1000 as LiveWtTonnes
     ,sum(a.EstKgWeightSUM * b.PricePerKgLiveWt) /sum(a.EstKgWeightSUM) as MeanPricePerKgLiveWt
  from declarations a
      join IcesLookup e
         on a.Division = e.IcesDivision
     join stecf.FishPrice_Division b
        on a.LogID = b.LogID
        and a.LogDeclarationID = b.LogDeclarationID
        and a.SpeciesID = b.SpeciesID
        and e.IcesDivisionCorrect = b.IcesDivisionCorrect
     join SpeciesLookup c
        on a.SpeciesID = c.SpeciesID
      join stecf.CombinedVesselTables d
         on a.VesselID = d.VesselID 
         and datepart(year,a.LandingDate) = d.ActivityYear
  where d.VesselProvenance = 'ireland'
     and datepart(yy,LandingDate) between 2008 and 2017
  group by datepart(yy,a.LandingDate)
      ,e.IcesDivisionCorrect
     ,case when FAQDesc like ('%Tuna%') then 'Tuna' 
          when FAQDesc = 'Pelagic shark' then 'Pelagic Shark'
          else FSS_SpeciesName end
   )

, decU10 as (
   select year
      ,species
      ,IcesDivision
      ,sum(landWt)/1000 as LiveWtTonnesInclU10 
   from(
      select datepart(year,d.LandingDate) as year
         ,i.IcesDivisionCorrect as IcesDivision
	      ,d.EstKgWeightSUM as landWt
         ,case when FAQDesc like ('%Tuna%') then 'Tuna' 
               when FAQDesc = 'Pelagic shark' then 'Pelagic Shark'
               else FSS_SpeciesName end as species
      from Declarations as d 
         join SpeciesLookup as s 
            on d.SpeciesID = s.SpeciesID 
          join stecf.CombinedVesselTables v
                  on d.VesselID = v.VesselID 
                  and datepart(year,d.LandingDate) = v.ActivityYear
         left join IcesLookup i
            on d.Division = i.IcesDivision
      where v.VesselProvenance='Ireland' 

      union all select datepart(year,d.EndDate) as year
         ,i.IcesDivisionCorrect as IcesDivision
	      ,d.EstLiveWeightSUM as landWt
         ,case when FAQDesc in ('Tuna Albacore','Tuna Northern Bluefin') then FAQDesc else FSS_SpeciesName end as species
      from Under10MeterLandings as d 
         join SpeciesLookup as s 
            on d.SpeciesID = s.SpeciesID
         left join IcesLookup i
            on d.Division = i.IcesDivision
      where datepart(year,d.EndDate) < 2015

      union all select datepart(year,d.LANDING_DATE) as year
         ,i.IcesDivisionCorrect as IcesDivision
	      ,d.EstKgLiveWt as landWt
        ,case when FAQDesc like ('%Tuna%') then 'Tuna' 
             when FAQDesc = 'Pelagic shark' then 'Pelagic Shark'
             else FSS_SpeciesName end as species
      from SalesNotesU10m as d 
         join SpeciesLookup as s 
            on d.SpeciesID = s.SpeciesID
         left join IcesLookup i
            on d.IcesDivision = i.IcesDivision
      where datepart(year,d.LANDING_DATE) >= 2015

   )x
   where species is not null
      and year between 2008 and 2017
   group by year, species ,IcesDivision
) 


select a.Year
   ,a.IcesDivision
   ,a.species
   ,a.LiveWtTonnes
   ,b.LiveWtTonnesInclU10
   ,a.MeanPricePerKgLiveWt
from decs a
   left join decU10 b
      on a.year = b.year
      and a.IcesDivision = b.IcesDivision
      and a.species = b.species
order by a.LiveWtTonnes-b.LiveWtTonnesInclU10 desc

"

channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_20180430")
  land <- sqlQuery(channel,Q)
close(channel)

table(land$species)
land$YearFact <- factor(land$Year)

land$species <- factor(ifelse(land$species=="Mackerel Chub\r\n\t","Mackerel Chub",as.character(land$species)))
#land <- subset(land,species!="Mackerel Chub\r\n\t")



    ## JW addded 4-9-2018
    write.csv(land, "IrishValue17.csv", row.names = T)



unlink('IrishValue17.txt')
sink('IrishValue17.txt')

for(s in sort(unique(land$species))) {

  t <- with(subset(land,species==s),tapply(LiveWtTonnesInclU10*MeanPricePerKgLiveWt/1000,list(YearFact),sum))
  
  
  S <- ifelse(s=='Sole Black','Sole',s)
  S <- ifelse(s=='Monkfish','Anglerfish',S)
  
  png(paste0(sub('/','-',S),'.png'),3,2,'in',8,'white',600)
    par(mar=c(3,4,2.5,0))
    barplot(t,las=3,col=pal[7],ylab='Value (millions €)',main=paste(S,'- value of Irish landings'))
  dev.off()
  cat(s,t['2017'],'\n')
}

sink(NULL)



for(s in sort(unique(land$species))) {
  a <- with(subset(land,species==s),aggregate(list(Wt=LiveWtTonnesInclU10),list(Div=IcesDivision),sum))
  i <- which(a$Wt/sum(a$Wt)>0.05)
  div <- a$Div[i]
  
  a <- with(subset(land,species==s & IcesDivision%in%div),aggregate(list(value=LiveWtTonnesInclU10*MeanPricePerKgLiveWt,Wt=LiveWtTonnesInclU10),list(Div=IcesDivision,Year=Year),sum))
  a$price <- a$value/a$Wt
  
  S <- ifelse(s=='Sole Black','Sole',s)
  S <- ifelse(s=='Monkfish','Anglerfish',S)
  
  png(paste0('Price_',sub('/','-',S),'.png'),6,4,'in',8,'white',600)
    p <- ggplot(a) + geom_line(aes(Year,price,col=Div)) + ylim(0,max(a$price))
    plot(p)
  dev.off()
}


library(dplyr)
library(tidyr)

land1 <- land %>% group_by(Year,species) %>% summarise(Value=sum(LiveWtTonnesInclU10*MeanPricePerKgLiveWt/1000))
land1 <- land1 %>% group_by(Year) %>% mutate(rank=rank(1/Value))

land2 <- land %>% group_by(species) %>% summarise(Value=sum(LiveWtTonnesInclU10*MeanPricePerKgLiveWt/1000))
land2 <- land2 %>% group_by() %>% mutate(rank=rank(1/Value))

land0 <- subset(land1,Year==max(land1$Year))
land0 <- land0 %>% arrange(rank)
s0 <- land0$species
land1$speciesFact <- ordered(land1$species,levels=as.character(s0))

s <- s0[1:30]
png('WinnersAndLosers.png',9,6,'in',res=300)
  ggplot(subset(land1,species%in%s),aes(Year,rank)) + geom_line(col='grey') + geom_text(aes(label=rank),size=2) + facet_wrap(~speciesFact) + ylim(40,0)
dev.off()

s <- s0[31:60]
png('WinnersAndLosers2.png',9,6,'in',res=300)
  ggplot(subset(land1,species%in%s),aes(Year,rank)) + geom_line(col='grey') + geom_text(aes(label=rank),size=2) + facet_wrap(~speciesFact) + ylim(80,0)
dev.off()

