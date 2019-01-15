library(RODBC)
library(RColorBrewer)

#setwd('F:\\StockBooks\\_Stockbook2016\\Plots\\ValueTimeSeries')
setwd('H:/Stockbook/shiny/WIP/www/ValueTimeSeries/2015')
pal <- brewer.pal(9, 'Blues')

Q <- "
  select datepart(yy,a.DepartureDate) as Year
     ,case when FAQDesc in ('Tuna Albacore','Tuna Northern Bluefin') then FAQDesc else FSS_SpeciesName end as species
     ,sum(a.EstKgWeightSUM) / 1000 as LiveWtTonnes
     ,sum(a.EstKgWeightSUM * b.PricePerKgLiveWt) /sum(a.EstKgWeightSUM) as MeanPricePerKgLiveWt
  from declarations a
     join stecf.FishPrice_Division b
        on a.LogID = b.LogID
        and a.LogDeclarationID = b.LogDeclarationID
        and a.SpeciesID = b.SpeciesID
     join SpeciesLookup c
        on a.SpeciesID = c.SpeciesID
     join vessels d
        on a.VesselID = d.VesselID
  where d.VesselProvenance = 'ireland'
     and datepart(yy,departuredate) between 2005 and 2014
  group by datepart(yy,a.DepartureDate)
     ,case when FAQDesc in ('Tuna Albacore','Tuna Northern Bluefin') then FAQDesc else FSS_SpeciesName end
  order by datepart(yy,a.DepartureDate)
     ,case when FAQDesc in ('Tuna Albacore','Tuna Northern Bluefin') then FAQDesc else FSS_SpeciesName end
"

Q <- "
  select datepart(yy,a.DepartureDate) as Year
     ,FSS_SpeciesName as species
     ,sum(a.EstKgWeightSUM) / 1000 as LiveWtTonnes
     ,sum(a.EstKgWeightSUM * b.PricePerKgLiveWt) /sum(a.EstKgWeightSUM) as MeanPricePerKgLiveWt
  from declarations a
     join stecf.FishPrice_Division b
        on a.LogID = b.LogID
        and a.LogDeclarationID = b.LogDeclarationID
        and a.SpeciesID = b.SpeciesID
     join SpeciesLookup c
        on a.SpeciesID = c.SpeciesID
     join vessels d
        on a.VesselID = d.VesselID
  where d.VesselProvenance = 'ireland'
     and datepart(yy,departuredate) between 2005 and 2014
  group by datepart(yy,a.DepartureDate)
     ,FSS_SpeciesName
  order by datepart(yy,a.DepartureDate)
     ,FSS_SpeciesName
"

channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_20170327")
  land <- sqlQuery(channel,Q)
close(channel)

table(land$species)
land$YearFact <- factor(land$Year)

land <- subset(land,species!="Mackerel Chub\r\n\t")

Value2014 <- with(subset(land,Year==2014),tapply(LiveWtTonnes*MeanPricePerKgLiveWt/1000,list(species),sum))
Weight2014 <- with(subset(land,Year %in% c(2012, 2013,2014)),tapply(LiveWtTonnes,list(species),mean))


for(s in sort(unique(land$species))) {
  t <- with(subset(land,species==s),tapply(LiveWtTonnes*MeanPricePerKgLiveWt/1000,list(YearFact),sum))
  
  
  S <- ifelse(s=='Sole Black','Sole',s)
  S <- ifelse(s=='Monkfish','Anglerfish',S)
  
  png(paste0(sub('/','-',S),'.png'),3,2,'in',8,'white',600)
    par(mar=c(3,4,2.5,0))
    barplot(t,las=3,col=pal[7],ylab='Value (millions \u20ac)',main=paste(S,'- value of Irish landings'))
  dev.off()
  cat(s,'\n')
}
