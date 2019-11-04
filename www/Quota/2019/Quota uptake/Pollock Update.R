library(RODBC)
library(ggplot2)
library(dplyr)
library(tidyr)

Q <- "
select year
   ,vesselname
   ,gear
   ,sum(landWt) as landWt 
from(
   select datepart(year,d.LandingDate) as year
      ,v.VesselName
	   ,case when substring(d.GearType,1,3) in ('GEN','GN ','GNC','GND','GNS','GTN','GTR') then 'GN '
         when substring(d.GearType,1,3) in ('OTB','OTT','PTB','TBN','OT ','TB ') then 'OTB'
         when substring(d.GearType,1,3) in ('PS ','PS2','SDN','SPR','SSC','SX ') then 'SSC'
         when substring(d.GearType,1,3) in ('TBB') then 'TBB'
         else 'OTH' end as gear
	   ,d.EstKgWeightSUM as landWt
   from Declarations as d 
      join SpeciesLookup as s 
         on d.SpeciesID = s.SpeciesID 
      join stecf.CombinedVesselTables v
         on d.VesselID = v.VesselID 
         and datepart(year,d.LandingDate) = v.ActivityYear
   where v.VesselProvenance='Ireland' 
      and d.PresentationType <> 'Below minimum conservation reference size'
      and s.FAQCode = 'POL'
      and datepart(year,d.LandingDate) between 2017 and 2018

    union all
   select datepart(year,d.LANDING_DATE) as year
      ,d.VESSEL_NAME as VesselName
      ,'U10' as gear
	   ,d.EstKgLiveWt as landWt
   from SalesNotesU10m as d 
      join SpeciesLookup as s 
         on d.SpeciesID = s.SpeciesID
      left join MarinePortLookup as p
         on	d.MarinePortID = p.MarinePortID
      join stecf.CombinedVesselTables as v
         on d.IFIS_VESSEL_ID = v.VesselID
         and datepart(year,d.LANDING_DATE) = v.ActivityYear
      where d.PRESENTATION_DESC <> 'Below minimum conservation reference size' -- does not occurl just to futureproof
         and v.vesselprovenance = 'Ireland'
         and s.FAQCode = 'POL'
         and datepart(year,d.LANDING_DATE) between 2017 and 2018
	  )x
group by year
   ,vesselname
   ,gear
"


channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_current")
  dec <- sqlQuery(channel,Q)
close(channel)

dec1 <- dec %>% group_by(year,gear) %>% summarise(land=sum(landWt)/1000)
ggplot(dec1,aes(gear,land,col=factor(year))) + geom_bar(stat='identity',position='dodge')
# a good bit of the landings is still outstanding (no U10 data yet)

subset(dec1,year==2017) %>% group_by(gear=='U10') %>% summarise(sum(land))
265/(265+661) #29%
# still looking at under-shoot of 20%

dec1 <- dec %>% spread(year,landWt)
names(dec1)[3:4] <- paste0('X',names(dec1)[3:4])
ggplot(subset(dec1,gear!='U10'),aes(X2018,X2017,col=gear)) + geom_point()
      
subset(dec1,gear!='U10' & X2017 > 20000 & X2018 < 15000)
