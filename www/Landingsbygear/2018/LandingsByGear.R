library(RODBC)
library(RColorBrewer)

setwd('y:\\StockBooks\\_Stock book 2018\\Plots\\LandingsByGear')

pal <- brewer.pal(9, 'Blues')

Q <- "
   select stock
      ,gear
      ,sum(landWt) as landWt 
   from(
      select datepart(year,d.LandingDate) as year
         ,case when substring(d.GearType,1,1) = 'G' then 'Gillnets'
            when substring(d.GearType,1,3) in ('TBB') then 'Beam trawls'
            when substring(d.GearType,1,3) in ('OTM','PTM','PT ') then 'Midwater trawls'
            when substring(d.GearType,1,1) in ('O','T') then 'Bottom otter trawls'
            when substring(d.GearType,1,3) in ('PTB') then 'Bottom otter trawls'
            when substring(d.GearType,1,1) in ('P','S') then 'Seines'
            else 'Others' end as gear
	      ,d.EstKgWeightSUM as landWt
         ,st.stock as stock
      from Declarations as d 
         join SpeciesLookup as s 
            on d.SpeciesID = s.SpeciesID 
         join Vessels as v 
            on d.VesselID = v.VesselID 
         left join MarinePortLookup p 
            on d.LandingPortID = p.MarinePortID
         left join IcesLookup i
            on d.Division = i.IcesDivision
         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME
         left join Stockman2015_views..IntercatchStocks st
            on st.Areas = i.IcesDivisionIC
            and st.Species = sl.species_code
      where v.VesselProvenance='Ireland' 

      union all select datepart(year,d.EndDate) as year
         ,'Others' as gear
	      ,d.EstLiveWeightSUM as landWt
         ,st.stock as stock
      from Under10MeterLandings as d 
         join SpeciesLookup as s 
            on d.SpeciesID = s.SpeciesID
         left join MarinePortLookup p 
            on	d.MarinePortID = p.MarinePortID
         left join IcesLookup i
            on d.Division = i.IcesDivision
         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME
         left join Stockman2015_views..IntercatchStocks st
            on st.Areas = i.IcesDivisionIC
            and st.Species = sl.species_code
   )x
   where stock is not null
      and year = 2017
   group by stock
      ,gear
   order by stock
      ,gear
"

channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_20180305")
  land <- sqlQuery(channel,Q)
close(channel)

subset(land,stock=='her.27.6a7bc')


table(land$stock)
table(land$gear)

land$gear <- ordered(land$gear, c("Others", "Seines", "Midwater trawls", "Gillnets", "Bottom otter trawls", 
"Beam trawls")
)

for(s in levels(land$stock)) {
  t <- with(subset(land,stock==s), tapply(landWt,gear,sum))
  t <- ifelse(is.na(t),0,t)
  cat(s,'\n')
  png(paste0(s,'.png'),4,1.75,'in',8,'white',600)
    par(mar=c(4,8.5,2,1))
    barplot(sort(t)/1000,xlim=extendrange(c(0,max(t)/1000),f=0.02),horiz=T,col=pal[7],border=pal[9],space=0.5,las=1,xlab='Tonnes',main = 'Irish landings by gear type')
    box()
  dev.off()
}



# or custom one, something like this:
"
select stock
      ,gear
      ,sum(landWt) as landWt 
   from(
      select datepart(year,d.LandingDate) as year
         ,case when substring(d.GearType,1,1) = 'G' then 'Gillnets'
            when substring(d.GearType,1,3) in ('TBB') then 'Beam trawls'
            when substring(d.GearType,1,3) in ('OTM') then 'Midwater trawls'
            when substring(d.GearType,1,1) in ('O','T') then 'Bottom otter trawls'
            when substring(d.GearType,1,1) in ('P','S') then 'Seines'
            else 'Others' end as gear
	      ,d.EstKgWeightSUM as landWt
         ,'stockname' as stock
      from Declarations as d 
         join SpeciesLookup as s 
            on d.SpeciesID = s.SpeciesID 
         join Vessels as v 
            on d.VesselID = v.VesselID 
         left join MarinePortLookup p 
            on d.LandingPortID = p.MarinePortID
         left join IcesLookup i
            on d.Division = i.IcesDivision

         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME

      where v.VesselProvenance='Ireland' 
         and FSS_Speciesname like ('%ray%') and coalesce(i.IcesDivisionCorrect,d.Division) in ('VIa','VIb','VIIa','VIIb','VIIc','VIId','VIIe','VIIf','VIIg','VIIh','VIIj','VIIk')

      union all select datepart(year,d.EndDate) as year
         ,'Others' as gear
	      ,d.EstLiveWeightSUM as landWt
         ,'stockname' as stock
      from Under10MeterLandings as d 
         join SpeciesLookup as s 
            on d.SpeciesID = s.SpeciesID
         left join MarinePortLookup p 
            on	d.MarinePortID = p.MarinePortID
         left join IcesLookup i
            on d.Division = i.IcesDivision

         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME

       where 1=1
         and FSS_Speciesname like ('%ray%') and coalesce(i.IcesDivisionCorrect,d.Division) in ('VIa','VIb','VIIa','VIIb','VIIc','VIId','VIIe','VIIf','VIIg','VIIh','VIIj','VIIk')

   )x
   where year = 2017
   group by stock
      ,gear
   order by stock
      ,gear
"

#####################################################  Code for Plaice and  Sole in VI ######################################################
Q <- "
select stock
,gear
,sum(landWt) as landWt 
from(
select datepart(year,d.LandingDate) as year
,case when substring(d.GearType,1,1) = 'G' then 'Gillnets'
when substring(d.GearType,1,3) in ('TBB') then 'Beam trawls'
when substring(d.GearType,1,3) in ('OTM') then 'Midwater trawls'
when substring(d.GearType,1,1) in ('O','T') then 'Bottom otter trawls'
when substring(d.GearType,1,1) in ('P','S') then 'Seines'
else 'Others' end as gear
,d.EstKgWeightSUM as landWt
,FSS_Speciesname as stock
from Declarations as d 
join SpeciesLookup as s 
on d.SpeciesID = s.SpeciesID 
join Vessels as v 
on d.VesselID = v.VesselID 
left join MarinePortLookup p 
on d.LandingPortID = p.MarinePortID
left join IcesLookup i
on d.Division = i.IcesDivision

         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME

where v.VesselProvenance='Ireland' 
and FSS_Speciesname in ( 'Sole Black','Plaice') and coalesce(i.IcesDivisionCorrect,d.Division) in ('VIa','VIb')

union all select datepart(year,d.EndDate) as year
,'Others' as gear
,d.EstLiveWeightSUM as landWt
,FSS_Speciesname as stock
from Under10MeterLandings as d 
join SpeciesLookup as s 
on d.SpeciesID = s.SpeciesID
left join MarinePortLookup p 
on	d.MarinePortID = p.MarinePortID
left join IcesLookup i
on d.Division = i.IcesDivision
         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME
where 1=1
and FSS_Speciesname in ( 'Sole Black','Plaice') and coalesce(i.IcesDivisionCorrect,d.Division) in ('VIa','VIb')

)x
where year = 2017
group by stock
,gear
order by stock
,gear

"

channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_20180305")
  land <- sqlQuery(channel,Q)
close(channel)

table(land$stock)
table(land$gear)

land$gear <- ordered(land$gear, c("Others", "Seines", "Midwater trawls", "Gillnets", "Bottom otter trawls", 
"Beam trawls")
)

for(s in levels(land$stock)) {
  t <- with(subset(land,stock==s), tapply(landWt,gear,sum))
  t <- ifelse(is.na(t),0,t)
  cat(s,'\n')
  png(paste0(s,' in VI.png'),4,1.75,'in',8,'white',600)
    par(mar=c(4,8.5,2,1))
    barplot(sort(t)/1000,xlim=extendrange(c(0,max(t)/1000),f=0.02),horiz=T,col=pal[7],border=pal[9],space=0.5,las=1,xlab='Tonnes',main = 'Irish landings by gear type')
    box()
  dev.off()
}
############################ Skates &  Rays##################################

Q <- "
    select stock
    ,gear
    ,sum(landWt) as landWt 
    from(
    select datepart(year,d.LandingDate) as year
    ,case when substring(d.GearType,1,1) = 'G' then 'Gillnets'
    when substring(d.GearType,1,3) in ('TBB') then 'Beam trawls'
    when substring(d.GearType,1,3) in ('OTM') then 'Midwater trawls'
    when substring(d.GearType,1,1) in ('O','T') then 'Bottom otter trawls'
    when substring(d.GearType,1,1) in ('P','S') then 'Seines'
    else 'Others' end as gear
    ,d.EstKgWeightSUM as landWt
    ,FSS_Speciesname as stock
    from Declarations as d 
    join SpeciesLookup as s 
    on d.SpeciesID = s.SpeciesID 
    join Vessels as v 
    on d.VesselID = v.VesselID 
    left join MarinePortLookup p 
    on d.LandingPortID = p.MarinePortID
    left join IcesLookup i
    on d.Division = i.IcesDivision
         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME
    where v.VesselProvenance='Ireland' 
    and (FSS_Speciesname like '%ray%' or FSS_Speciesname like '%skate%')and coalesce(i.IcesDivisionCorrect,d.Division) in ('VIa','VIb','VIIa','VIIb','VIIc','VIId','VIIe','VIIf','VIIg','VIIh','VIIj','VIIk')
    
    union all select datepart(year,d.EndDate) as year
    ,'Others' as gear
    ,d.EstLiveWeightSUM as landWt
    ,FSS_Speciesname as stock
    from Under10MeterLandings as d 
    join SpeciesLookup as s 
    on d.SpeciesID = s.SpeciesID
    left join MarinePortLookup p 
    on	d.MarinePortID = p.MarinePortID
    left join IcesLookup i
    on d.Division = i.IcesDivision
         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME    
    where 1=1
    and (FSS_Speciesname like '%ray%' or FSS_Speciesname like '%skate%')and coalesce(i.IcesDivisionCorrect,d.Division) in ('VIa','VIb','VIIa','VIIb','VIIc','VIId','VIIe','VIIf','VIIg','VIIh','VIIj','VIIk')
    
    )x
    where year = 2017
    group by stock
    ,gear
    order by stock
    ,gear

"

channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_20180305")
land <- sqlQuery(channel,Q)
close(channel)

table(land$stock)
table(land$gear)

land$gear <- ordered(land$gear, c("Others", "Seines", "Midwater trawls", "Gillnets", "Bottom otter trawls", 
                                  "Beam trawls")
)

for(s in levels(land$stock)) {
    t <- with(subset(land,stock==s), tapply(landWt,gear,sum))
    t <- ifelse(is.na(t),0,t)
    cat(s,'\n')
    png(paste0(s,' in VI and VII.png'),4,1.75,'in',8,'white',600)
    par(mar=c(4,8.5,2,1))
    barplot(sort(t)/1000,xlim=extendrange(c(0,max(t)/1000),f=0.02),horiz=T,col=pal[7],border=pal[9],space=0.5,las=1,xlab='Tonnes',main = 'Irish landings by gear type')
    box()
    dev.off()
}

###################################################### Nep and Saithe in VII ###############################################
    Q <- "
        select stock
    ,gear
    ,sum(landWt) as landWt 
    from(
    select datepart(year,d.LandingDate) as year
    ,case when substring(d.GearType,1,1) = 'G' then 'Gillnets'
    when substring(d.GearType,1,3) in ('TBB') then 'Beam trawls'
    when substring(d.GearType,1,3) in ('OTM') then 'Midwater trawls'
    when substring(d.GearType,1,1) in ('O','T') then 'Bottom otter trawls'
    when substring(d.GearType,1,1) in ('P','S') then 'Seines'
    else 'Others' end as gear
    ,d.EstKgWeightSUM as landWt
    ,FSS_Speciesname as stock
    from Declarations as d 
    join SpeciesLookup as s 
    on d.SpeciesID = s.SpeciesID 
    join Vessels as v 
    on d.VesselID = v.VesselID 
    left join MarinePortLookup p 
    on d.LandingPortID = p.MarinePortID
    left join IcesLookup i
    on d.Division = i.IcesDivision
         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME
    where v.VesselProvenance='Ireland' 
    and (FSS_Speciesname like 'Saithe' or FSS_Speciesname like 'Nephrops')and coalesce(i.IcesDivisionCorrect,d.Division) in ('VIIa','VIIb','VIIc','VIId','VIIe','VIIf','VIIg','VIIh','VIIj','VIIk')
    
    union all select datepart(year,d.EndDate) as year
    ,'Others' as gear
    ,d.EstLiveWeightSUM as landWt
    ,FSS_Speciesname as stock
    from Under10MeterLandings as d 
    join SpeciesLookup as s 
    on d.SpeciesID = s.SpeciesID
    left join MarinePortLookup p 
    on	d.MarinePortID = p.MarinePortID
    left join IcesLookup i
    on d.Division = i.IcesDivision
         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME
    where 1=1
    and (FSS_Speciesname like 'Saithe' or FSS_Speciesname like 'Nephrops')and coalesce(i.IcesDivisionCorrect,d.Division) in ('VIIa','VIIb','VIIc','VIId','VIIe','VIIf','VIIg','VIIh','VIIj','VIIk')
    
    )x
    where year = 2017
    group by stock
    ,gear
    order by stock
    ,gear


"

channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_201803057")
land <- sqlQuery(channel,Q)
close(channel)

table(land$stock)
table(land$gear)

land$gear <- ordered(land$gear, c("Others", "Seines", "Midwater trawls", "Gillnets", "Bottom otter trawls", 
                                  "Beam trawls")
)

for(s in levels(land$stock)) {
    t <- with(subset(land,stock==s), tapply(landWt,gear,sum))
    t <- ifelse(is.na(t),0,t)
    cat(s,'\n')
    png(paste0(s,' in VII.png'),4,1.75,'in',8,'white',600)
    par(mar=c(4,8.5,2,1))
    barplot(sort(t)/1000,xlim=extendrange(c(0,max(t)/1000),f=0.02),horiz=T,col=pal[7],border=pal[9],space=0.5,las=1,xlab='Tonnes',main = 'Irish landings by gear type')
    box()
    dev.off()
}

###################################################### Nep in VI ###############################################
    Q <- "
        select stock
    ,gear
    ,sum(landWt) as landWt 
    from(
    select datepart(year,d.LandingDate) as year
    ,case when substring(d.GearType,1,1) = 'G' then 'Gillnets'
    when substring(d.GearType,1,3) in ('TBB') then 'Beam trawls'
    when substring(d.GearType,1,3) in ('OTM') then 'Midwater trawls'
    when substring(d.GearType,1,1) in ('O','T') then 'Bottom otter trawls'
    when substring(d.GearType,1,1) in ('P','S') then 'Seines'
    else 'Others' end as gear
    ,d.EstKgWeightSUM as landWt
    ,FSS_Speciesname as stock
    from Declarations as d 
    join SpeciesLookup as s 
    on d.SpeciesID = s.SpeciesID 
    join Vessels as v 
    on d.VesselID = v.VesselID 
    left join MarinePortLookup p 
    on d.LandingPortID = p.MarinePortID
    left join IcesLookup i
    on d.Division = i.IcesDivision
         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME
    where v.VesselProvenance='Ireland' 
    and (FSS_Speciesname like 'Nephrops')and coalesce(i.IcesDivisionCorrect,d.Division) in ('VIa','VIb','VI')
    
    union all select datepart(year,d.EndDate) as year
    ,'Others' as gear
    ,d.EstLiveWeightSUM as landWt
    ,FSS_Speciesname as stock
    from Under10MeterLandings as d 
    join SpeciesLookup as s 
    on d.SpeciesID = s.SpeciesID
    left join MarinePortLookup p 
    on	d.MarinePortID = p.MarinePortID
    left join IcesLookup i
    on d.Division = i.IcesDivision
         left join Stockman2015_views..IntercatchSpeciesLut sl
            on case when s.scientificname like 'lophius%' then 'Lophius Piscatorius'
               when s.scientificname like 'lepidorhombus%' then 'Lepidorhombus whiffiagonis'
               else s.ScientificName end = sl.FISHLATINNAME
    where 1=1
    and (FSS_Speciesname like 'Nephrops') and coalesce(i.IcesDivisionCorrect,d.Division) in ('VIa','VIb','VI')
    
    )x
    where year = 2017
    group by stock
    ,gear
    order by stock
    ,gear


"

channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_20180305")
land <- sqlQuery(channel,Q)
close(channel)

table(land$stock)
table(land$gear)

land$gear <- ordered(land$gear, c("Others", "Seines", "Midwater trawls", "Gillnets", "Bottom otter trawls", 
                                  "Beam trawls")
)

for(s in levels(land$stock)) {
    t <- with(subset(land,stock==s), tapply(landWt,gear,sum))
    t <- ifelse(is.na(t),0,t)
    cat(s,'\n')
    png(paste0(s,' in VI.png'),4,1.75,'in',8,'white',600)
    par(mar=c(4,8.5,2,1))
    barplot(sort(t)/1000,xlim=extendrange(c(0,max(t)/1000),f=0.02),horiz=T,col=pal[7],border=pal[9],space=0.5,las=1,xlab='Tonnes',main = 'Irish landings by gear type')
    box()
    dev.off()
}
