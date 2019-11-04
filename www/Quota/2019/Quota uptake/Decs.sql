select year
   ,week
   ,species
   ,area
   ,sum(landWt) as landWt 
from(
   select datepart(year,d.LandingDate) as year
	   ,datepart(week,d.LandingDate) as week
	   ,i.icesdivisionIC as area
      ,s.FSS_SpeciesName as species
	   ,d.EstKgWeightSUM as landWt
   from Declarations as d 
      join SpeciesLookup as s 
         on d.SpeciesID = s.SpeciesID 
      join stecf.CombinedVesselTables v
         on d.VesselID = v.VesselID 
         and datepart(year,d.LandingDate) = v.ActivityYear
      left join IcesLookup i
         on d.Division = i.IcesDivision
   where v.VesselProvenance='Ireland' 
      and d.PresentationType <> 'Below minimum conservation reference size'


   union all
   select datepart(year,d.EndDate) as year
	   ,datepart(week,d.EndDate) as week
      ,i.IcesDivisionIC as area
      ,s.FSS_SpeciesName as species
	   ,d.EstLiveWeightSUM as landWt
   from Under10MeterLandings as d 
      join SpeciesLookup as s 
         on d.SpeciesID = s.SpeciesID
      left join IcesLookup i
         on d.Division = i.IcesDivision
      where datepart(year,d.EndDate) < 2015

   union all
   select datepart(year,d.LANDING_DATE) as year
	   ,datepart(week,d.LANDING_DATE) as week
      ,i.IcesDivisionIC as area
      ,s.FSS_SpeciesName as species
	   ,d.EstKgLiveWt as landWt
   from SalesNotesU10m as d 
      join SpeciesLookup as s 
         on d.SpeciesID = s.SpeciesID
      left join MarinePortLookup p 
         on	d.MarinePortID = p.MarinePortID
      join stecf.CombinedVesselTables as v
         on d.IFIS_VESSEL_ID = v.VesselID
         and datepart(year,d.LANDING_DATE) = v.ActivityYear
      left join IcesLookup i
         on p.IcesDivision = i.IcesDivision
      where d.PRESENTATION_DESC <> 'Below minimum conservation reference size' -- does not occurl just to futureproof
         and v.vesselprovenance = 'Ireland'
         and datepart(year,d.LANDING_DATE) >=2015
	  )x
where year > 2003
group by year
   ,week
   ,species
   ,area
