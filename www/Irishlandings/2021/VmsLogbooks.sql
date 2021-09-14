drop table #ops
drop table #vms
drop table #pings
drop table #my_vms

select convert(date,a.OperationDate) as OperationDate
   ,c.CFR
   ,sum(case when FAQCODE in ('ALB','Tun') then LiveWeightSUM else 0 end) as DayLiveWtAlb
   ,sum(case when FSS_SpeciesName = 'Monkfish' then LiveWeightSUM else 0 end) as DayLiveWtAnf
   ,sum(case when FSS_SpeciesName = 'Boarfish' then LiveWeightSUM else 0 end) as DayLiveWtBoc
   ,sum(case when FSS_SpeciesName = 'Sole Black' then LiveWeightSUM else 0 end) as DayLiveWtSol
   ,sum(case when FSS_SpeciesName = 'Blue Whiting' then LiveWeightSUM else 0 end) as DayLiveWtWhb
   ,sum(case when FSS_SpeciesName = 'cod' then LiveWeightSUM else 0 end) as DayLiveWtCod
   ,sum(case when FSS_SpeciesName = 'haddock' then LiveWeightSUM else 0 end) as DayLiveWtHad
   ,sum(case when FSS_SpeciesName = 'hake' then LiveWeightSUM else 0 end) as DayLiveWtHke
   ,sum(case when FSS_SpeciesName = 'herring' then LiveWeightSUM else 0 end) as DayLiveWtHer
   ,sum(case when FSS_SpeciesName = 'Horse Mackerel' then LiveWeightSUM else 0 end) as DayLiveWtJax
   ,sum(case when FSS_SpeciesName = 'John dory' then LiveWeightSUM else 0 end) as DayLiveWtJod
   ,sum(case when FSS_SpeciesName = 'lemon sole' then LiveWeightSUM else 0 end) as DayLiveWtLem
   ,sum(case when FSS_SpeciesName = 'ling' then LiveWeightSUM else 0 end) as DayLiveWtLin
   ,sum(case when FSS_SpeciesName = 'Mackerel' then LiveWeightSUM else 0 end) as DayLiveWtMac
   ,sum(case when FSS_SpeciesName = 'Megrim' then LiveWeightSUM else 0 end) as DayLiveWtLez
   ,sum(case when FSS_SpeciesName = 'nephrops' then LiveWeightSUM else 0 end) as DayLiveWtNep
   ,sum(case when FSS_SpeciesName = 'plaice' then LiveWeightSUM else 0 end) as DayLiveWtPle
   ,sum(case when FSS_SpeciesName = 'Pollack' then LiveWeightSUM else 0 end) as DayLiveWtPol
   ,sum(case when FSS_SpeciesName = 'Saithe' then LiveWeightSUM else 0 end) as DayLiveWtPok
   ,sum(case when FAQDesc like '%ray%' or FAQDesc like '%skate%' then LiveWeightSUM else 0 end) as DayLiveWtRaj
   ,sum(case when FSS_SpeciesName = 'Scallop' then LiveWeightSUM else 0 end) as DayLiveWtSca
   ,sum(case when FSS_SpeciesName = 'squid' then LiveWeightSUM else 0 end) as DayLiveWtSqu
   ,sum(case when FSS_SpeciesName = 'sprat' then LiveWeightSUM else 0 end) as DayLiveWtSpr
   ,sum(case when FSS_SpeciesName = 'whiting' then LiveWeightSUM else 0 end) as DayLiveWtWhg
   ,sum(case when FSS_SpeciesName = 'witch' then LiveWeightSUM else 0 end) as DayLiveWtWit
   ,sum(case when FAQCODE = 'BFT' then LiveWeightSUM else 0 end) as DayLiveWtBft
   ,sum(case when FSS_SpeciesName = 'European Pilchard' then LiveWeightSUM else 0 end) as DayLiveWtPil
   ,sum(case when FSS_SpeciesName = 'Swordfish' then LiveWeightSUM else 0 end) as DayLiveWtSwo
   ,sum(LiveWeightSUM) as DayLiveWtTot
into #ops
from Operational a
   join OperationalEffort b
      on a.FishingOpId = b.FishingOpId
   join Vessels c
      on b.VesselID = c.VesselID
   join SpeciesLookup d
      on a.SpeciesID = d.SpeciesID
where a.OperationDate between '1 jan 2015' and '1 jan 2020'
   and c.vesselprovenance = 'Ireland'
group by convert(date,a.OperationDate)
   ,c.CFR

select b.cfr
   ,b.date_logged
   ,coalesce(LogbookMainGear,EuMainGear) as best_gear
   ,a.posn_long as lon
   ,a.posn_lat as lat
   ,case when b.dt > 4 then 4 else b.dT end as dT
into #vms
from FEAS_VMS..VMS_Position_Report a
   join FEAS_VMS..VMS_Calculated_Fields b
      on a.prt_id = b.prt_id
   join FEAS_VMS..VMS_Gear c
      on a.prt_id = c.prt_id
where a.date_time_logged between '1 jan 2015' and '1 jan 2020'
   and a.posn_long between -20 and 7
   and a.posn_lat between 36 and 65
   and b.estimated_speed between 
         case when substring(coalesce(LogbookMainGear,EuMainGear),1,3) in ('FPO','GEN','GN','GNC','GND','GNF','GNS','GTN','GTR','LHM','LHP','LLD','LLS','LTL','LX') then 0.1 
         else 0.5 end  
      and case when substring(coalesce(LogbookMainGear,EuMainGear),1,3) in ('DRB','DRH','HMD','OTB','OTT','PTB') then 5.5
         when substring(coalesce(LogbookMainGear,EuMainGear),1,3) in ('OTM','PTM','TM','LNB','LLS','TBB') then 6
         else 4.5
         end
   and b.dt > 0
   and c.LogbookMainGear is not null

select cfr
   ,date_logged
   ,best_gear
   ,count(*) as fishing_pings
into #pings
from #vms
group by cfr
   ,date_logged
   ,best_gear

select datepart(yy, a.date_logged) as [year]
   ,a.lon
   ,a.lat
   ,a.best_gear
   ,a.dT
   ,c.DayLiveWtAlb / b.fishing_pings as LiveWtAlb
   ,c.DayLiveWtAnf / b.fishing_pings as LiveWtAnf
   ,c.DayLiveWtSol / b.fishing_pings as LiveWtSol
   ,c.DayLiveWtWhb / b.fishing_pings as LiveWtWhb
   ,c.DayLiveWtBoc / b.fishing_pings as LiveWtBoc
   ,c.DayLiveWtCod / b.fishing_pings as LiveWtCod
   ,c.DayLiveWtHad / b.fishing_pings as LiveWtHad
   ,c.DayLiveWtHke / b.fishing_pings as LiveWtHke
   ,c.DayLiveWtHer / b.fishing_pings as LiveWtHer
   ,c.DayLiveWtJax / b.fishing_pings as LiveWtJax
   ,c.DayLiveWtJod / b.fishing_pings as LiveWtJod
   ,c.DayLiveWtLem / b.fishing_pings as LiveWtLem
   ,c.DayLiveWtLin / b.fishing_pings as LiveWtLin
   ,c.DayLiveWtMac / b.fishing_pings as LiveWtMac
   ,c.DayLiveWtLez / b.fishing_pings as LiveWtLez
   ,c.DayLiveWtNep / b.fishing_pings as LiveWtNep
   ,c.DayLiveWtPle / b.fishing_pings as LiveWtPle
   ,c.DayLiveWtPol / b.fishing_pings as LiveWtPol
   ,c.DayLiveWtPok / b.fishing_pings as LiveWtPok
   ,c.DayLiveWtRaj / b.fishing_pings as LiveWtRaj
   ,c.DayLiveWtSca / b.fishing_pings as LiveWtSca
   ,c.DayLiveWtSqu / b.fishing_pings as LiveWtSqu
   ,c.DayLiveWtSpr / b.fishing_pings as LiveWtSpr
   ,c.DayLiveWtWhg / b.fishing_pings as LiveWtWhg
   ,c.DayLiveWtWit / b.fishing_pings as LiveWtWit
   ,c.DayLiveWtBft / b.fishing_pings as LiveWtBft
   ,c.DayLiveWtPil / b.fishing_pings as LiveWtPil
   ,c.DayLiveWtSwo / b.fishing_pings as LiveWtSwo
   ,c.DayLiveWtTot / b.fishing_pings as LiveWtTot
into #my_vms
from #vms a
   join #pings b
      on a.cfr = b.cfr
      and a.date_logged = b.date_logged
      and a.best_gear = b.best_gear
   join #ops c
      on a.cfr = c.cfr
      and a.date_logged = c.operationdate
;

---
