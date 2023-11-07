-- first run VmsLogbooks.sql
select [Year]
   ,round(lon / 0.03, 0) * 0.03 as Lon
   ,round(lat / 0.02, 0) * 0.02 as Lat
--   ,case when best_gear in ('OTB','OTT','PTB') then 'BottomOtterTrawls'
--      when best_gear in ('DRB','DRH','HMD') then 'Dredges'
--      when best_gear in ('TBB') then 'BeamTrawls'
--      when best_gear in ('PS','PS2','SDN','SPR','SSC','LA','SB') then 'Seines'
--      when best_gear in ('OTM','PTM','TM') then 'PelagicTrawls'
--      when best_gear in ('FPO') then 'Pots'
--      when best_gear in ('GEN','GN','GNC','GND','GNF','GNS','GTN','GTR') then 'GillNets'
--      when best_gear in ('LHM','LHP','LLD','LLS','LTL','LX','LL') then 'LongLines'
--      end as Gear
   ,sum(dT) as EffortHours
   ,sum(LiveWtAlb) as LiveWtAlb
   ,sum(LiveWtAnf) as LiveWtAnf
   ,sum(LiveWtSol) as LiveWtSol
   ,sum(LiveWtWhb) as LiveWtWhb
   ,sum(LiveWtBoc) as LiveWtBoc
   ,sum(LiveWtCod) as LiveWtCod
   ,sum(LiveWtHad) as LiveWtHad
   ,sum(LiveWtHke) as LiveWtHke
   ,sum(LiveWtHer) as LiveWtHer
   ,sum(LiveWtJax) as LiveWtJax
   ,sum(LiveWtJod) as LiveWtJod
   ,sum(LiveWtLem) as LiveWtLem
   ,sum(LiveWtLin) as LiveWtLin
   ,sum(LiveWtMac) as LiveWtMac
   ,sum(LiveWtLez) as LiveWtLez
   ,sum(LiveWtNep) as LiveWtNep
   ,sum(LiveWtPle) as LiveWtPle
   ,sum(LiveWtPol) as LiveWtPol
   ,sum(LiveWtPok) as LiveWtPok
   ,sum(LiveWtRaj) as LiveWtRaj
   ,sum(LiveWtSca) as LiveWtSca
   ,sum(LiveWtSol) as LiveWtSol
   ,sum(LiveWtSqu) as LiveWtSqu
   ,sum(LiveWtSpr) as LiveWtSpr
   ,sum(LiveWtWhg) as LiveWtWhg
   ,sum(LiveWtWit) as LiveWtWit

   ,sum(LiveWtBft) as LiveWtBft
   ,sum(LiveWtPil) as LiveWtPil
   ,sum(LiveWtSwo) as LiveWtSwo
   ,sum(LiveWtTot) as LiveWtTot
from #my_vms
group by [Year]
   ,round(lon / 0.03, 0) * 0.03
   ,round(lat / 0.02, 0) * 0.02
--   ,case when best_gear in ('OTB','OTT','PTB') then 'BottomOtterTrawls'
--      when best_gear in ('DRB','DRH','HMD') then 'Dredges'
--      when best_gear in ('TBB') then 'BeamTrawls'
--      when best_gear in ('PS','PS2','SDN','SPR','SSC','LA','SB') then 'Seines'
--      when best_gear in ('OTM','PTM','TM') then 'PelagicTrawls'
--      when best_gear in ('FPO') then 'Pots'
--      when best_gear in ('GEN','GN','GNC','GND','GNF','GNS','GTN','GTR') then 'GillNets'
--      when best_gear in ('LHM','LHP','LLD','LLS','LTL','LX','LL') then 'LongLines'
--      end