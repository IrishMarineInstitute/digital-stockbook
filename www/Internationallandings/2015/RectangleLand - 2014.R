library(mapplots)
library(shapefiles)
library(RColorBrewer)
library(splancs)
library(rgdal)
coast <- read.shapefile('//galwayfs03/FishData/Mapping/Shapefiles/Europe')

#eez <- read.csv('F:\\Mapping\\Shapefiles\\Maritime Boundaries\\eez.csv')
eezWorld <- readOGR("//Galwayfs03/FishData/StockBooks/_StockBook2017/Maps/VMS/PropSpecies","eez_boundaries") # marineregions v9
eezWorld1 <- subset(eezWorld,Line_type%in%c('Treaty','200 NM','Connection line','Median line','Unilateral claim (undisputed)'))


legend.grid <- 
function (x, y = NULL, breaks, col, digits = 2, suffix = "", 
    type = 1, pch = 15, pt.cex = 2.5, bg = "lightblue", ...) 
{
    ncol <- length(breaks) - 1
    if (missing(col)) 
        col = c("white", heat.colors(ncol - 1)[(ncol - 1):1])
    tempfun <- function(x) format(x, digits = digits)
    min <- sapply(breaks[(ncol):1], tempfun)
    mid <- sapply((breaks[(ncol):1] + breaks[(ncol + 1):2])/2, 
        tempfun)
    max <- sapply(breaks[(ncol + 1):2], tempfun)
    if (type == 1) 
        legend <- paste(mid, suffix, sep = "")
    if (type == 2) 
        legend <- paste(min, " - ", max, suffix, sep = "")
    legend(x, y, legend = legend, col = col[ncol:1], pch = pch, 
        pt.cex = pt.cex, bg = bg, ...)
}


setwd('H:/Stockbook/shiny/WIP/www/Internationallandings/2015')

sp <- sort(c('MAC','JAX','HER','WHB','NEP','WHG','HAD','ANF','CRE','LEZ','SPR','RAJ'
  ,'HKE','COD','SCE','LIN','ALB','BFT','POL','POK','PLE','SOL','WIT','JOD','BOC','LEM','SWO','PIL'))


# https://stecf.jrc.ec.europa.eu/data-reports, see email steve holmes in folder: F:\StockBooks\_Stockbook2016\Maps\STECF

stecf <- read.csv('stecf_2014.csv')

stecf$Species <- ifelse(stecf$species %in% c('JAD','RJG','RAJ','RJY','SRX'),'RAJ',as.character(stecf$species))
stecf$Species <- ifelse(stecf$Species %in% c('BOR','BOF','BOC'),'BOC',as.character(stecf$Species))
stecf$Species <- factor(stecf$Species)

stecf$area <- 1.0*cos(stecf$lat*pi/180)*60*1.852 * 0.5*60*1.852 


xlim <- c(-30.5,15)
ylim <- c(30.25,70)
grd <- with(stecf, make.multigrid(lon,lat,1000*landings/area,Species,1,0.5,xlim,ylim) )
col <- colorRampPalette(c("lightyellow","yellow","orange","red", "brown4"))(8)

xlim <- c(-16.5,5)
ylim <- c(45.25,63)

#grd[['RAJ']] <- NULL

for(s in sp){
#  S <- SP[match(s,sp)]
  png(paste0('Rect',s,'.png'),2.75,3.25,'in',6,res=600)
#  pdf(paste0('./PlotsLand/Rect',s,'.pdf'),2.75,3.25,pointsize=6)  
    par(mar=c(1,1,.1,.1),lwd=0.5)  
    basemap(xlim=xlim,ylim=ylim,xaxt='n',yaxt='n',ann=F,bg='lightcyan')
    if(s%in%c('ALB','BFT')) basemap(xlim=xlim-5,ylim=ylim-5,xaxt='n',yaxt='n',ann=F,bg='lightcyan')
    axis(1,-90:90,labels=c(paste0(90:1,'°W'),0,paste0(1:90,'°E')),lwd=0.5,tcl=-0.15,padj=-2,cex.axis=0.8)
    axis(2,0:90,labels=paste0(0:90,'°N'),lwd=0.5,tcl=-0.15,padj=1.7,cex.axis=0.8)
    if(!is.null(grd[[s]])){
      breaks <- breaks.grid(grd[[s]],ncol=8,zero=F)
      draw.grid(grd[[s]],breaks,col=col)
    }

   plot(eezWorld1,lwd=0.25,add=T,col='darkblue')
   
    draw.shape(coast,col='darkolivegreen1',lwd=0.25) #cornsilk1
  
    title <- expression(paste('kg/',km^2))
    if(!is.null(grd[[s]])) legend.grid('bottomright',NULL,breaks,col,2,'',1,bg='lightcyan',inset=0.02,title=title) else
      text(-10,60,'No data',cex=2)
    box()
  dev.off()

}
