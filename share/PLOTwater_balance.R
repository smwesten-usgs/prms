
library(Cairo)

# Data values starts on line 3
inp =  read.table("prms_summary.csv", skip=2, sep=",")
date<-inp[[1]]
shortDate<-date[-1]

# The column names are on line 1
hl = readLines("prms_summary.csv", 1)
hl = strsplit(hl, ',')
colnames(inp) = hl[[1]]

Cairo(960,1280,"water_balance.png",bg="white",pointsize=10,horizontal=FALSE)

# total water balance
oldpar <- par(oma=c(0,0,2,0), mar=c(3,3,3,1), mfrow=c(1,2))

#colNum<-match(c("basin_total_storage""),colnames(inp))
storage<-inp[[match(c("basin_total_storage"),colnames(inp))]]

basin_gwstor_minarea_wb<-inp[[match(c("basin_gwstor_minarea_wb"),colnames(inp))]]
precip<-inp[[match(c("basin_ppt"),colnames(inp))]]
inflow<-basin_gwstor_minarea_wb+precip
inflow<-inflow[-1]

basin_actet<-inp[[match(c("basin_actet"),colnames(inp))]]
basin_gwsink<-inp[[match(c("basin_gwsink"),colnames(inp))]]
basin_stflow_in<-inp[[match(c("basin_stflow_in"),colnames(inp))]]
outflow<-basin_actet+basin_gwsink+basin_stflow_in
outflow<-outflow[-1]

error<-diff(storage,lag=1)-inflow+outflow

dx<-data.frame(date=as.Date(shortDate), value=cumsum(error))
par(fig=c(0.04,0.96,0.76,0.96),new=TRUE,mar=c(0,4,2,0))
plot(dx,type="l",col="red",main="Model Domain",xlab=" ",ylab=" ",yaxt="n")
axis(2,las=2,tck=0.02,cex.axis=0.8)
box()
mtext("Cumulative Error in Water Balance, inches", outer=TRUE, cex=1.5)
par(oldpar)


# capilary

basin_capillary_wb<-inp[[match(c("basin_capillary_wb"),colnames(inp))]]
dx<-data.frame(date=as.Date(date), value=cumsum(basin_capillary_wb))
par(fig=c(0.04,0.5,0.58,0.74),new=TRUE,mar=c(0,4,2,0))
plot(dx,type="l",col="red",main="Capilary Reservoir",xlab=" ",ylab=" ", tck=0,col.axis="white",axes=FALSE,xpd=FALSE)
axis(2,las=2,tck=0.02,cex.axis=0.8)
box()

# Gravity Reservoir

basin_gravity_wb<-inp[[match(c("basin_gravity_wb"),colnames(inp))]]
dx<-data.frame(date=as.Date(date), value=cumsum(basin_gravity_wb))
par(fig=c(0.04,0.5,0.4,0.58),new=TRUE,mar=c(0,4,2,0))
plot(dx,type="l",col="red",main="Gravity Reservoir Water Balance, inches",xlab=" ",ylab=" ", tck=0,col.axis="white",axes=FALSE,xpd=FALSE)
axis(2,las=2,tck=0.02,cex.axis=0.8)
box()

# Groundwater Reservoir 

basin_gwstor<-inp[[match(c("basin_gwstor"),colnames(inp))]]
basin_gwin<-inp[[match(c("basin_gwin"),colnames(inp))]]
basin_gwstor_minarea_wb<-inp[[match(c("basin_gwstor_minarea_wb"),colnames(inp))]]
basin_dnflow<-inp[[match(c("basin_dnflow"),colnames(inp))]]

infoo<-basin_gwin+basin_gwstor_minarea_wb+basin_dnflow
infoo<-infoo[-1]

basin_gwflow<-inp[[match(c("basin_gwflow"),colnames(inp))]]
basin_gwsink<-inp[[match(c("basin_gwsink"),colnames(inp))]]
out<-basin_gwsink+basin_gwflow
out<-out[-1]

error<-diff(basin_gwstor,lag=1)-infoo+out
dx<-data.frame(date=as.Date(shortDate), value=cumsum(error))
par(fig=c(0.04,0.5,0.22,0.4),new=TRUE,mar=c(0,4,2,0))
plot(dx,type="l",col="red",main="Groundwater Reservoir",xlab=" ",ylab=" ", tck=0,col.axis="white",axes=FALSE,xpd=FALSE)
axis(2,las=2,tck=0.02,cex.axis=0.8)
box()

# Soilzone 
basin_soilzone_wb<-inp[[match(c("basin_soilzone_wb"),colnames(inp))]]
dx<-data.frame(date=as.Date(date), value=cumsum(basin_soilzone_wb))
par(fig=c(0.04,0.5,0.04,0.22),new=TRUE,mar=c(0,4,2,0))
plot(dx,type="l",col="red",main="Soilzone",xlab=" ",ylab=" ",yaxt="n")
axis(2,las=2,tck=0.02,cex.axis=0.8)
box()


#######################################

# snowpack  NEED basin_snow precip in inches

basin_pweqv<-inp[[match(c("basin_pweqv"),colnames(inp))]]
basin_pk_precip<-inp[[match(c("basin_pk_precip"),colnames(inp))]]
basin_snowmelt<-inp[[match(c("basin_snowmelt"),colnames(inp))]]
basin_snowevap<-inp[[match(c("basin_snowevap"),colnames(inp))]]

outfoo<-basin_snowmelt+basin_snowevap
outfoo<-outfoo[-1]

inflow<-basin_pk_precip
inflow<-inflow[-1]

error<-diff(basin_pweqv,lag=1)-inflow+outfoo
dx<-data.frame(date=as.Date(shortDate), value=cumsum(error))
par(fig=c(0.50,0.96,0.58,0.74),new=TRUE,mar=c(0,4,2,0))
plot(dx,type="l",col="red",main="Snowpack",xlab=" ",ylab=" ", tck=0,col.axis="white",axes=FALSE,xpd=FALSE)
axis(2,las=2,tck=0.02,cex.axis=0.8)
box()

# Depression Storage

basin_dprst_wb<-inp[[match(c("basin_dprst_wb"),colnames(inp))]]
dx<-data.frame(date=as.Date(date), value=cumsum(basin_dprst_wb))
par(fig=c(0.50,0.96,0.4,0.58),new=TRUE,mar=c(0,4,2,0))
plot(dx,type="l",col="red",main="Depression Storage",xlab=" ",ylab=" ", tck=0,col.axis="white",axes=FALSE,xpd=FALSE)
axis(2,las=2,tck=0.02,cex.axis=0.8)
box()

# Lake
basin_lake_stor<-inp[[match(c("basin_lake_stor"),colnames(inp))]]
basin_lakeevap<-inp[[match(c("basin_lakeevap"),colnames(inp))]]
basin_lakeevap<-basin_lakeevap[-1]

error<-diff(basin_lake_stor,lag=1)-basin_lakeevap
dx<-data.frame(date=as.Date(shortDate), value=cumsum(error))
par(fig=c(0.50,0.96,0.22,0.4),new=TRUE,mar=c(0,4,2,0))
plot(dx,type="l",col="red",main="Lake Water Balance, inches",xlab=" ",ylab=" ", tck=0,col.axis="white",axes=FALSE,xpd=FALSE)
axis(2,las=2,tck=0.02,cex.axis=0.8)
box()

# Stream routing
basin_stflow_in<-inp[[match(c("basin_stflow_in"),colnames(inp))]]
basin_stflow_out<-inp[[match(c("basin_stflow_out"),colnames(inp))]]
delta<-basin_stflow_in-basin_stflow_out

dx<-data.frame(date=as.Date(date), value=cumsum(delta))
par(fig=c(0.50,0.96,0.04,0.22),new=TRUE,mar=c(0,4,2,0))
plot(dx,type="l",col="red",main="Stream Routing Sum of Difference between in and out, inches",xlab=" ",ylab=" ",yaxt="n")
axis(2,las=2,tck=0.02,cex.axis=0.8)
box()

foo<-dev.off()

