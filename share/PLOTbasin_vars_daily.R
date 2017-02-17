
library(Cairo)

# Data values starts on line 3
inp =  read.table("prms_summary.csv", skip=2, sep=",")

# The column names are on line 1
hl = readLines("prms_summary.csv", 1)
hl = strsplit(hl, ',')
colnames(inp) = hl[[1]]

# The units are on line 2
hl = readLines("prms_summary.csv", 2)
hl = strsplit(hl, ',')
units<-hl[[2]]

# assign the values in the column with the name "Date" into date variable
date<-inp[[match(c("Date"),colnames(inp))]]
f_month<-30.0/length(date)
f_year<-365.0/length(date)

# function for drawing single variable plots with no dates on x-axis
singleValuePlot<-function(varName, extent, desc, color="black") {
   par(fig=extent,new=TRUE,mar=c(0,4,2,0))
   colNum<-match(c(varName),colnames(inp))
   val<-inp[[colNum]]
   dx<-data.frame(date=as.Date(date), value=val)
   plot(dx,type="l",col=color,main=paste(varName, ": ", desc, ", ", units[colNum]),xlab=" ",ylab=" ", tck=0,col.axis="white",axes=FALSE,xpd=FALSE)
   lines(lowess(dx, f=f_month),col="red")
   lines(lowess(dx, f=f_year),col="green")
   lines(lowess(dx, f=1.0),col="blue")
   axis(2,las=2,tck=0.02,cex.axis=0.8)
   box()
}

# function for drawing single variable plots with dates on x-axis
singleValuePlotWithDates<-function(varName, extent, desc) {
   par(fig=extent,new=TRUE,mar=c(0,4,2,0))
   colNum<-match(c(varName),colnames(inp))
   val<-inp[[colNum]]
   dx<-data.frame(date=as.Date(date), value=val)
   plot(dx,type="l",col="black",main=paste(varName, ": ", desc, ", ", units[colNum]),xlab=" ",ylab=" ",yaxt="n")
   lines(lowess(dx, f=.1),col="red")
   lines(lowess(dx, f=.5),col="green")
   lines(lowess(dx, f=1.0),col="blue")
   axis(2,las=2,tck=0.02,cex.axis=0.8)
   box()
}

# Atmoshpere Fluxes
Cairo(960,1280,"basin_atmflux.png",type="png",bg="white",pointsize=10,horizontal=FALSE)
singleValuePlot("basin_actet", c(0.04,0.5,0.81,0.96), "Actual ET")
singleValuePlot("basin_dprst_evap", c(0.04,0.5,0.66,0.81), "ET from surface depression")
singleValuePlot("basin_imperv_evap", c(0.04,0.5,0.51,0.66), "ET from impervious area")
singleValuePlot("basin_intcp_evap", c(0.04,0.5,0.36,0.51), "ET from canopy")
singleValuePlot("basin_lakeevap", c(0.04,0.5,0.21,0.36), "Lake ET")
singleValuePlotWithDates("basin_perv_et", c(0.04,0.5,0.06,0.21), "ET from capillary reservoirs")
singleValuePlot("basin_potet", c(0.50,0.96,0.81,0.96), "PET")
singleValuePlot("basin_potsw", c(0.50,0.96,0.66,0.81), "Potential shortwave radiaiton")
singleValuePlot("basin_ppt", c(0.50,0.96,0.51,0.66), "Precipitation")
singleValuePlot("basin_snowevap", c(0.50,0.96,0.36,0.51), "ET and sublimation")
singleValuePlot("basin_tmax", c(0.50,0.96,0.21,0.36), "Maximum temperature")
singleValuePlotWithDates("basin_tmin", c(0.50,0.96,0.06,0.21), "Minimum temperature")
foo<-dev.off()

# Reservoir Fluxes
Cairo(960,1280,"basin_resflux.png",type="png",bg="white",pointsize=10,horizontal=FALSE)
singleValuePlot("basin_capwaterin", c(0.04,0.5,0.81,0.96), "Flow added to capillary reservoir storage")
singleValuePlot("basin_dprst_seep", c(0.04,0.5,0.66,0.81), "Seepage from surface depressions")
singleValuePlot("basin_gwin", c(0.04,0.5,0.51,0.66), "Inflow to ground water reservoirs")
singleValuePlot("basin_gwsink", c(0.04,0.5,0.36,0.51), "Groundwater sink")
singleValuePlot("basin_gwstor_minarea_wb", c(0.04,0.5,0.21,0.36), "Groundwater Minimum")
singleValuePlotWithDates("basin_pref_flow_in", c(0.04,0.5,0.06,0.21), "Infiltration to preferential-flow res storage")
singleValuePlot("basin_prefflow", c(0.50,0.96,0.81,0.96), "Interflow from preferential-flow res to stream")
singleValuePlot("basin_recharge", c(0.50,0.96,0.66,0.81), "Recharge to groundwater reservoirs")
singleValuePlot("basin_slowflow", c(0.50,0.96,0.51,0.66), "Interflow from rervoirs to the stream")
singleValuePlot("basin_snowmelt", c(0.50,0.96,0.36,0.51), "Snowmelt")
singleValuePlot("basin_soil_to_gw", c(0.50,0.96,0.21,0.36), "Excess flow to cap res that drains to gw res")
singleValuePlotWithDates("basin_sz2gw", c(0.50,0.96,0.06,0.21), "Drainage from gravity res to gw res")
foo<-dev.off()

# Storage
Cairo(960,1280,"basin_storage.png",type="png",bg="white",pointsize=10,horizontal=FALSE)
singleValuePlot("basin_dprst_volop", c(0.04,0.5,0.81,0.96), "storage volume in open surface depressions")
singleValuePlot("basin_gwstor", c(0.04,0.5,0.66,0.81), "storage in groundwater reservoirs")
singleValuePlot("basin_imperv_stor", c(0.04,0.5,0.51,0.66), "storage on impervious areas")
singleValuePlot("basin_intcp_stor", c(0.04,0.5,0.36,0.51), "interception storage")
singleValuePlot("basin_pref_stor", c(0.04,0.5,0.21,0.36), "storage in preferential-flow reservoirs")
singleValuePlotWithDates("basin_pweqv", c(0.04,0.5,0.06,0.21), "snowpack water equivalent")
singleValuePlot("basin_slstor", c(0.50,0.96,0.81,0.96), "storage of gravity reservoirs")
singleValuePlot("basin_snowcov", c(0.50,0.96,0.66,0.81), "snowcover")
singleValuePlot("basin_soil_moist", c(0.50,0.96,0.51,0.66), "capillary reservoir storage")
singleValuePlot("basin_soil_rechr", c(0.50,0.96,0.36,0.51), "storage for recharge zone")
singleValuePlot("basin_ssstor", c(0.50,0.96,0.21,0.36), "gravity and preferential-flow reservoir storage")
singleValuePlotWithDates("basin_total_storage", c(0.50,0.96,0.06,0.21), "storage in all water storage reservoirs")
foo<-dev.off()

# Streamflow
Cairo(960,1280,"basin_flow.png",type="png",bg="white",pointsize=10,horizontal=FALSE)
singleValuePlot("basin_cfs", c(0.04,0.96,0.75,1), "Total Stream Flow", color="black")
#legend("topleft",col="red","Surface",lty=1,lwd=1)

# Components of flow
singleValuePlot("basin_sroff_cfs", c(0.04,0.96,0.5,0.75), "Components of Flow", color="black")

val<-inp[[match(c("basin_gwflow_cfs"),colnames(inp))]]
dx<-data.frame(date=as.Date(date), value=val)
lines(dx,type="l",col="cyan")

colNum<-match(c("basin_ssflow_cfs"),colnames(inp))
date<-inp[[1]]
val<-inp[[colNum]]
dx<-data.frame(date=as.Date(date), value=val)
lines(dx,type="l",col="purple")
legend("topleft",col=c("black","cyan","purple"), c("basin_sroff_cfs","basin_gwflow_cfs","basin_ssflow_cfs"),lty=1,lwd=1)

foo<-dev.off()


