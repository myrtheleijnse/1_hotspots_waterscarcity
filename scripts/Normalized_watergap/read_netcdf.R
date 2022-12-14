library(ncdf4)
library(ggplot2)
library(raster)
library(rgdal)
library(plotly)
library(rasterly)

#source https://rpubs.com/boyerag/297592

setwd("C:/Users/5738091/Documents/WorldWaterMap")

#read and inspect nc data
nc_data <- nc_open("irrigationWaterWithdrawal_annuaTot_output.nc")

{
  sink('irrigationWaterWithdrawal_annuaTot_output.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

head(t)

irr_withd.array <- ncvar_get(nc_data, "irrigation_withdrawal") # store the data in a 3-dimensional array
dim(irr_withd.array)
fillvalue <- ncatt_get(nc_data, "irrigation_withdrawal", "_FillValue")
fillvalue
nc_close(nc_data)

#plot 1st timestep spatial
irr_withd.slice <- irr_withd.array[,,1]

r <- raster(t(irr_withd.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction="y")
plot(r)
abline(v=120)
abline(h=33)

# ggplot raster
r_df <- as.data.frame(r, xy=T)
colnames(r_df) <- c("lon", "lat", "withdrawal")

ggplot()+
  geom_raster(data=r_df, aes(x=lon, y=lat, fill=withdrawal))

ggplotly()

#timeseries

china_lon <- 120
china_lat <- 33

lon_diff = lon - china_lon
china_x = which.min(abs(lon_diff))
lat_diff = lat - china_lat
china_y = which.min(abs(lat_diff))
irr_withd.series = irr_withd.array[china_x, china_y, ]

china_df <- data.frame(Year= seq(from=1, to=40, by=1), Withdrawal=irr_withd.series)
ggplot(data=china_df, aes(x=Year, y=Withdrawal, group=1)) +
  geom_line() + 
  ggtitle("Yearly irrigation withdrawal (m)") + 
  theme_bw() 
