# smaller helper functions for cleaning VI and climate data


# find common indices between VI and climate data 
find_common_index <- function(VI_data,climate_data) {
  
  # inputs:
  # VI_data= vegiation index data
  # Climate_data= any data from the climate datasets
  
  # outputs:
  # index that are shared between VI data and climate data
  
  idx_share=c()
  for (i in 1:length(VI_data$Point_id)){
    tryCatch({
      idx_share[i]= which(VI_data$Point_id[i]==climate_data$Sample_ID)
    }, error=function(e){"missing ID"}
    )
  }
  
    return(idx_share)
  
}


# function for getting correct time format, adding seasons, months and dates as well

# assign seasons based on dates
library(lubridate)

add_dates <- function(dat) {
  
  # input: 
  # data which has a date column in the format of d-m-y
  
  # outputs:
  # added date column
  # added month column
  # added season column
  
  # Convert the date strings to a Date object
  dates <- as.Date(dat$date, format = "%d-%m-%y")
  
  # add months
  dat$month <- as.POSIXlt(dat$date)$mon + 1
  
  # add seasons
  dat$season <- cut(dat$month,
                    #breaks = c(0, 3, 6, 9, 12),
                    breaks = c(1, 3, 6, 9, 11, 12),
                    labels = c("Winter", "Spring", "Summer", "Fall","Winter"),
                    include.lowest = TRUE)
  
  
  
  # add dates in correct format
  dat$dates= dates
  
  return(dat)
  
}

season_check <- function(dat) {
  idx_wi=which(dat$month %in% c(12,1,2)==TRUE)
  idx_sp=which(dat$month %in% c(3,4,5)==TRUE)
  idx_su=which(dat$month %in% c(6,7,8)==TRUE)
  idx_fa=which(dat$month %in% c(9,10,11)==TRUE)
  
  if (length(idx_wi)>0){
    dat$season[idx_wi]="Winter"
  }
  
  else{
    dat$season[idx_wi]=NULL
  }
  
  if (length(idx_sp)>0){
    dat$season[idx_sp]="Spring"
  }
  
  else{
    dat$season[idx_sp]=NULL
  }
  
  if (length(idx_su)>0){
    dat$season[idx_su]="Summer"
  }
  
  else{
    dat$season[idx_su]=NULL
  }
  
  if (length(idx_fa)>0){
    dat$season[idx_fa]="Fall"
  }
  
  else{
    dat$season[idx_fa]=NULL
  }
  
  dat$season=as.factor(as.character(dat$season))
  
  return(dat)
}

# factor correction functions
correct_factors <- function(dat) {
  # inputs: 
  # Columns of LC (landcover), seasons and month
  
  # outputs:
  # corrected factor levels
  
  dat$LC=factor(dat$LC,levels=unique(dat$LC))
  dat$season=factor(dat$season,levels=unique(dat$season))
  dat$month=factor(dat$month,levels=sort(unique(dat$month)))
  
  return(dat)
}


# add climate zone to the dataset based on lat and lon coordinates
add_climate_zone <- function(dat) {
  library(kgc)
  data=data.frame(dat$TH_LAT,dat$TH_LONG)
  names(data)=c("Latitude","Longitude")
  data <- data.frame(data, rndCoord.lat = RoundCoordinates(data$Latitude), rndCoord.lon = RoundCoordinates(data$Longitude))
  dat_climate=data.frame(data,ClimateZ=LookupCZ(data))
  
  dat$climate=dat_climate$ClimateZ # climate zones
  
  # detach to aviod bugs
  detach("package:kgc", unload = TRUE)
  detach("package:plyr", unload = TRUE)
  
  return(dat)
  
}

fix_gps_format=function(dat){
  
  # inputs: data with gps coordinate columns
  # outputs: data with correct format as seperate lat and lon columns
  
  test=names(dat)=="gps_point" 
  if (any(test)=="TRUE"){
    
    print("fixing data")
    
    N=length(dat$gps_point)
    lat_vect=c()
    lon_vect=c()
    
    for (i in 1:N ){
      input_string=dat$gps_point[i]
      numbers_string <- gsub("POINT \\(|\\)", "", input_string)
      numbers <- strsplit(numbers_string, " ")[[1]]
      
      longitude <- as.numeric(numbers[1])
      latitude <- as.numeric(numbers[2])
      
      lon_vect[i]=longitude
      lat_vect[i]=latitude
      
    }
   
    dat$TH_LAT=lat_vect
    dat$TH_LONG=lon_vect
    
    
  }
  
  else{
    print("data is fine")
    dat=dat
  }
 
  return(dat)
   
}

#dat=dat_2018_lan8_cleaned

prev_moth_climate <- function(dat,year) {
  
  # input:
  # dataframw with meant
  
  dat$prev_air_temp=NA
  dat$prev_soil_temp=NA
  dat$prev_soil_water=NA
  
  string_air_temp <- sprintf("mean_t2m_%d-", year)
  string_soil_temp <- sprintf("mean_soiltemp_%d-", year)
  string_soil_water <- sprintf("mean_soilwater_%d-", year)
  
  for (i in 1:nrow(dat)){
    month_value <- as.numeric(as.character(dat$month[i])) 
    # air temperature
    previous_month_temp <- dat[i, paste0(string_air_temp, sprintf("%02d", month_value))]
    dat$prev_air_temp[i]=previous_month_temp
    # soil temp
    previous_month_soil_temp <- dat[i, paste0(string_soil_temp, sprintf("%02d", month_value))]
    dat$prev_soil_temp[i]=previous_month_soil_temp
    # soil water content
    previous_month_soil_water <- dat[i, paste0(string_soil_water, sprintf("%02d", month_value))]
    dat$prev_soil_water[i]=previous_month_soil_water
  }
  
  return(dat)
  
}



yearly_means <- function(dat) {
  
  # inputs:
  # data set
  
  # outputs:
  # added yearly air temp coulmm
  # added yearly soil water column
  # added yearly soil temp column
  
  idx_airtemp=grep("mean_t2m", names(dat))
  idx_soiltemp=grep("mean_soiltemp", names(dat))
  idx_soilwater=grep("mean_soilwater", names(dat))
  
  dat$soil_temp_yearly=rowMeans(dat[,idx_soiltemp], na.rm = TRUE)
  dat$soil_water_yearly=rowMeans(dat[,idx_soilwater], na.rm = TRUE)
  dat$air_temp_yearly=rowMeans(dat[,idx_airtemp], na.rm = TRUE)
  
  return(dat)
  
}

crop_plotting_colum<- function(dat) {
  
  # change B11, B13 and B16 to wheat barley and maize
  crop_type_natural=rep(NA,length(dat$crop_type))
  
  B11_idx=which(dat$`Crop type`=="B11")
  B13_idx=which(dat$`Crop type`=="B13")
  B16_idx=which(dat$`Crop type`=="B16")
  
  crop_type_natural[B11_idx]=rep("Wheat",length(B11_idx))
  crop_type_natural[B13_idx]=rep("Barley",length(B13_idx))
  crop_type_natural[B16_idx]=rep("Maize",length(B16_idx))
  
  dat$crop_type_natural=crop_type_natural
  
  return(dat)
}


current_moth_climate <- function(dat) {
   # inputs:
   # dataset 
  
   # outputs:
   # current month soil temp, water temp and air temp
  
  
  dat$current_air_temp=NA
  dat$current_soil_temp=NA
  dat$current_soil_water=NA
  
  idx_airtemp=grep("mean_t2m", names(dat))
  idx_soiltemp=grep("mean_soiltemp", names(dat))
  idx_soilwater=grep("mean_soilwater", names(dat))
  
  N=length(dat$date)
  
  for (i in 1:N){
    
    month_from_date <- format(as.Date(dat$date[i]), "%m")
    
    idx_air=grep(paste0("-", month_from_date), names(dat[,idx_airtemp]))
    idx_temp=grep(paste0("-", month_from_date), names(dat[,idx_soiltemp]))
    idx_water=grep(paste0("-", month_from_date), names(dat[,idx_soilwater]))
    
    dat$current_air_temp[i]=dat[i,idx_airtemp][idx_air]
    dat$current_soil_temp[i]=dat[i,idx_soiltemp][idx_temp]
    dat$current_soil_water[i]=dat[i,idx_soilwater][idx_water]
    
    
    
  }
  
  # remove all months that are not part of the previous month
  dat=dat[,-c(idx_airtemp,idx_soiltemp,idx_soilwater)]
  
  dat$current_air_temp=do.call("c",dat$current_air_temp)
  dat$current_soil_temp=do.call("c",dat$current_soil_temp)
  dat$current_soil_water=do.call("c", dat$current_soil_water)
  
  
  return(dat)
  
}


accumlated_metro <- function(dat,metro_variable_idx) {
  
  N=dim(dat)[1]
  acc_sum=c()
  
  for (i in 1:N){
    month_match=as.numeric(dat[i,]$month)
    acc_sum[i]=do.call("sum",c(dat[1,metro_variable_idx][1:month_match]))
  }
  
  return(acc_sum)
  
}

