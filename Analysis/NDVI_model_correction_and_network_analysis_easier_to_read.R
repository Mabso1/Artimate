# analyse and read data from sentinel 2 and landsat 8
library(readr)

# read in climate data
setwd("~/Desktop/nitrogen_phosphat_predictions/remote_sensing_data/climate data/climate_data_2015/corrected_group_names") # 2015 dir
air_temp_2015=read_csv("air_temp_grouped_correct.csv")
soil_temp_2015=read_csv("soil_temp_grouped_correct.csv")
soil_type_2015=read_csv("soil_type_grouped_correct.csv")
soil_water_2015=read_csv("soil_water_grouped_correct.csv")

setwd("~/Desktop/nitrogen_phosphat_predictions/remote_sensing_data/climate data/climate_data_2018/corrected_group_names") # 2018 dir
air_temp_2018=read_csv("air_temp_grouped_correct.csv")
soil_temp_2018=read_csv("soil_temp_grouped_correct.csv")
soil_type_2018=read_csv("soil_type_grouped_correct.csv")
soil_water_2018=read_csv("soil_water_grouped_correct.csv")

setwd("~/Desktop/soil data analysis/climate_data_2018_ekstra/corrected_data")
air_temp_2018_1=read_csv("air_temp_grouped_correct.csv")
soil_temp_2018_1=read_csv("soil_temp_grouped_correct.csv")
soil_type_2018_1=read_csv("soil_type_grouped_correct.csv")
soil_water_2018_1=read_csv("soil_water_grouped_correct.csv")

air_temp_2018_1=rbind(air_temp_2018,air_temp_2018_1)
soil_temp_2018_1=rbind(soil_temp_2018,soil_temp_2018_1)
soil_type_2018_1=rbind(soil_type_2018,soil_type_2018_1)
soil_water_2018_1=rbind(soil_water_2018,soil_water_2018_1)

# read in VI + lucas data
setwd("~/Desktop/nitrogen_phosphat_predictions/remote_sensing_data/vegitation indices")

# get landsat 8 data
setwd("~/Desktop/soil data analysis/new script")
VI_data_2018_lan8=read_csv('landsat8.csv')

setwd("~/Desktop/nitrogen_phosphat_predictions/remote_sensing_data/vegitation indices")
VI_data_2015_lan8=read_csv('landsat8_2015.csv')

VI_data_2015_lan8=VI_data_2015_lan8[-grep("2018",VI_data_2015_lan8$date),]

# read in the NDVI values and lucas soil data connected to the values sentinel 2 dataset
setwd("~/Desktop/lucas_annoations") # set to your dir
VI_data_2018_sen2 <- read_csv("lucas_soil_ndvi_new8_postive.csv") # lucas soil data + NDVI values


##### clean up the data, one dataset at a time (eg. 2015 and 2018 separately followed by final merging) ####

# load helper functions
setwd("~/Desktop/nitrogen_phosphat_predictions/functions script")
source("helper functions.R")


idx_share_2018_sen2=na.omit(find_common_index(VI_data = VI_data_2018_sen2,climate_data =air_temp_2018))
VI_data_2018_sen2=VI_data_2018_sen2[idx_share_2018_sen2,]

# ok

idx_share_2018_lan8=find_common_index(VI_data = VI_data_2018_lan8,climate_data =air_temp_2018_1)
idx_share_2015_lan8=find_common_index(VI_data = VI_data_2015_lan8,climate_data =air_temp_2015)

# remove the non common data points and merge
climate_2018_lan8_combined=cbind(air_temp_2018_1[,-1],soil_temp_2018_1[,-1],soil_type_2018_1[,-1],soil_water_2018_1[,-1]) # merge climate data (remove redundant id columns)
climate_2015_lan8_combined=cbind(air_temp_2015[,-1],soil_temp_2015[,-1],soil_type_2015[,-1],soil_water_2015[,-1])

climate_2018_sen2_combined=cbind(air_temp_2018[,-1],soil_temp_2018[,-1],soil_type_2018[,-1],soil_water_2018[,-1]) # merge climate data (remove redundant id columns)


# ok

climate_2015_lan8=climate_2015_lan8_combined
climate_2018_lan8=climate_2018_lan8_combined
climate_2018_sen2=climate_2018_sen2_combined

# ok


dat_2018_lan8=cbind(VI_data_2018_lan8,climate_2018_lan8[idx_share_2018_lan8,]) # combined VI and climate data 2018
dat_2015_lan8=cbind(VI_data_2015_lan8,climate_2015_lan8[idx_share_2015_lan8,]) # combined VI and climate data 2018

dat_2018_sen2=cbind(VI_data_2018_sen2,climate_2018_sen2) # combined VI and climate data 2018

# ok


#### fix the format of the data (time format and factor format) ####

# assign seasons, month and correctly formatted dates
dat_2015_lan8=add_dates(dat_2015_lan8)

dat_2018_lan8=add_dates(dat_2018_lan8)

dat_2018_sen2=add_dates(dat_2018_sen2)

#ok

# set crop types, seasons and months as factors
dat_2015_lan8=correct_factors(dat_2015_lan8)

dat_2018_lan8=correct_factors(dat_2018_lan8)

dat_2018_sen2=correct_factors(dat_2018_sen2)

#ok

#  add climate zones based on lat and lon coords
dat_2015_lan8=fix_gps_format(dat_2015_lan8) # format data correctly
dat_2018_lan8=fix_gps_format(dat_2018_lan8) # format data correctly
dat_2018_sen2=fix_gps_format(dat_2018_sen2)

#ok
dat_2015_lan8=add_climate_zone(dat_2015_lan8)
dat_2018_lan8=add_climate_zone(dat_2018_lan8)
dat_2018_sen2=add_climate_zone(dat_2018_sen2)

#ok

# clean data leaving only air temp, mean soil water and soil temp
mean_air_temp_2018_lan8_idx <- grep("mean_t2m_2018", names(dat_2018_lan8))
mean_soiltemp_2018_lan8_idx <- grep("mean_soiltemp_2018", names(dat_2018_lan8))
mean_soilwater_2018_lan8_idx <- grep("mean_soilwater_2018", names(dat_2018_lan8))

mean_air_temp_2018_sen2_idx <- grep("mean_t2m_2018", names(dat_2018_sen2))
mean_soiltemp_2018_sen2_idx <- grep("mean_soiltemp_2018", names(dat_2018_sen2))
mean_soilwater_2018_sen2_idx <- grep("mean_soilwater_2018", names(dat_2018_sen2))

mean_air_temp_2015_lan8_idx <- grep("mean_t2m_2015", names(dat_2015_lan8))
mean_soiltemp_2015_lan8_idx <- grep("mean_soiltemp_2015", names(dat_2015_lan8))
mean_soilwater_2015_lan8_idx <- grep("mean_soilwater_2015", names(dat_2015_lan8))

# clean the data names and keep only relevant data
dat_2015_lan8_cleaned=data.frame(
  dat_2015_lan8$N,
  dat_2015_lan8$OC,
  dat_2015_lan8$EC,
  dat_2015_lan8$K,
  dat_2015_lan8$P,
  dat_2015_lan8$Mean_NDVI,
  dat_2015_lan8$climate,
  dat_2015_lan8$LC,
  dat_2015_lan8$month,
  dat_2015_lan8$season,
  dat_2015_lan8$date,
  dat_2015_lan8$soil_type,
  dat_2015_lan8[,mean_air_temp_2015_lan8_idx],
  dat_2015_lan8[,mean_soiltemp_2015_lan8_idx],
  dat_2015_lan8[,mean_soilwater_2015_lan8_idx],
  dat_2015_lan8$Point_id,
  dat_2015_lan8$pH_CaCl2
)

names(dat_2015_lan8_cleaned)=c("N","OC","EC","K","P","NDVI","climate","crop_type", "month","season","date","soil_type",
                               names(dat_2015_lan8[,mean_air_temp_2015_lan8_idx]),names(dat_2015_lan8[,mean_soiltemp_2015_lan8_idx]),names(dat_2015_lan8[,mean_soilwater_2015_lan8_idx]),
                               "ID","CaCl2")

# clean the data names and keep only relevant data
dat_2018_lan8_cleaned=data.frame(
  dat_2018_lan8$N,
  dat_2018_lan8$OC,
  dat_2018_lan8$EC,
  dat_2018_lan8$K,
  dat_2018_lan8$P,
  dat_2018_lan8$Mean_NDVI,
  dat_2018_lan8$climate,
  dat_2018_lan8$LC,
  dat_2018_lan8$month,
  dat_2018_lan8$season,
  dat_2018_lan8$date,
  dat_2018_lan8$soil_type,
  dat_2018_lan8[,mean_air_temp_2018_lan8_idx],
  dat_2018_lan8[,mean_soiltemp_2018_lan8_idx],
  dat_2018_lan8[,mean_soilwater_2018_lan8_idx],
  dat_2018_lan8$Point_id,
  dat_2018_lan8$pH_CaCl2
)

names(dat_2018_lan8_cleaned)=c("N","OC","EC","K","P","NDVI","climate","crop_type", "month","season","date","soil_type",
                            names(dat_2018_lan8[,mean_air_temp_2018_lan8_idx]),names(dat_2018_lan8[,mean_soiltemp_2018_lan8_idx]),names(dat_2018_lan8[,mean_soilwater_2018_lan8_idx]),
                            "ID","CaCl2")

dat_2018_sen2_cleaned=data.frame(
  dat_2018_sen2$N,
  dat_2018_sen2$OC,
  dat_2018_sen2$EC,
  dat_2018_sen2$K,
  dat_2018_sen2$P,
  dat_2018_sen2$Mean,
  dat_2018_sen2$climate,
  dat_2018_sen2$LC,
  dat_2018_sen2$month,
  dat_2018_sen2$season,
  dat_2018_sen2$date,
  dat_2018_sen2$soil_type,
  dat_2018_sen2[,mean_air_temp_2018_sen2_idx],
  dat_2018_sen2[,mean_soiltemp_2018_sen2_idx],
  dat_2018_sen2[,mean_soilwater_2018_sen2_idx],
  dat_2018_sen2$Point_id,
  dat_2018_sen2$pH_CaCl2
)

names(dat_2018_sen2_cleaned)=c("N","OC","EC","K","P","NDVI","climate","crop_type", "month","season","date","soil_type",
                               names(dat_2018_sen2[,mean_air_temp_2018_sen2_idx]),names(dat_2018_sen2[,mean_soiltemp_2018_sen2_idx]),names(dat_2018_lan8[,mean_soilwater_2018_lan8_idx]),
                               "ID","CaCl2")

# ok


# subset crop types
dat_2018_sen2_cleaned=dat_2018_sen2_cleaned[dat_2018_sen2_cleaned$`crop_type` %in% c("B11","B13","B16"), ] 
dat_2018_lan8_cleaned=dat_2018_lan8_cleaned[dat_2018_lan8_cleaned$`crop_type` %in% c("B11","B13","B16"), ] 
dat_2015_lan8_cleaned=dat_2015_lan8_cleaned[dat_2015_lan8_cleaned$`crop_type` %in% c("B11","B13","B16"), ] 

# ok

# add yearly climate contribution
dat_2015_lan8_cleaned=yearly_means(dat = dat_2015_lan8_cleaned)
dat_2018_lan8_cleaned=yearly_means(dat = dat_2018_lan8_cleaned)
dat_2018_sen2_cleaned=yearly_means(dat = dat_2018_sen2_cleaned)

# ok

# add previous month temperature to each dataset as well
dat_2015_lan8_cleaned=prev_moth_climate(dat_2015_lan8_cleaned,2015)
dat_2018_lan8_cleaned=prev_moth_climate(dat_2018_lan8_cleaned,2018)
dat_2018_sen2_cleaned=prev_moth_climate(dat_2018_sen2_cleaned,2018)

#ok

# remove column not belonging to prev month or yearly contribution
dat_2018_sen2_cleaned=dat_2018_sen2_cleaned[,c(1:12,49:56)]
dat_2018_lan8_cleaned=dat_2018_lan8_cleaned[,c(1:12,49:56)]
dat_2015_lan8_cleaned=dat_2015_lan8_cleaned[,c(1:12,49:56)]

# ok


# remove winter months and sparsely populated climate zones
dat_2018_sen2_cleaned=dat_2018_sen2_cleaned[dat_2018_sen2_cleaned$month %in% c("5","6","7","8","9"), ]
dat_2018_lan8_cleaned=dat_2018_lan8_cleaned[dat_2018_lan8_cleaned$month %in% c("5","6","7","8","9"), ]
dat_2015_lan8_cleaned=dat_2015_lan8_cleaned[dat_2015_lan8_cleaned$month %in% c("5","6","7","8","9"), ]

# ok

# make sure the seasons are correctly encoded
dat_2018_sen2_cleaned=season_check(dat_2018_sen2_cleaned)
dat_2018_lan8_cleaned=season_check(dat_2018_lan8_cleaned)
dat_2015_lan8_cleaned=season_check(dat_2015_lan8_cleaned)

table(dat_2018_lan8_cleaned$climate)
rm_idx1=which(dat_2018_lan8_cleaned$climate=="BSh")
rm_idx2=which(dat_2018_lan8_cleaned$climate=="Climate Zone info missing")

dat_2018_lan8_cleaned=dat_2018_lan8_cleaned[-c(rm_idx1,rm_idx2),]

table(dat_2018_sen2_cleaned$climate)


table(dat_2015_lan8_cleaned$climate)
rm_idx1=which(dat_2015_lan8_cleaned$climate=="BSh")
rm_idx2=which(dat_2015_lan8_cleaned$climate=="Climate Zone info missing")

dat_2015_lan8_cleaned=dat_2015_lan8_cleaned[-c(rm_idx1,rm_idx2),]

table(dat_2018_sen2_cleaned$climate)

rm_idx1=which(dat_2018_sen2_cleaned$climate=="Climate Zone info missing")
rm_idx2=which(dat_2018_sen2_cleaned$climate=="BSh")
rm_idx3=which(dat_2018_sen2_cleaned$climate=="ET")
rm_idx4=which(dat_2018_sen2_cleaned$climate=="Cfc")

dat_2018_sen2_cleaned=dat_2018_sen2_cleaned[-c(rm_idx1,rm_idx2,rm_idx3,rm_idx4),]

# make sure data is in the correct format with correct factor levels
dat_2018_sen2_cleaned$climate=as.factor(as.character(dat_2018_sen2_cleaned$climate))
dat_2018_sen2_cleaned$month=as.factor(as.character(dat_2018_sen2_cleaned$month))
dat_2018_sen2_cleaned$season=as.factor(as.character(dat_2018_sen2_cleaned$season))
dat_2018_sen2_cleaned$soil_type=as.factor(as.character(dat_2018_sen2_cleaned$soil_type))
dat_2018_sen2_cleaned$crop_type=as.factor(as.character(dat_2018_sen2_cleaned$crop_type))

idx_na=which(is.na(dat_2018_sen2_cleaned$NDVI)==TRUE)
dat_2018_sen2_cleaned=dat_2018_sen2_cleaned[-idx_na,]


idx_na=which(is.na(dat_2018_lan8_cleaned$NDVI)==TRUE)
dat_2018_lan8_cleaned=dat_2018_lan8_cleaned[-idx_na,]

idx_na=which(is.na(dat_2015_lan8_cleaned$NDVI)==TRUE)
dat_2015_lan8_cleaned=dat_2015_lan8_cleaned[-idx_na,]

# landsat 8
dat_2018_lan8_cleaned$climate=as.factor(as.character(dat_2018_lan8_cleaned$climate))
dat_2018_lan8_cleaned$month=as.factor(as.character(dat_2018_lan8_cleaned$month))
dat_2018_lan8_cleaned$season=as.factor(as.character(dat_2018_lan8_cleaned$season))
dat_2018_lan8_cleaned$soil_type=as.factor(as.character(dat_2018_lan8_cleaned$soil_type))
dat_2018_lan8_cleaned$crop_type=as.factor(as.character(dat_2018_lan8_cleaned$crop_type))

dat_2015_lan8_cleaned$climate=as.factor(as.character(dat_2015_lan8_cleaned$climate))
dat_2015_lan8_cleaned$month=as.factor(as.character(dat_2015_lan8_cleaned$month))
dat_2015_lan8_cleaned$season=as.factor(as.character(dat_2015_lan8_cleaned$season))
dat_2015_lan8_cleaned$soil_type=as.factor(as.character(dat_2015_lan8_cleaned$soil_type))
dat_2015_lan8_cleaned$crop_type=as.factor(as.character(dat_2015_lan8_cleaned$crop_type))

# convert the data to correct formats
dat_2018_sen2_cleaned$P[which(dat_2018_sen2_cleaned$P=="< LOD")]=0.0 # set obs below limit of detection to 0
dat_2018_sen2_cleaned$P=as.numeric(dat_2018_sen2_cleaned$P)

# landsat 8
dat_2018_lan8_cleaned$P[which(dat_2018_lan8_cleaned$P=="< LOD")]=0.0 # set obs below limit of detection to 0
dat_2018_lan8_cleaned$P=as.numeric(dat_2018_lan8_cleaned$P)

dat_2018_lan8_cleaned$OC=as.numeric(dat_2018_lan8_cleaned$OC)
dat_2018_lan8_cleaned$N=as.numeric(dat_2018_lan8_cleaned$N)

dat_2015_lan8_cleaned$P[which(dat_2015_lan8_cleaned$P=="< LOD")]=0.0 # set obs below limit of detection to 0
dat_2015_lan8_cleaned$P=as.numeric(dat_2015_lan8_cleaned$P)

dat_2015_lan8_cleaned$OC=as.numeric(dat_2015_lan8_cleaned$OC)
dat_2015_lan8_cleaned$N=as.numeric(dat_2015_lan8_cleaned$N)



# add dummy variables for plotting

# change B11, B13 and B16 to wheat barley and maize
crop_type_natural=rep(NA,length(dat_2018_sen2_cleaned$crop_type))

B11_idx=which(dat_2018_sen2_cleaned$crop_type=="B11")
B13_idx=which(dat_2018_sen2_cleaned$crop_type=="B13")
B16_idx=which(dat_2018_sen2_cleaned$crop_type=="B16")

crop_type_natural[B11_idx]=rep("Wheat",length(B11_idx))
crop_type_natural[B13_idx]=rep("Barley",length(B13_idx))
crop_type_natural[B16_idx]=rep("Maize",length(B16_idx))

dat_2018_sen2_cleaned$crop_type_natural=crop_type_natural

# change climate zones into non abbv format
# Create a named vector for mapping
idx_BSk=which(dat_2018_sen2_cleaned$climate=="BSk")
idx_Csa=which(dat_2018_sen2_cleaned$climate=="Csa")
idx_Csb=which(dat_2018_sen2_cleaned$climate=="Csb")
idx_Cfa=which(dat_2018_sen2_cleaned$climate=="Cfa")
idx_Cfb=which(dat_2018_sen2_cleaned$climate=="Cfb")
idx_Dfb=which(dat_2018_sen2_cleaned$climate=="Dfb")
idx_Dfc=which(dat_2018_sen2_cleaned$climate=="Dfc")

dat_2018_sen2_cleaned$climate_full=rep(NA,length(dat_2018_sen2_cleaned$climate))
dat_2018_sen2_cleaned$climate_full[idx_BSk]="Bsk (Cold semi-arid climate)"
dat_2018_sen2_cleaned$climate_full[idx_Csa]="Csa (Hot-summer Mediterranean climate)"
dat_2018_sen2_cleaned$climate_full[idx_Csb]="Csb (Warm-summer Mediterranean climate)"
dat_2018_sen2_cleaned$climate_full[idx_Cfa]="Cfa (Humid subtropical climate)"
dat_2018_sen2_cleaned$climate_full[idx_Cfb]="Cfb (Temperate oceanic climate)"
dat_2018_sen2_cleaned$climate_full[idx_Dfb]="Dfb (Warm-summer humid continental climate)"
dat_2018_sen2_cleaned$climate_full[idx_Dfc]="Dfc (Subarctic climate)"

dat_2018_sen2_cleaned$climate_full=as.factor(dat_2018_sen2_cleaned$climate_full)

# 2018 landsat dummy vars
crop_type_natural=rep(NA,length(dat_2018_lan8_cleaned$crop_type))

B11_idx=which(dat_2018_lan8_cleaned$crop_type=="B11")
B13_idx=which(dat_2018_lan8_cleaned$crop_type=="B13")
B16_idx=which(dat_2018_lan8_cleaned$crop_type=="B16")

crop_type_natural[B11_idx]=rep("Wheat",length(B11_idx))
crop_type_natural[B13_idx]=rep("Barley",length(B13_idx))
crop_type_natural[B16_idx]=rep("Maize",length(B16_idx))

dat_2018_lan8_cleaned$crop_type_natural=crop_type_natural

# change climate zones into non abbv format
# Create a named vector for mapping
idx_BSk=which(dat_2018_lan8_cleaned$climate=="BSk")
idx_Csa=which(dat_2018_lan8_cleaned$climate=="Csa")
idx_Csb=which(dat_2018_lan8_cleaned$climate=="Csb")
idx_Cfa=which(dat_2018_lan8_cleaned$climate=="Cfa")
idx_Cfb=which(dat_2018_lan8_cleaned$climate=="Cfb")
idx_Dfb=which(dat_2018_lan8_cleaned$climate=="Dfb")
idx_Dfc=which(dat_2018_lan8_cleaned$climate=="Dfc")

dat_2018_lan8_cleaned$climate_full=rep(NA,length(dat_2018_lan8_cleaned$climate))
dat_2018_lan8_cleaned$climate_full[idx_BSk]="Bsk (Cold semi-arid climate)"
dat_2018_lan8_cleaned$climate_full[idx_Csa]="Csa (Hot-summer Mediterranean climate)"
dat_2018_lan8_cleaned$climate_full[idx_Csb]="Csb (Warm-summer Mediterranean climate)"
dat_2018_lan8_cleaned$climate_full[idx_Cfa]="Cfa (Humid subtropical climate)"
dat_2018_lan8_cleaned$climate_full[idx_Cfb]="Cfb (Temperate oceanic climate)"
dat_2018_lan8_cleaned$climate_full[idx_Dfb]="Dfb (Warm-summer humid continental climate)"
dat_2018_lan8_cleaned$climate_full[idx_Dfc]="Dfc (Subarctic climate)"

dat_2018_lan8_cleaned$climate_full=as.factor(dat_2018_lan8_cleaned$climate_full)

# 2015 landsat dummy vars
crop_type_natural=rep(NA,length(dat_2015_lan8_cleaned$crop_type))

B11_idx=which(dat_2015_lan8_cleaned$crop_type=="B11")
B13_idx=which(dat_2015_lan8_cleaned$crop_type=="B13")
B16_idx=which(dat_2015_lan8_cleaned$crop_type=="B16")

crop_type_natural[B11_idx]=rep("Wheat",length(B11_idx))
crop_type_natural[B13_idx]=rep("Barley",length(B13_idx))
crop_type_natural[B16_idx]=rep("Maize",length(B16_idx))

dat_2015_lan8_cleaned$crop_type_natural=crop_type_natural

# change climate zones into non abbv format
# Create a named vector for mapping
idx_BSk=which(dat_2015_lan8_cleaned$climate=="BSk")
idx_Csa=which(dat_2015_lan8_cleaned$climate=="Csa")
idx_Csb=which(dat_2015_lan8_cleaned$climate=="Csb")
idx_Cfa=which(dat_2015_lan8_cleaned$climate=="Cfa")
idx_Cfb=which(dat_2015_lan8_cleaned$climate=="Cfb")
idx_Dfb=which(dat_2015_lan8_cleaned$climate=="Dfb")
idx_Dfc=which(dat_2015_lan8_cleaned$climate=="Dfc")

dat_2015_lan8_cleaned$climate_full=rep(NA,length(dat_2015_lan8_cleaned$climate))
dat_2015_lan8_cleaned$climate_full[idx_BSk]="Bsk (Cold semi-arid climate)"
dat_2015_lan8_cleaned$climate_full[idx_Csa]="Csa (Hot-summer Mediterranean climate)"
dat_2015_lan8_cleaned$climate_full[idx_Csb]="Csb (Warm-summer Mediterranean climate)"
dat_2015_lan8_cleaned$climate_full[idx_Cfa]="Cfa (Humid subtropical climate)"
dat_2015_lan8_cleaned$climate_full[idx_Cfb]="Cfb (Temperate oceanic climate)"
dat_2015_lan8_cleaned$climate_full[idx_Dfb]="Dfb (Warm-summer humid continental climate)"
dat_2015_lan8_cleaned$climate_full[idx_Dfc]="Dfc (Subarctic climate)"

dat_2015_lan8_cleaned$climate_full=as.factor(dat_2015_lan8_cleaned$climate_full)



# compare NDVI
matching_indices=which(dat_2018_lan8_cleaned$ID %in% dat_2018_sen2_cleaned$ID)

idx_list=c()
match_entery=dat_2018_lan8_cleaned$ID[matching_indices]
for (i in 1:length(match_entery)){
  idx_list[i]=which(dat_2018_sen2_cleaned$ID==match_entery[i])
}

plot(dat_2018_sen2_cleaned$NDVI[idx_list],dat_2018_lan8_cleaned$NDVI[matching_indices])
cor(dat_2018_sen2_cleaned$NDVI[idx_list],dat_2018_lan8_cleaned$NDVI[matching_indices])

dat_2018_sen2_cleaned=na.omit(dat_2018_sen2_cleaned)
dat_2018_sen2_cleaned=dat_2018_sen2_cleaned[-which(dat_2018_sen2_cleaned$EC>500),]


##### Exploratory analysis ####
library(ggplot2)
library(corrplot)
library(patchwork)

# plot correlation of numerical features
correlation_matrix <- cor((dat_2018_sen2_cleaned[,c(1:6,14:20)]),use = "complete.obs",method="pearson")
corrplot(correlation_matrix)


# plot NDVI vs dates with added trendline
P1=ggplot(dat_2015_lan8_cleaned, aes(x = date, y = NDVI, color = climate_full)) +
  geom_point() +  # Scatter points
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +  # Add single trend line across all crops
  labs(title = "Time Series: NDVI for Different Crop Types 2015 Landsat 8", 
       x = "Dates", y = "NDVI", color = "Climate zones") +
  theme(plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  facet_grid(crop_type_natural ~ ., scales = "free_y", switch = "y") +
  theme(strip.text.y = element_text(size = 12, colour = "black", angle = 90))

P2=ggplot(dat_2018_lan8_cleaned, aes(x = date, y = NDVI, color = climate_full)) +
  geom_point() +  # Scatter points
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +  # Add single trend line across all crops
  labs(title = "Time Series: NDVI for Different Crop Types 2018 Landsat 8", 
       x = "Dates", y = "NDVI", color = "Climate zones") +
  theme(plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  facet_grid(crop_type_natural ~ ., scales = "free_y", switch = "y") +
  theme(strip.text.y = element_text(size = 12, colour = "black", angle = 90))

P3=ggplot(dat_2018_lan8_cleaned, aes(x = date, y = NDVI, color = climate_full)) +
  geom_point() +  # Scatter points
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +  # Add single trend line across all crops
  labs(title = "Time Series: NDVI for Different Crop Types 2018 Sentinel-2", 
       x = "Dates", y = "NDVI", color = "Climate zones") +
  theme(plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  facet_grid(crop_type_natural ~ ., scales = "free_y", switch = "y") +
  theme(strip.text.y = element_text(size = 12, colour = "black", angle = 90))

P1 <- P1 + theme(legend.position = "none")
P2 <- P2 + theme(legend.position = "none")
P3 <- P3 + theme(legend.position = "none")

combined_plot <- P1 + P2 + P3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "Time Series: NDVI for Different Crop Types",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

# Display the combined plot
print(combined_plot)


# other plots
# climate vs NDVI
p1=ggplot(dat_2018_sen2_cleaned, aes(x = as.factor(climate_full), y = NDVI)) +
  geom_boxplot(fill = "blue") +
  labs(title = "NDVI by Climate Sentinel-2, 2018", x = "Climate", y = "NDVI") +
  theme_minimal()

p2=ggplot(dat_2018_lan8_cleaned, aes(x = as.factor(climate_full), y = NDVI)) +
  geom_boxplot(fill = "blue") +
  labs(title = "NDVI by Climate Landsat 8, 2018", x = "Climate", y = "NDVI") +
  theme_minimal()

p3=ggplot(dat_2015_lan8_cleaned, aes(x = as.factor(climate_full), y = NDVI)) +
  geom_boxplot(fill = "blue") +
  labs(title = "NDVI by Climate Landsat 8, 2015", x = "Climate", y = "NDVI") +
  theme_minimal()

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI by Climate",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

# Display the combined plot
print(combined_plot)

# NDVI vs moisture
p1=ggplot(dat_2018_sen2_cleaned, aes(x = soil_water_yearly, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs yearly average soil moisture, Sentinel-2, 2018", x = expression(paste("Moisture (", m^{3}, ")")), y = "NDVI")# +

p2=ggplot(dat_2018_lan8_cleaned, aes(x = soil_water_yearly, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs yearly average soil moisture, Landsat 8, 2018", x = expression(paste("Moisture (", m^{3}, ")")), y = "NDVI")

p3=ggplot(dat_2015_lan8_cleaned, aes(x = soil_water_yearly, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs yearly average soil moisture, Landsat 8, 2015", x = expression(paste("Moisture (", m^{3}, ")")), y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs yearly average soil moisture",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# NDVI vs soil temp
p1=ggplot(dat_2018_sen2_cleaned, aes(x = soil_temp_yearly, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs yearly average soil temperature, Sentinel-2, 2018", x = "Temperature (°K)", y = "NDVI")

p2=ggplot(dat_2018_lan8_cleaned, aes(x = soil_temp_yearly, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs yearly average soil temperature, Landsat 8, 2018", x = "Temperature (°K)", y = "NDVI")

p3=ggplot(dat_2015_lan8_cleaned, aes(x = soil_temp_yearly, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs yearly average soil temperature, Landsat 8, 2015", x = "Temperature (°K)", y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs yearly average soil temperature",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# NDVI vs air temp
p1=ggplot(dat_2018_sen2_cleaned, aes(x = air_temp_yearly, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs yearly average air temperature, Sentinel 2, 2018", x = "Temperature (°K)", y = "NDVI")
p2=ggplot(dat_2018_lan8_cleaned, aes(x = air_temp_yearly, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs yearly average air temperature, Landsat 8, 2018", x = "Temperature (°K)", y = "NDVI")
p3=ggplot(dat_2015_lan8_cleaned, aes(x = air_temp_yearly, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs yearly average air temperature, Landsat 8, 2015", x = "Temperature (°K)", y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs yearly average air temperature",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# NDVI vs one month prior moisture
p1=ggplot(dat_2018_sen2_cleaned, aes(x = prev_soil_water, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs one month prior soil moisture, Sentinel-2, 2018", x = expression(paste("Moisture (", m^{3}, ")")), y = "NDVI")

p2=ggplot(dat_2018_lan8_cleaned, aes(x = prev_soil_water, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs one month prior soil moisture, Landsat 8, 2018", x = expression(paste("Moisture (", m^{3}, ")")), y = "NDVI")

p3=ggplot(dat_2015_lan8_cleaned, aes(x = prev_soil_water, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs one month prior soil moisture, Landsat 8, 2015", x = expression(paste("Moisture (", m^{3}, ")")), y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs one month prior soil moisture",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# NDVI vs one month prior soil temperature
p1=ggplot(dat_2018_sen2_cleaned, aes(x = prev_soil_temp, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs one month prior soil temperature, Sentinel-2, 2018", x = "Temperature (°K)", y = "NDVI")

p2=ggplot(dat_2018_lan8_cleaned, aes(x = prev_soil_temp, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs one month prior soil temperature, Landsat 8, 2018", x = "Temperature (°K)", y = "NDVI")

p3=ggplot(dat_2015_lan8_cleaned, aes(x = prev_soil_temp, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs one month prior soil temperature, Landsat 8, 2015", x = "Temperature (°K)", y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs one month prior soil temperature",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# NVDI vs one month prior air temperature
p1=ggplot(dat_2018_sen2_cleaned, aes(x = prev_air_temp, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs one month prior air temperature, Sentinel-2, 2018", x = "Temperature (°K)", y = "NDVI")
p2=ggplot(dat_2018_lan8_cleaned, aes(x = prev_air_temp, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs one month prior air temperature, Landsat 8, 2018", x = "Temperature (°K)", y = "NDVI")
p3=ggplot(dat_2015_lan8_cleaned, aes(x = prev_air_temp, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs one month prior air temperature, Landsat 8, 2015", x = "Temperature (°K)", y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs one month prior air temperature",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# NDVI vs EC
p1=ggplot(dat_2018_sen2_cleaned[-733,], aes(x = log(EC), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs electric conductivity, Sentinel-2, 2018", x = "Salinity (mS/m)", y = "NDVI")

p2=ggplot(dat_2018_lan8_cleaned[-733,], aes(x = log(EC), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs electric conductivity, Landsat 8, 2018", x = "Salinity (mS/m)", y = "NDVI")

p3=ggplot(dat_2015_lan8_cleaned[-733,], aes(x = log(EC), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs electric conductivity, Landsat 8, 2015", x = "Salinity (mS/m)", y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs electric conductivity",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# NDVI OC
p1=ggplot(dat_2018_sen2_cleaned, aes(x = log(OC), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Organic Carbon, Sentinel-2, 2018", x = "mass (g/kg)", y = "NDVI")
p2=ggplot(dat_2018_lan8_cleaned, aes(x = log(OC), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Organic Carbon, Landsat 8, 2018", x = "mass (g/kg)", y = "NDVI")
p3=ggplot(dat_2015_lan8_cleaned, aes(x = log(OC), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Organic Carbon, Landsat8, 2015", x = "mass (g/kg)", y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs eExtractable Organic Carbon",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# NDVI vs P
p1=ggplot(dat_2018_sen2_cleaned, aes(x = P, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Phosphorus, Sentinel-2, 2018", x = "mass (mg/kg)", y = "NDVI")
p2=ggplot(dat_2018_lan8_cleaned, aes(x = P, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Phosphorus, Landsat 8, 2018", x = "mass (mg/kg)", y = "NDVI")
p3=ggplot(dat_2015_lan8_cleaned, aes(x = P, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Phosphorus, Landsat 8, 2015", x = "mass (mg/kg)", y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs Extractable Phosphorus",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)


# NDVI CaCL2
p1=ggplot(dat_2018_sen2_cleaned, aes(x = CaCl2, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs CaCl2, Sentinel-2, 2018", x = "pH", y = "NDVI")
p2=ggplot(dat_2018_lan8_cleaned, aes(x = CaCl2, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs CaCl2, Landsat 8, 2018", x = "pH", y = "NDVI")
p3=ggplot(dat_2015_lan8_cleaned, aes(x = CaCl2, y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs CaCl2, Landsat 8, 2015", x = "pH", y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs CaCl2",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# NDVI vs K
p1=ggplot(dat_2018_sen2_cleaned, aes(x = log(K), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Potassium, Sentinel-2, 2018", x = "mass (g/kg)", y = "NDVI")
p2=ggplot(dat_2018_lan8_cleaned, aes(x = log(K), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Potassium, Landsat 8, 2018", x = "mass (g/kg)", y = "NDVI")
p3=ggplot(dat_2015_lan8_cleaned, aes(x = log(K), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Potassium, Landsat 8, 2015", x = "mass (g/kg)", y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs Extractable Potassium",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# NDVI vs N
p1=ggplot(dat_2018_sen2_cleaned, aes(x = log(N), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Nitrogen, Sentinel-2, 2018", x = "mass (g/kg)", y = "NDVI")
p2=ggplot(dat_2018_lan8_cleaned, aes(x = log(N), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Nitrogen, Landsat 8, 2018", x = "mass (g/kg)", y = "NDVI")
p3=ggplot(dat_2015_lan8_cleaned, aes(x = log(N), y = NDVI)) +
  geom_point() +
  labs(title = "NDVI vs Extractable Nitrogen, Landsat 8, 2015", x = "mass (g/kg)", y = "NDVI")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI vs Extractable Nitrogen",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# crop type vs NDVI
ggplot(dat_2018_sen2_cleaned, aes(x = as.factor(crop_type), y = NDVI)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of NDVI by Crop type", x = "Crop_type", y = "NDVI") +
  theme_minimal()

# NDVI vs crop and month
p1=ggplot(dat_2018_sen2_cleaned, aes(x = as.factor(season), y = NDVI, fill = crop_type_natural)) +
  geom_boxplot() +
  scale_fill_discrete(name = "Crop Type") +
  labs(title = "NDVI relation by Season and Crop Type, Sentinel-2, 2018", x = "Season", y = "NDVI") +
  #theme_minimal() +
  facet_wrap(~ as.factor(crop_type_natural), scales = "free_y")

p2=ggplot(dat_2018_lan8_cleaned, aes(x = as.factor(season), y = NDVI, fill = crop_type_natural)) +
  geom_boxplot() +
  scale_fill_discrete(name = "Crop Type") +
  labs(title = "NDVI relation by Season and Crop Type, Landsat 8, 2018", x = "Season", y = "NDVI") +
  #theme_minimal() +
  facet_wrap(~ as.factor(crop_type_natural), scales = "free_y")

p3=ggplot(dat_2015_lan8_cleaned, aes(x = as.factor(season), y = NDVI, fill = crop_type_natural)) +
  geom_boxplot() +
  scale_fill_discrete(name = "Crop Type") +
  labs(title = "NDVI relation by Season and Crop Type, Landsat 8, 2015", x = "Season", y = "NDVI") +
  #theme_minimal() +
  facet_wrap(~ as.factor(crop_type_natural), scales = "free_y")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI relation by Season and Crop Type",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)


# climate vs crop type
p1=ggplot(dat_2018_sen2_cleaned, aes(x = climate, y = NDVI, fill = crop_type_natural)) +
  geom_boxplot() +
  labs(title = "NDVI relation by Climate zones and Crop type, Sentinel-2, 2018 ", x = "Climate", y = "NDVI") +
  scale_fill_discrete(name = "Crop Type") +
  #theme_minimal() +
  facet_wrap(~ as.factor(crop_type_natural), scales = "free_y")
p2=ggplot(dat_2018_lan8_cleaned, aes(x = climate, y = NDVI, fill = crop_type_natural)) +
  geom_boxplot() +
  labs(title = "NDVI relation by Climate zones and Crop type, Landsat 8, 2018 ", x = "Climate", y = "NDVI") +
  scale_fill_discrete(name = "Crop Type") +
  #theme_minimal() +
  facet_wrap(~ as.factor(crop_type_natural), scales = "free_y")
p3=ggplot(dat_2015_lan8_cleaned, aes(x = climate, y = NDVI, fill = crop_type_natural)) +
  geom_boxplot() +
  labs(title = "NDVI relation by Climate zones and Crop type, Landsat 8, 2015 ", x = "Climate", y = "NDVI") +
  scale_fill_discrete(name = "Crop Type") +
  #theme_minimal() +
  facet_wrap(~ as.factor(crop_type_natural), scales = "free_y")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI relation by Climate zones and Crop type",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)


# crop vs soil type
p1=ggplot(na.omit(dat_2018_sen2_cleaned), aes(x = as.factor(soil_type), y = NDVI, fill = crop_type_natural)) +
  geom_boxplot() +
  labs(title = "NDVI relation by Soil and Crop Type, Sentinel-2, 2018", x = "Soil type", y = "NDVI") +
  scale_fill_discrete(name = "Crop Type") +
  #theme_minimal() +
  facet_wrap(~ as.factor(crop_type_natural), scales = "free_y")

p2=ggplot(na.omit(dat_2018_lan8_cleaned), aes(x = as.factor(soil_type), y = NDVI, fill = crop_type_natural)) +
  geom_boxplot() +
  labs(title = "NDVI relation by Soil and Crop Type, Landsat 8, 2018", x = "Soil type", y = "NDVI") +
  scale_fill_discrete(name = "Crop Type") +
  #theme_minimal() +
  facet_wrap(~ as.factor(crop_type_natural), scales = "free_y")

p3=ggplot(dat_2015_lan8_cleaned, aes(x = as.factor(soil_type), y = NDVI, fill = crop_type_natural)) +
  geom_boxplot() +
  labs(title = "NDVI relation by Soil and Crop Type, Landsat 8, 2015", x = "Soil type", y = "NDVI") +
  scale_fill_discrete(name = "Crop Type") +
  #theme_minimal() +
  facet_wrap(~ as.factor(crop_type_natural), scales = "free_y")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "NDVI relation by Soil and Crop Type",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

#### view how the NDVI values are clustering when not correcting for any variables ####
dat=na.omit(dat_2018_lan8_cleaned)
# read in the soil microbiome data (count data matrix)
tax_meta_soil_combo <- read_csv("~/Desktop/soil data analysis/meta_soil.csv")

# subset the matrix to the targeted crops
crop_type_idx=which(tax_meta_soil_combo$LC1_2018 %in% c("B11","B13","B16"))
tax_meta_soil_combo=tax_meta_soil_combo[crop_type_idx,]

# find the missing points
length(which(tax_meta_soil_combo$LUCAS_ID %in% dat$ID)) #

# set the taxon data to only include sample from which the NDVI values are availability 
idx_NDVI=which(tax_meta_soil_combo$LUCAS_ID %in% dat$ID)
idx_barcode=which(dat$ID %in%  tax_meta_soil_combo$LUCAS_ID)

# subset of bio-data where NDVI values are present 
bio_subset=dat[idx_barcode,]
bio_subset$barcode_id=tax_meta_soil_combo$BARCODE_ID[idx_NDVI]


# results from cluster analysis prior to abiotic correction of NDVI
cluster_relation_count_Hclust <- read_csv("~/Desktop/soil data analysis/bio_data/count_hierchical_result_cut1_3crops_grain_new_method.csv") # 3 crops
bar_code_temp=cluster_relation_count_Hclust$...1

idx_match=which(bar_code_temp %in% bio_subset$barcode_id)

cluster_hc=cluster_relation_count_Hclust[idx_match,]

bio_subset_hc_count=bio_subset[idx_match,]

bio_subset_hc_count$cluster=cluster_relation_count_Hclust$x

# non-corrected data
boxplot(NDVI ~ cluster, data = bio_subset_hc_count, main = "Boxplot of uncorrected NDVI by Cluster", xlab = "Cluster", ylab = "uncorrected NDVI")
summary(aov(NDVI~as.factor(cluster),data=bio_subset_hc_count))

# fancy plot (run above agian with different dataset as dat to get results)
p1=ggplot(data = bio_subset_hc_count, aes(x = factor(cluster), y = NDVI, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "Boxplot of raw NDVI by Cluster, Sentinel 2, 2018",
       x = "Cluster",
       y = "NDVI",
       color = "Cluster")+
  scale_fill_manual(values = c("blue", "orange","red"),name="Cluster")+
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=14),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),    # Adjust legend text size
        legend.title = element_text(size = 16))

p2=ggplot(data = bio_subset_hc_count, aes(x = factor(cluster), y = NDVI, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "Boxplot of raw NDVI by Cluster, Landsat 8, 2018",
       x = "Cluster",
       y = "NDVI",
       color = "Cluster")+
  scale_fill_manual(values = c("blue", "orange","red"),name="Cluster")+
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=14),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),    # Adjust legend text size
        legend.title = element_text(size = 16))   # Adjust legend title size)

combined_plot <- p1 + p2 +
  plot_layout(ncol = 2, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "Raw NDVI by Cluster for Sentinel-2 and Landsat 8",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 20, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

#### modelling the data ####

# split data into train and test
library(caret)
library(car)
# remove dummy variables
dat$climate_full=NULL
dat$crop_type_natural=NULL

# data with within 1 year 
set.seed(123)
trainIndex <- createDataPartition(dat$NDVI, p = 0.8, list = FALSE)

# Create training and testing datasets
train_dat <- dat[trainIndex, ]
test_dat <- dat[-trainIndex, ]

# data with two years (2015 and 2018) landsat 8
train_dat1=na.omit(dat_2018_lan8_cleaned)
test_dat1=na.omit(dat_2015_lan8_cleaned)

# train data = 2015, test data=2018
test_dat3=na.omit(dat_2018_lan8_cleaned)
train_dat3=na.omit(dat_2015_lan8_cleaned)

# data with mixed years
dat_2015_lan8_cleaned$year=rep("2015",length(dat_2015_lan8_cleaned$N))
dat_2018_lan8_cleaned$year=rep("2018",length(dat_2018_lan8_cleaned$N))

dat1=rbind(dat_2015_lan8_cleaned,dat_2018_lan8_cleaned)
dat1=na.omit(dat1)
dat1$year=as.factor(dat1$year)


set.seed(123)
trainIndex <- createDataPartition(dat1$NDVI, p = 0.8, list = FALSE)

# Create training and testing datasets
train_dat2 <- dat1[trainIndex, ]
test_dat2 <- dat1[-trainIndex, ]


train_dat3$year=NULL
test_dat3$year=NULL
# make simple model -year
model_lm <-lm(NDVI~ .-climate_full-crop_type_natural-ID-date-month+log(OC)-OC-N+log(N)-EC+log(EC),data=train_dat3)
summary(model_lm)
# view model status
Anova(model_lm,type="III")

# make two factor interaction model
model_lm=update(model_lm,~(.)^2,data=train_dat3)
summary(model_lm)

# Initial model
model_lm2 <- model_lm  # Assuming model_lm is your initial model

# Set a significance threshold
significance_level <- 0.05

# Function to safely remove terms from a formula
remove_term_from_formula <- function(formula, term_to_remove) {
  # Convert the formula to a character string
  formula_str <- paste(deparse(formula), collapse = " ")
  # Escape special characters in the term to remove
  term_to_remove_escaped <- gsub("([\\(\\)\\:])", "\\\\\\1", term_to_remove)
  # Use regex to remove the term
  updated_formula_str <- gsub(
    paste0("(^|\\+|\\s)", term_to_remove_escaped, "(\\+|$|\\s)"), 
    " ", 
    formula_str
  )
  # Convert the updated string back to a formula
  as.formula(updated_formula_str)
}

# Iterative term removal process (it takes a while!)
while (TRUE) {
  # Perform the drop1 test
  ss <- drop1(model_lm2, test = "Chi")
  
  # Exclude the `<none>` term
  ss <- ss[rownames(ss) != "<none>", ]
  
  # Get the p-values for the terms
  p_values <- ss$`Pr(>Chi)`
  
  # Remove NA entries (e.g., intercepts or untested terms)
  p_values <- p_values[!is.na(p_values)]
  
  # Check if any terms are above the significance threshold
  if (length(p_values) == 0 || all(p_values <= significance_level)) {
    # Break the loop if no terms are above the threshold
    break
  }
  
  # Find the term with the highest p-value
  var_to_remove <- rownames(ss)[which.max(p_values)]
  
  # Print the term to be removed
  print(paste("Removing variable:", var_to_remove))
  
  # Update the formula by removing the non-significant term
  current_formula <- formula(model_lm2)
  updated_formula <- remove_term_from_formula(current_formula, var_to_remove)
  
  # Update the model with the new formula
  model_lm2 <- update(model_lm2, updated_formula)
}

# getting weird error - removing manually from here!

# sentinel 2 model results within 1 year test and train (2018)
#model_lm3=update(model_lm2,~.-season:soil_temp_yearly-soil_type:prev_soil_temp-CaCl2-crop_type:CaCl2-climate:soil_water_yearly-K:log(OC)-season:prev_soil_temp-soil_temp_yearly:soil_water_yearly-P:climate -climate:crop_type-season:log(EC)-P:soil_temp_yearly-air_temp_yearly:log(OC)-log(N)-P:soil_water_yearly-crop_type:soil_water_yearly-soil_water_yearly:air_temp_yearly-K:crop_type-P:log(N)-P:log(OC)-soil_type:prev_soil_water-climate:log(EC)-CaCl2:soil_temp_yearly-CaCl2:air_temp_yearly-soil_water_yearly:log(EC)-prev_soil_water:log(EC)-CaCl2:prev_air_temp-CaCl2:prev_soil_temp-P:prev_air_temp-P:air_temp_yearly  -prev_air_temp:air_temp_yearly-prev_air_temp:soil_temp_yearly-prev_air_temp:log(OC)-crop_type:log(OC)-season:air_temp_yearly-season:prev_soil_water-prev_soil_water:soil_temp_yearly-season:soil_water_yearly-climate:soil_temp_yearly-P:crop_type-soil_temp_yearly:log(N)-air_temp_yearly:log(N)-P:season-CaCl2:log(OC)-CaCl2:log(N)-prev_soil_water:air_temp_yearly-prev_soil_temp:log(OC)-soil_type:prev_air_temp-season:log(N)-season:log(OC)-soil_type:log(N)-soil_type:log(OC)-log(OC):log(N)-prev_soil_temp:soil_temp_yearly-prev_soil_temp:air_temp_yearly-P:prev_soil_water-soil_type:log(EC)-season:soil_type-crop_type:soil_type-K:P-K:soil_water_yearly-prev_soil_water:log(N)-prev_soil_temp:log(N)-prev_air_temp:log(N)-log(N):log(EC),data=train_dat)

# landsat 8 model results within 1 year test and train results (2018)
#model_lm3=update(model_lm2,~.-CaCl2:soil_water_yearly-season:air_temp_yearly-prev_soil_water:log(N)-P:soil_water_yearly-season:prev_air_temp-P:soil_temp_yearly-air_temp_yearly:prev_air_temp-soil_type:prev_soil_temp-crop_type:soil_water_yearly-crop_type:soil_temp_yearly-climate:crop_type-soil_type:soil_temp_yearly-CaCl2:air_temp_yearly-crop_type:log(N)-K:P-K:crop_type-P:crop_type-P:log(EC)-crop_type:log(EC)-P:prev_air_temp-P:prev_soil_temp-climate:log(EC)-soil_type:log(N)-log(OC):log(N)-K:climate-soil_water_yearly:prev_soil_temp-soil_water_yearly:prev_soil_water-soil_temp_yearly:prev_soil_temp-air_temp_yearly:log(EC)-climate:season-prev_soil_temp:log(EC)-soil_type:log(OC)-P:climate-climate:log(N)-prev_air_temp:log(EC)-climate:log(OC)-prev_soil_water:log(OC)-soil_water_yearly:log(OC)-soil_water_yearly:log(N)-P:air_temp_yearly-P:soil_type-K:soil_type-crop_type:soil_type-crop_type:prev_soil_water-crop_type:prev_soil_temp-season:prev_soil_water-prev_air_temp:prev_soil_temp-K:soil_water_yearly-soil_temp_yearly:prev_soil_water-air_temp_yearly:prev_soil_water-prev_air_temp:prev_soil_water-K:log(EC)-soil_type:log(EC)-prev_soil_water:log(EC)-CaCl2:prev_soil_water-K:log(N)-P:log(N)-CaCl2:log(N)-P:log(OC)-CaCl2:log(OC)-K:log(OC)-soil_water_yearly:air_temp_yearly-climate:soil_water_yearly-soil_type:air_temp_yearly-season:log(EC)-air_temp_yearly:prev_soil_temp-air_temp_yearly:log(OC)-season:log(N)-season:log(OC)-season:CaCl2-log(N):log(EC),data=train_dat)

# landsat 8 model results with two seperate years for test and train set (2015,2018) results (2015= test set, 2018=train set)
#model_lm3=update(model_lm2,~.-season:air_temp_yearly -air_temp_yearly:prev_air_temp-crop_type:soil_temp_yearly -climate:crop_type-soil_type:prev_air_temp  -soil_type:air_temp_yearly-CaCl2:air_temp_yearly-K:crop_type-crop_type:log(EC)-K:P-P:log(EC)-prev_soil_water:log(OC)-K:soil_water_yearly-P:soil_water_yearly-climate:log(EC)-CaCl2:soil_water_yearly-air_temp_yearly:log(EC)-K:soil_type-soil_type:log(EC)-soil_type:log(N)-crop_type:CaCl2-prev_air_temp:log(EC)-climate:season-season:prev_soil_water-K:climate-crop_type:soil_type-season:log(N)-log(N):log(EC)-P:log(N),data=train_dat1)

# landsat 8 model results with two seperate years for test and train set (2015,2018) results (2015= test set, 2018=train set)
model_lm3=update(model_lm2,~.-K:season-K:air_temp_yearly-K:prev_soil_temp-crop_type:prev_air_temp-climate:soil_temp_yearly-climate:prev_soil_water-K:prev_air_temp-K:prev_soil_water-P:prev_soil_temp-P:soil_water_yearly-P:prev_air_temp-climate:log(EC)-prev_soil_temp:log(OC)-prev_air_temp:log(OC)-soil_temp_yearly:log(OC)-air_temp_yearly:log(OC)-soil_temp_yearly:log(EC)-prev_soil_temp:log(EC)-prev_air_temp:log(EC)-air_temp_yearly:log(EC)-crop_type:air_temp_yearly-CaCl2:soil_water_yearly-CaCl2:soil_temp_yearly-CaCl2:air_temp_yearly-prev_soil_temp:log(N)-season:log(N)-season:log(OC)-soil_type:prev_soil_water-soil_type:soil_temp_yearly-CaCl2:prev_soil_water-season:log(EC)-soil_type:log(EC)-P:soil_type-soil_water_yearly:log(EC)-prev_soil_water:log(EC)-prev_soil_water:log(OC)-soil_temp_yearly:prev_soil_temp-air_temp_yearly:prev_soil_temp-P:air_temp_yearly-P:crop_type-soil_water_yearly:log(OC)-soil_type:CaCl2-CaCl2:log(OC)-P:log(N)-K:P-prev_air_temp:log(N)-CaCl2:log(N)-crop_type:log(OC)-log(OC):log(EC)-K:log(EC)-P:soil_temp_yearly-P:season-P:prev_soil_water-K:CaCl2-crop_type:log(EC)-P:CaCl2-CaCl2:prev_air_temp-K:soil_type-K:log(N)-climate:prev_air_temp-K:soil_temp_yearly-soil_temp_yearly:log(N)-soil_type:prev_soil_temp-prev_soil_water:log(N)-soil_type:log(N)-climate:log(N)-soil_temp_yearly:prev_air_temp-climate:prev_soil_temp-log(N):log(EC)-crop_type:CaCl2-soil_type:air_temp_yearly-season:soil_type,data=train_dat3)

# landsat 8 model results with two mixed years for test and train set (2015,2018) resultsta = test_dat),NDVI=test_dat$NDVI)
#model_lm3=update(model_lm2,~.-season:soil_temp_yearly-P:crop_type-K:crop_type-prev_soil_temp:log(OC)-prev_air_temp:log(OC)-soil_water_yearly:prev_air_temp-P:soil_water_yearly-P:soil_temp_yearly-P:air_temp_yearly-prev_soil_water:log(OC)-CaCl2:log(OC)-CaCl2:prev_soil_temp-CaCl2:prev_air_temp-climate:prev_soil_water-climate:prev_soil_temp-soil_temp_yearly:prev_soil_water-climate:prev_air_temp-crop_type:air_temp_yearly-K:P-P:log(OC)-CaCl2:log(N)-soil_water_yearly:log(OC)-soil_temp_yearly:log(OC)-soil_type:log(N)-soil_type:log(EC)-soil_temp_yearly:log(EC)-P:season-soil_water_yearly:log(EC)-prev_soil_water:log(EC)-K:log(EC)-air_temp_yearly:prev_air_temp-season:air_temp_yearly-soil_temp_yearly:prev_air_temp-K:soil_water_yearly-climate:soil_temp_yearly-climate:air_temp_yearly-K:prev_soil_water-climate:CaCl2-prev_soil_water:log(N)-crop_type:log(OC)-crop_type:CaCl2-crop_type:log(EC)-CaCl2:soil_temp_yearly-CaCl2:soil_water_yearly-soil_type:soil_water_yearly-K:soil_type-season:prev_soil_temp-crop_type:soil_type-soil_type:prev_soil_water-climate:soil_water_yearly-soil_water_yearly:log(N)-soil_temp_yearly:log(N)-air_temp_yearly:log(N)-air_temp_yearly:log(OC)-P:climate-CaCl2:prev_soil_water-soil_type:CaCl2-P:prev_soil_water-season:log(EC)-log(OC):log(EC)-log(OC):log(N)-K:climate-log(N):log(EC),data=train_dat2)


# for manual removal
ss=drop1(model_lm3,test = "Chi")
var_to_remove <- rownames(ss)[which.max(ss$`Pr(>Chi)`)]
print(var_to_remove)
print(ss)

xtable::xtable(ss)

anova(model_lm3)
par(mfrow=c(1,2))
plot(model_lm,which = c(1,2))
mtext("Model Diagnostics (model E-1)", side = 3, line = - 2, outer = TRUE)

# do model preds on full data and test data
pred_lm_test=predict(model_lm3,test_dat3)


library(Metrics)
plot(pred_lm_test,test_dat3$NDVI)
rmse(pred_lm_test,test_dat3$NDVI)
1-rmse(pred_lm_test,test_dat3$NDVI)^2/var(test_dat3$NDVI)

model_lm_full=update(model_lm3,~.,data=dat1)
pred_lm_full=predict(model_lm_full,dat1)
rmse(pred_lm_full,dat1$NDVI)
summary(model_lm_full)
1-rmse(pred_lm_full,dat1$NDVI)^2/var(dat1$NDVI)


preds_test=data.frame(NDVI_pred=predict(model_lm3,newdata = test_dat1),NDVI=test_dat1$NDVI)
preds_full=data.frame(NDVI_pred=predict(model_lm3,newdata = dat1),NDVI=dat1$NDVI)
# plot linear model test and full data set preds vs actual
p1=ggplot(preds_test, aes(x = NDVI_pred, y = NDVI)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  # Add labels and customize the plot
  labs(x = "Predicted NDVI", y = "Observed NDVI", title = "Observed NDVI vs. Predicted NDVI (test set), 2018 Landsat 8")+
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=14),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

p2=ggplot(preds_full, aes(x = NDVI_pred, y = NDVI)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  # Add labels and customize the plot
  labs(x = "Predicted NDVI", y = "Observed NDVI", title = "Observed NDVI vs. Predicted NDVI (full dataset), 2018 Sentinel-2")+
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=14),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

combined_plot <- p1 + p2 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "Modelling NDVI with Linear regression model (2018 Sentinel-2 dataset)  ",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# try with an RF model instead
library(randomForest)
library(ranger)

param_grid=expand.grid(mtry = c(1:10),
                       splitrule = c( "variance"),
                       min.node.size = c(5, 10, 15))
cv_scheme <- trainControl(method = "repeatedcv",
                          repeats = 5,
                          number = 5,
                          verboseIter = FALSE)
#x=train_dat[,-c(6,9,11,13,21,22)] # get rid of dummy vars and IDs, dates and month
x=train_dat3[,-c(6,9,11,13,21)] # get rid of dummy vars and IDs, dates and month
x$year=NULL
x$climate_full=NULL
y=train_dat3[,6]

models_50=list()
for (ntree in c(200,400,800,1600)){
  set.seed(123)
  rf_model <- train(y=y,
                    x=x,
                    method = "ranger",
                    trControl = cv_scheme,
                    tuneGrid = param_grid,
                    num.trees=ntree,
                    max.depth=50)
  name=paste0(ntree,"_tr_model")
  models_50[[name]]=rf_model
}

models_500=list()
for (ntree in c(200,400,800,1600)){
  set.seed(123)
  rf_model <- train(y=y,
                    x=x,
                    method = "ranger",
                    trControl = cv_scheme,
                    tuneGrid = param_grid,
                    num.trees=ntree,
                    max.depth=500)
  name=paste0(ntree,"_tr_model")
  models_500[[name]]=rf_model
}

models_1000=list()
for (ntree in c(200,400,800,1600)){
  set.seed(123)
  rf_model <- train(y=y,
                    x=x,
                    method = "ranger",
                    trControl = cv_scheme,
                    tuneGrid = param_grid,
                    num.trees=ntree,
                    max.depth=1000)
  name=paste0(ntree,"_tr_model")
  models_1000[[name]]=rf_model
}



ggplot(models_50$"200_tr_model")+ggtitle("Random forest model tunning results Ntrees=200")
ggplot(models_50$"400_tr_model")+ggtitle("Random forest model tunning results Ntrees=400")
ggplot(models_50$"800_tr_model")+ggtitle("Random forest model tunning results Ntrees=800")
ggplot(models_50$"1600_tr_model")+ggtitle("Random forest model tunning results Ntrees=1600")

# find the best model parameters (replcae max node with 50, 100 and 200)
idx_min=which.min(models_1000$"1600_tr_model"$results$RMSE)
rmse_1600_1000=models_1000$"1600_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_50$"1600_tr_model"$results$RMSE)
rmse_1600_50=models_50$"1600_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_500$"1600_tr_model"$results$RMSE)
rmse_1600_500=models_500$"1600_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_1000$"800_tr_model"$results$RMSE)
rmse_800_1000=models_1000$"800_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_50$"800_tr_model"$results$RMSE)
rmse_800_50=models_50$"800_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_500$"800_tr_model"$results$RMSE)
rmse_800_500=models_500$"800_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_1000$"400_tr_model"$results$RMSE)
rmse_400_1000=models_1000$"400_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_50$"400_tr_model"$results$RMSE)
rmse_400_50=models_50$"400_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_500$"400_tr_model"$results$RMSE)
rmse_400_500=models_500$"400_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_1000$"200_tr_model"$results$RMSE)
rmse_200_1000=models_1000$"800_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_50$"200_tr_model"$results$RMSE)
rmse_200_50=models_50$"200_tr_model"$results$RMSE[idx_min]

idx_min=which.min(models_500$"200_tr_model"$results$RMSE)
rmse_200_500=models_500$"200_tr_model"$results$RMSE[idx_min]


best_rmse_idx=which.min(c(rmse_200_500,rmse_200_50,rmse_200_1000,
                          rmse_400_500,rmse_400_50,rmse_200_1000,
                          rmse_800_500,rmse_800_50,rmse_800_1000,
                          rmse_1600_50,rmse_1600_500,rmse_1600_1000))

# take the best model and find hyper parms
idx_min=which.min(models_50$"1600_tr_model"$results$RMSE)
best_mtry=models_50$"1600_tr_model"$results$mtry[idx_min]
best_node=models_50$"1600_tr_model"$results$min.node.size[idx_min]

# model based on traing data rmse_200_50 0.1591199
set.seed(123)#-crop_type_natural-climate_full
model_RF=randomForest(NDVI~.-crop_type_natural-climate_full-date-ID-month-OC+log(OC)-N+log(N)-EC+log(EC),data=na.omit(train_dat3),ntree=1600,nodesize=best_node,mtry=best_mtry,maxnodes=50,importance=F)

pred_RF_test=predict(model_RF,test_dat3)
pred_RF_train=predict(model_RF,train_dat3)

plot(c(pred_RF_test),test_dat3$NDVI)
rmse(na.omit(c(pred_RF_test)),na.omit(test_dat3$NDVI))
1-rmse(na.omit(c(pred_RF_test)),na.omit(test_dat2$NDVI))^2/var(test_dat2$NDVI)

rmse(na.omit(c(pred_RF_train)),na.omit(train_dat3$NDVI))
1-rmse(na.omit(c(pred_RF_train)),na.omit(train_dat3$NDVI))^2/var(train_dat3$NDVI)

preds_test=data.frame(NDVI_pred=predict(model_RF,newdata = test_dat3),NDVI=test_dat3$NDVI)


# Create the fancy scatter plot
p1=ggplot(preds_test, aes(x = NDVI_pred, y = NDVI)) +
  geom_point() +
  # Add labels and customize the plot
  labs(x = "Predicted NDVI", y = "Observed NDVI", title =  "Observed NDVI vs. Predicted NDVI (test dataset),model A-2")+
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


set.seed(123) #
model_RF=randomForest(NDVI~.-climate_full-crop_type_natural-year-date-ID-month-OC+log(OC)-N+log(N)-EC+log(EC),data=na.omit(dat1),ntree=1600,nodesize=best_node,mtry=best_mtry,maxnodes=50,importance=T)
pred_RF=predict(model_RF,na.omit(dat1))
rmse(na.omit(c(pred_RF)),na.omit(dat1$NDVI))
1-rmse(na.omit(c(pred_RF)),na.omit(dat1$NDVI))^2/var(dat1$NDVI)

# varible importance plot
V=varImp(model_RF,scale=TRUE)
# nicer names
rownames(V)=c("Potassium","Phosphorus", "Climate zone","Crop type","Seasonality","Soil type","CaCl2",
              "Soil temperature","Soil moisture",
              "Air temperature","Air temperature prior","Soil temperature prior","Soil moisture prior","Organic carbon","Nitrogen",
              "Electric Conductivity")

ggplot2::ggplot(V, aes(x=reorder(rownames(V),Overall), y=Overall)) +
  geom_point( color="blue", size=4, alpha=0.6)+
  geom_segment( aes(x=rownames(V), xend=rownames(V), y=0, yend=Overall), 
                color='skyblue') +
  ggtitle("Varible Importance plot (model D-2)")+
  xlab('Variable')+
  ylab('%IncMSE')+
  coord_flip() +
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# model diagnostics plots
# Predict fitted values
fitted_values <- predict(model_RF,newdata = dat)

# Calculate residuals
residuals <- dat$NDVI - fitted_values

# Add fitted values and residuals to the dataset
result1=dat
result1$fitted <- fitted_values
result1$residuals <- residuals

result1$N=log(result1$N)
result1$OC=log(result1$OC)
result1$EC=log(result1$EC)
result1$K=log(result1$K)
#result1$P=log(result1$P)

result1=result1[-which(result1$P==0.0),]
result1$P=log(result1$P)

library(ggplot2)

for (var in names(result1)) {
  if (is.factor(result1[[var]]) || is.character(result1[[var]])) {  # Check if the variable is categorical
    p <- ggplot(result1, aes_string(x = var, y = "residuals")) +
      geom_boxplot(alpha = 0.6) +  # Use boxplot for categorical predictors
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = paste("Residuals vs", var), x = var, y = "Residuals")
    print(p)
  } else {  # Handle non-categorical variables
    p <- ggplot(result1, aes_string(x = var, y = "residuals")) +
      geom_point(alpha = 0.6) +  # Use scatter plot for non-categorical predictors
     # geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dotted") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = paste("Residuals vs", var), x = var, y = "Residuals")
    print(p)
  }
}

# get predictions for the corrected NDVI
preds_all=data.frame(NDVI_pred=predict(model_RF,newdata = dat))

pred_res_all=cbind(dat,preds_all)

cor_NDVI=na.omit(dat$NDVI)-preds_all$NDVI_pred

new_result=cbind(cor_NDVI,dat)

# Create the scatter plot
p2=ggplot(pred_res_all, aes(x = NDVI_pred, y = NDVI)) +
  geom_point() +
  # Add labels and customize the plot
  labs(x = "Predicted NDVI", y = "Observed NDVI", title =  "Observed NDVI vs. Predicted NDVI (full dataset), model A-2")+
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

combined_plot <- p1 + p2 +
  plot_layout(ncol = 1, guides = "collect") +  # Stack plots in one column and collect legends
  plot_annotation(
    title = "Modelling NDVI with Random Forest model (model A-2)  ",
    tag_levels = "A",  # Automatically tag subplots as (A), (B), (C)
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  ) &
  theme(legend.position = "bottom")  # Set shared legend to bottom

print(combined_plot)

# correct the NDVI values
# find where biosubseet and corrected NDVI values matches
new_result1=new_result[which(new_result$year=="2018"),]
idx_subset=which(new_result1$ID %in% bio_subset_hc_count$ID )


bio_subset_hc_count$NDVI_cor=c(new_result1$cor_NDVI[idx_subset])

# for handling outliers when investigating wheat (skip for all crops)
clust_num="1" # change cluster number to 1, 2 and 3 when doing this (store in idx_total1, idx_total2 and idx_total3)
IQR_c2=IQR(bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$NDVI_cor,na.rm = T)
lower_lim=quantile(bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$NDVI_cor,0.25,na.rm = T)-1.5*IQR_c2
upper_lim=quantile(bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$NDVI_cor,0.75,na.rm = T)+1.5*IQR_c2

idx_upper=which(bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$NDVI_cor>upper_lim)
idx_upper_barcode=bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$barcode_id[idx_upper]

idx_lower=which(bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$NDVI_cor<lower_lim)
idx_lower_barcode=bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$barcode_id[idx_lower]

idx_total1=c(idx_lower_barcode,idx_upper_barcode)
idx_total2=c(idx_lower_barcode,idx_upper_barcode)
idx_total3=c(idx_lower_barcode,idx_upper_barcode)


idx_total=c(idx_total1,idx_total2)

idx_out=which(bio_subset_hc_count$barcode_id %in% idx_total) # apply to data when investigating wheat

#### view effects of NDVI corrections #### 

# view box plots (set as bio_subset_hc_count[-idx_out,] when investigating wheat)
ggplot(data = bio_subset_hc_count[-idx_out,], aes(x = factor(cluster), y = NDVI_cor, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "Model D-2, Residual NDVI by Cluster (p-value 0.03)",
       x = "Cluster",
       y = "NDVI",
       color = "Cluster")+
  scale_fill_manual(values = c("blue", "orange","red"),name="Cluster")+
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


res=lm(NDVI_cor~as.factor(cluster),data=bio_subset_hc_count[-idx_out,],na.action = na.exclude)#, insert when investigating wheat
anova(res)


######### Relative abudance analysis ######## 

# read in full OTU count data matrix 
library(readr)
setwd("~/Desktop/soil data analysis/bio_data") # set your wd
genus_dat=read_csv("genus_data_updated.csv")

genus_dat=genus_dat[,-1]

index_match=match(bio_subset$barcode_id,genus_dat$barcode) # match with NDVI values

Y=bio_subset$NDVI_cor
X=genus_dat[index_match,]

# remove outliers
bio_subset_hc_count=bio_subset_hc_count[-idx_out,] 


# cluster 2
idx_clust2=which(bio_subset_hc_count2$cluster==2)

idx_clust2_barcode=bio_subset_hc_count2[idx_clust2,]$barcode_id

samples_clust2_idx=which(X$barcode %in% idx_clust2_barcode )

samples_clust2=X[samples_clust2_idx,]

c2_croptype=bio_subset_hc_count2[idx_clust2,]$crop_type #add crop type as well

idx0=which(colSums(samples_clust2)==0)

samples_clust2=samples_clust2[,-idx0]


samples_clust2$barcode=NULL
dfc2=apply(samples_clust2, 1, function(x) x / sum(x))
dfc2=as.data.frame(dfc2)

dfc2=as.data.frame(t(dfc2))
dfc2$crop_type=c2_croptype


# get average genus per crop
means_per_crop <- dfc2 %>%
  group_by(crop_type) %>%
  summarize(across(starts_with("k__"), ~mean(., na.rm=TRUE)))

dfc2_subset=means_per_crop
parts <- unlist(strsplit(names(dfc2_subset[-1]), ";"))
g_element <- grep("^g__", parts, value = TRUE)
dfc2_subset=as.data.frame(dfc2_subset)

frac_abb_c2=dfc2_subset[,-1]*100
names(frac_abb_c2)=g_element

frac_mean=sort(colMeans(frac_abb_c2),decreasing = T,index.return=T)
frac_abb_c2=frac_abb_c2[frac_mean$ix]
frac_abb_c2=frac_abb_c2[,1:30]

frac_abb_c2$crop_type=dfc2_subset$crop_type
names(frac_abb_c2)=gsub("g__", "", names(frac_abb_c2))

library(dplyr)
library(tidyr)

df_long <- frac_abb_c2 %>%
  pivot_longer(cols = -crop_type, names_to = "g__", values_to = "Col_Mean")
df_long$g__ <- factor(df_long$g__, levels = names(sort(colSums(frac_abb_c2[, -31]), decreasing = F)))

df_long$crop_type=as.character(df_long$crop_type)

df_long$crop_type[which(df_long$crop_type=="B16")]="Maize"
df_long$crop_type[which(df_long$crop_type=="B11")]="Wheat"
df_long$crop_type[which(df_long$crop_type=="B13")]="Barley"

df_long$crop_type=as.factor(df_long$crop_type)


p=ggplot(df_long, aes(x = Col_Mean, y =g__, fill = crop_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Frequency", y = "Genus", fill = "Crop type") +
  ggtitle("Cluster 2 average relative abudance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("blue", "orange","red"),name="Crop type")+
  # annotate("text", x = Inf, y = Inf, label = "(B)", vjust = 1.4, hjust = 1.2)
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
library(cowplot)
p2=add_sub(p, "(B)", y  = 0, vjust = 0)
ggdraw(p2)

# cluster 1
idx_clust1=which(bio_subset_hc_count$cluster==1)

idx_clust1_barcode=bio_subset_hc_count[idx_clust1,]$barcode_id

samples_clust1_idx=which(X$barcode %in% idx_clust1_barcode )

samples_clust1=X[samples_clust1_idx,]

c1_croptype=bio_subset_hc_count[idx_clust1,]$crop_type #add crop type as well

idx0=which(colSums(samples_clust1)==0)


samples_clust1$barcode=NULL
dfc1=apply(samples_clust1, 1, function(x) x / sum(x))

dfc1=as.data.frame(dfc1)
dfc1=as.data.frame(t(dfc1))
dfc1$crop_type=c1_croptype

# get average genus per crop
means_per_crop <- dfc1 %>%
  group_by(crop_type) %>%
  summarize(across(starts_with("k__"), ~mean(., na.rm=TRUE)))

dfc1_subset=means_per_crop
parts <- unlist(strsplit(names(dfc1_subset[-1]), ";"))
g_element <- grep("^g__", parts, value = TRUE)
dfc1_subset=as.data.frame(dfc1_subset)
g_element[which(g_element=="g__Tetracladium")][1]="g__Tetracladium_35"

frac_abb=dfc1_subset[,-1]*100
names(frac_abb)=g_element
frac_mean=sort(colMeans(frac_abb),decreasing = T,index.return=T)
frac_abb=frac_abb[frac_mean$ix]
frac_abb=frac_abb[,1:30]

frac_abb$crop_type=dfc1_subset$crop_type
names(frac_abb)=gsub("g__", "", names(frac_abb))


df_long <- frac_abb %>%
  pivot_longer(cols = -crop_type, names_to = "g__", values_to = "Col_Mean")


df_long$g__ <- factor(df_long$g__, levels = names(sort(colSums(frac_abb[, -31]), decreasing = F)))

df_long$crop_type=as.character(df_long$crop_type)

df_long$crop_type[which(df_long$crop_type=="B16")]="Maize"
df_long$crop_type[which(df_long$crop_type=="B11")]="Wheat"
df_long$crop_type[which(df_long$crop_type=="B13")]="Barley"

df_long$crop_type=as.factor(df_long$crop_type)

p=ggplot(df_long, aes(x = Col_Mean, y =g__, fill = crop_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Frequency", y = "Genus", fill = "Crop type") +
  ggtitle("Cluster 1 average relative abudance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #annotate("text", x = Inf, y = Inf, label = "(A)", vjust = 1.4, hjust = 1.2)+
  scale_fill_manual(values = c("blue", "orange","red"),name="Crop type")+
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
library(cowplot)
p1=add_sub(p, "(A)", y  = 0, vjust = 0)
ggdraw(p1)

library(patchwork)
plot_grid(p1, p2, ncol=1,rel_heights = c(5,5),
          rel_widths = c(5,5))

# cluster 3 (for wheat)
idx_clust3=which(bio_subset_hc_count$cluster==3)

idx_clust3_barcode=bio_subset_hc_count[idx_clust3,]$barcode_id

samples_clust3_idx=which(X$barcode %in% idx_clust3_barcode )

samples_clust3=X[samples_clust3_idx,]

c3_croptype=bio_subset_hc_count[idx_clust3,]$crop_type #add crop type as well

idx0=which(colSums(samples_clust1)==0)

samples_clust3=samples_clust3[,-idx0]

samples_clust3$barcode=NULL
dfc3=apply(samples_clust3, 1, function(x) x / sum(x))
dfc3=as.data.frame(dfc3)

dfc3=as.data.frame(t(dfc3))
dfc3$crop_type=c3_croptype

# get average genus per crop
means_per_crop <- dfc3 %>%
  group_by(crop_type) %>%
  summarize(across(starts_with("k__"), ~mean(., na.rm=TRUE)))

dfc3_subset=means_per_crop
parts <- unlist(strsplit(names(dfc3_subset[-1]), ";"))
g_element <- grep("^g__", parts, value = TRUE)
dfc3_subset=as.data.frame(dfc3_subset)
g_element[which(g_element=="g__Tetracladium")][1]="g__Tetracladium_35"

frac_abb=dfc3_subset[,-1]*100
names(frac_abb)=g_element
frac_mean=sort(colMeans(frac_abb),decreasing = T,index.return=T)
frac_abb=frac_abb[frac_mean$ix]
frac_abb=frac_abb[,1:30]

frac_abb$crop_type=dfc3_subset$crop_type
names(frac_abb)=gsub("g__", "", names(frac_abb))

df_long <- frac_abb %>%
  pivot_longer(cols = -crop_type, names_to = "g__", values_to = "Col_Mean")
df_long$g__ <- factor(df_long$g__, levels = names(sort(colSums(frac_abb[, -31]), decreasing = F)))

df_long$crop_type=as.character(df_long$crop_type)

df_long$crop_type[which(df_long$crop_type=="B16")]="Maize"
df_long$crop_type[which(df_long$crop_type=="B11")]="Wheat"
df_long$crop_type[which(df_long$crop_type=="B13")]="Barley"

df_long$crop_type=as.factor(df_long$crop_type)

p=ggplot(df_long, aes(x = Col_Mean, y =g__, fill = crop_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Frequency", y = "Genus", fill = "Crop type") +
  ggtitle("Cluster 3 average relative abudance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #annotate("text", x = Inf, y = Inf, label = "(C)", vjust = 1.4, hjust = 1.2)
  scale_fill_manual(values = c("blue", "orange","red"),name="Crop type")+
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
p3=add_sub(p, "(C)", y  = 0, vjust = 0)

plot_grid(p1, p2,p3, ncol=1,rel_heights = c(5,5),
          rel_widths = c(5,5))



# format each cluster for further analysis

# cluster 2
dfc2_t=as.data.frame(samples_clust2)
dfc2_t$barcode=NULL

idx_c2=which(bio_subset_hc_count$cluster==2)
rownames(dfc2_t)=round(bio_subset_hc_count[idx_c2,]$NDVI_cor,digits = 6)

# cluster 1
dfc1_t=as.data.frame(samples_clust1)
dfc1_t$barcode=NULL
idx_c1=which(bio_subset_hc_count$cluster==1)
rownames(dfc1_t)=round(bio_subset_hc_count[idx_c1,]$NDVI_cor,digits = 6)

# cluster 3
dfc3_t=as.data.frame(samples_clust3)
dfc3_t$barcode=NULL

idx_c3=which(bio_subset_hc_count$cluster==3)
rownames(dfc3_t)=round(bio_subset_hc_count[idx_c3,]$NDVI_cor,digits = 6)

library("factoextra")
library(vegan)
library(cooccur)
library(visNetwork)
library(igraph)


#### IQR boostrap filtering ####### 
data_matrix=dfc2_t # change data matrix, eg dfc2_t= cluster 2, dfc1_t= cluster 1, to match cluster (run this for each cluster)

library(boot)

Function = function(input, index){
  Input = input[index]
  Stat = IQR(Input,na.rm=TRUE)
  return(Stat)
}



data_matrix=decostand(data_matrix,"hellinger") # get scaled relative abudances

# running this might take some time
boot_list=list()
for (i in 1:length(data_matrix)){
  Boot = boot(data_matrix[,i], Function, R=1000)
  mean_boot=median(Boot$t)
  boot_CI=boot.ci(Boot, conf = 0.95, type = "norm")
  lower_CI=boot_CI$normal[c(2)]
  upper_CI=boot_CI$normal[c(3)]
  boot_CI=boot.ci(Boot, conf = 0.95, type = "basic")
  lower_CI_p=boot_CI$basic[c(4)]
  upper_CI_p=boot_CI$basic[c(5)]
  
  boot_list[[i]]=c(mean_boot,lower_CI,upper_CI, lower_CI_p,upper_CI_p)
}


# cluster 1 boot results (rerun to above above when switching to new cluster)
boot_matrix_clust1=do.call("rbind",boot_list)
colnames(boot_matrix_clust1)=c("mean","CI_lower","CI_upper","CI_lower_p","CI_upper_p")
rownames(boot_matrix_clust1)=colnames(dfc1_t)
boot_matrix_clust1=as.data.frame(boot_matrix_clust1)

# idenfty which CIs contain 0
zero_list=c()
for (i in 1:length(dfc1_t)){
  zero_list[i] <- 0 >= boot_matrix_clust1$CI_lower_p[i] & 0 <= boot_matrix_clust1$CI_upper_p[i]
}


idx0_c1_plus=which(zero_list=="FALSE")


boot_matrix_clust1$genus_no=seq(1:length(boot_matrix_clust1[,1]))
boot_matrix_clust1$Zero=rep(NA,length(boot_matrix_clust1[,1]))
boot_matrix_clust1$Zero[idx0_c1_plus]="No"
boot_matrix_clust1$Zero[-idx0_c1_plus]="Yes"
boot_matrix_clust1$Zero=as.factor(boot_matrix_clust1$Zero)
# plot results
p1_CI=ggplot(boot_matrix_clust1) +
  geom_point(aes(x = mean, y = genus_no, color=Zero)) +
  geom_segment(aes(y = genus_no, yend = genus_no, x = CI_lower_p, xend = CI_upper_p, 
                   color=Zero)) +
  labs(
    x = expression("IQR of relative abudance based OTU counts"),
    y = "Genus ID",
    title ="Cluster 1: 95% confidence intervals for IQR of relative abudance",
  ) +
  scale_color_manual(values = c("blue", "orange"),name="CIs overlapping 0") + 
  geom_vline(xintercept =0, color = "red") +
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
p1_CI=add_sub(p1_CI, "(A)", y  = 0, vjust = -1)

# cluster 2 boot results (rerun above boostrap runs when switching to new cluster)
boot_matrix_clust2=do.call("rbind",boot_list)
colnames(boot_matrix_clust2)=c("mean","CI_lower","CI_upper","CI_lower_p","CI_upper_p")
rownames(boot_matrix_clust2)=colnames(dfc2_t)
boot_matrix_clust2=as.data.frame(boot_matrix_clust2)

zero_list=c()
for (i in 1:length(dfc2_t)){
  zero_list[i] <- 0 >= boot_matrix_clust2$CI_lower_p[i] & 0 <= boot_matrix_clust2$CI_upper_p[i]
}

idx0_c2_plus=which(zero_list=="FALSE")

boot_matrix_clust2$genus_no=seq(1:length(boot_matrix_clust2[,1]))
#boot_matrix_clust2$genus_no=as.factor(sub(".*g__", "", rownames(boot_matrix_clust2)))
boot_matrix_clust2$Zero=rep(NA,length(boot_matrix_clust2[,1]))
boot_matrix_clust2$Zero[idx0_c2_plus]="No"
boot_matrix_clust2$Zero[-idx0_c2_plus]="Yes"
boot_matrix_clust2$Zero=as.factor(boot_matrix_clust2$Zero)
# plot results
p2_CI=ggplot(boot_matrix_clust2) +
  geom_point(aes(x = mean, y = genus_no, color=Zero)) +
  geom_segment(aes(y = genus_no, yend = genus_no, x = CI_lower_p, xend = CI_upper_p, 
                   color=Zero)) +
  labs(
    x = expression("IQR of relative abudance based OTU counts"),
    y = "Genus ID",
    title ="Cluster 2: 95% confidence intervals for IQR of relative abudance",
  ) +
  scale_color_manual(values = c("blue", "orange"),name="CIs overlapping 0") + 
  geom_vline(xintercept =0, color = "red") +
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
p2_CI=add_sub(p2_CI, "(B)", y  = 0, vjust = -1)


# cluster 3 boot results
boot_matrix_clust3=do.call("rbind",boot_list)
colnames(boot_matrix_clust3)=c("mean","CI_lower","CI_upper","CI_lower_p","CI_upper_p")
rownames(boot_matrix_clust3)=colnames(dfc3_t)
boot_matrix_clust3=as.data.frame(boot_matrix_clust3)

# idenfty which CIs contain 0
zero_list=c()
for (i in 1:length(dfc3_t)){
  zero_list[i] <- 0 >= boot_matrix_clust3$CI_lower_p[i] & 0 <= boot_matrix_clust3$CI_upper_p[i]
}


idx0_c3_plus=which(zero_list=="FALSE")


boot_matrix_clust3$genus_no=seq(1:length(boot_matrix_clust3[,1]))
boot_matrix_clust3$Zero=rep(NA,length(boot_matrix_clust3[,1]))
boot_matrix_clust3$Zero[idx0_c3_plus]="No"
boot_matrix_clust3$Zero[-idx0_c3_plus]="Yes"
boot_matrix_clust3$Zero=as.factor(boot_matrix_clust3$Zero)
# plot results
p3_CI=ggplot(boot_matrix_clust3) +
  geom_point(aes(x = mean, y = genus_no, color=Zero)) +
  geom_segment(aes(y = genus_no, yend = genus_no, x = CI_lower_p, xend = CI_upper_p, 
                   color=Zero)) +
  labs(
    x = expression("IQR of relative abudance based OTU counts"),
    y = "Genus ID",
    title ="Cluster 3: 95% confidence intervals for IQR of relative abudance",
  ) +
  scale_color_manual(values = c("blue", "orange"),name="CIs overlapping 0") + 
  geom_vline(xintercept =0, color = "red") +
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
p3_CI=add_sub(p3_CI, "(C)", y  = 0, vjust = -1)

library(cowplot)
plot_grid(p1_CI, p2_CI, ncol=1,rel_heights = c(5,5),
          rel_widths = c(5,5))


##### graphical lasso model #####
library(glasso)
library(huge)
library(nethet)
library(mvtnorm)
library(igraph)

# functions modifying the lasso cv output
cv.fold <- function(n,folds=10){
  split(sample(1:n),rep(1:folds,length=n))
}

mytrunc.method <- function(n,wi,method='linear.growth',trunc.k=5){
  p <- ncol(wi)
  if (method=='none'){
    return(list(wi=wi))
  }
  if(method=='linear.growth'){
    wi.trunc <- wi
    diag(wi.trunc) <- 0
    nonzero <- min(2*ceiling(p*ceiling(n/trunc.k)/2),sum(wi.trunc!=0))
    wi.trunc[-order(abs(wi.trunc),decreasing=TRUE)[1:nonzero]] <- 0
    diag(wi.trunc) <- diag(wi)
    return(list(wi=wi.trunc))
  }
  if(method=='sqrt.growth'){
    wi.trunc <- wi
    diag(wi.trunc) <- 0
    nonzero <- min(2*ceiling(p*sqrt(n)/2),sum(wi.trunc!=0))
    wi.trunc[-order(abs(wi.trunc),decreasing=TRUE)[1:nonzero]] <- 0
    diag(wi.trunc) <- diag(wi)
    return(list(wi=wi.trunc))
  }
}

hugepath <- function(s,rholist,penalize.diagonal=NULL,trace=NULL){
  #fit.huge <- huge(s,method = "glasso",cov.output =TRUE,verbose = FALSE)
  fit.huge <- huge(s,lambda=sort(rholist,decreasing=TRUE),method = "glasso",cov.output =TRUE,verbose = FALSE)
  wi <- sapply(fit.huge$icov,as.matrix,simplify='array')
  w <- sapply(fit.huge$cov,as.matrix,simplify='array')
  #return(list(wi=wi[,,length(fit.huge$lambda):1],w=w[,,length(fit.huge$lambda):1]))
  return(list(rholist=rholist,wi=wi[,,length(rholist):1],w=w[,,length(rholist):1]))
}


lambda.max <- function(x){
  n <- nrow(x)
  s.var <- var(x)
  diag(s.var) <- 0
  return(n*max(abs(s.var))/2)
}

make_grid <- function(lambda.min,lambda.max,nr.gridpoints,method='lambdagrid_mult'){
  eval(as.name(method))(lambda.min,lambda.max,nr.gridpoints)
}

lambdagrid_mult <- function(lambda.min,lambda.max,nr.gridpoints){
  mult.const <- (lambda.max/lambda.min)^(1/(nr.gridpoints-1))
  return(lambda.min*mult.const^((nr.gridpoints-1):0))
}

screen_cv.glasso_custom=function (x, include.mean = FALSE, folds = min(10, dim(x)[1]), 
                                  length.lambda = 20, lambdamin.ratio = ifelse(ncol(x) > nrow(x), 
                                                                               0.01, 0.001), penalize.diagonal = FALSE, trunc.method = "linear.growth", 
                                  trunc.k = 5, plot.it = FALSE, se = FALSE, use.package = "huge", 
                                  verbose = FALSE) 
{
  if (dim(x)[1] < 5) 
    stop("Sample size too small to perform cross-validation")
  gridmax <- lambda.max(x)
  gridmin <- lambdamin.ratio * gridmax
  lambda <- make_grid(gridmin, gridmax, length.lambda)[length.lambda:1]
  colnames(x) <- paste("x", 1:ncol(x), sep = "")
  all.folds <- cv.fold(nrow(x), folds)
  residmat <- matrix(NA, folds, length(lambda))
  for (cvfold in 1:folds) {
    omit <- all.folds[[cvfold]]
    s <- var(x[-omit, ])
    if (include.mean == TRUE) {
      mu <- colMeans(x[-omit, , drop = FALSE])
    }
    else {
      mu <- rep(0, ncol(x))
    }
    fit.path <- eval(as.name(paste(use.package, "path", sep = "")))(s, 
                                                                    rholist = 2 * lambda/nrow(x), penalize.diagonal = penalize.diagonal, 
                                                                    trace = 0)
    if (length(omit) == 1) {
      residmat[cvfold, ] <- -2 * apply(fit.path$w, 3, dmvnorm, 
                                       log = TRUE, mean = mu, x = x[omit, , drop = FALSE])
    }
    else {
      residmat[cvfold, ] <- apply(-2 * apply(fit.path$w, 
                                             3, dmvnorm, log = TRUE, mean = mu, x = x[omit, 
                                                                                      , drop = FALSE]), 2, sum)
    }
  }
  cv <- apply(sqrt(residmat^2), 2, mean)
  cv.error <- sqrt(apply(residmat, 2, var)/folds)
  gl.opt <- glasso(var(x), rho = 2 * lambda[which.min(cv)]/nrow(x), 
                   penalize.diagonal = penalize.diagonal)
  if (verbose) {
    cat("la.min:", gridmin, "\n")
    cat("la.max:", gridmax, "\n")
    cat("la.opt:", lambda[which.min(cv)], "\n")
  }
  w <- gl.opt$w
  wi <- gl.opt$wi
  wi[abs(wi) < 10^{
    -3
  }] <- 0
  wi <- (wi + t(wi))/2
  colnames(w) <- rownames(w) <- colnames(wi) <- rownames(wi) <- colnames(x)
  wi.trunc <- mytrunc.method(n = nrow(x), wi = wi, method = trunc.method, 
                             trunc.k = trunc.k)$wi
  if (plot.it) {
    plotCV(lambda, cv, cv.error, se = se)
  }
  list(rho.opt = 2 * lambda[which.min(cv)]/nrow(x), w = w, 
       wi = wi.trunc, wi.orig = wi, mu = colMeans(x),cv1=cv,lambda1=lambda)
}

plotCV <- function(lambda,cv,cv.error,se=TRUE,type='b',...){
  if (se==TRUE){ylim <- range(cv,cv+cv.error,cv-cv.error)}
  else {ylim <- range(cv)}
  plot(lambda,cv,type=type,ylim=ylim,...)
  if (se)
    error.bars(lambda,cv+cv.error,cv-cv.error,width=1/length(lambda))
  invisible()
}

# run the cv-based graphical lasso model for X1= cluster1, X2= cluster 2 and X3= cluster 3 (wheat)
X1=screen_cv.glasso_custom(x=dfc1_t[,c(idx0_c1_plus)],plot.it = T,se=F,penalize.diagonal = F,use.package = "huge",length.lambda=20,lambdamin.ratio = 0.001) # cluster 1

X2=screen_cv.glasso_custom(x=dfc2_t[,c(idx0_c2_plus)],plot.it = T,se=F,penalize.diagonal = F,use.package = "huge",length.lambda=20,lambdamin.ratio = 0.001) # cluster 2

X3=screen_cv.glasso_custom(x=dfc3_t[,c(idx0_c3_plus)],plot.it = T,se=F,penalize.diagonal = F,use.package = "huge",length.lambda=20,lambdamin.ratio = 0.001) # cluster 3

# plot CV-results
X1_res=as.data.frame(X1$cv1)
X1_res$lambda=X1$lambda1
names(X1_res)=c("cv","lambda")

lambda_min=X1_res$lambda[which.min(X1_res$cv)]

library(ggplot2)

# Create the ggplot object
px1=ggplot(data = X1_res, aes(x = lambda, y = cv)) +
  # Add points
  geom_point() +
  geom_line()+
  # Add labels
  labs(x = "Lambda", y = "Cross-Validation Error") +
  geom_vline(xintercept = lambda_min, linetype = "dashed", color = "red")+
  annotate("text", x = Inf, y = Inf, label = "(A)", vjust = 1, hjust = 5.5)+
  # Add title
  ggtitle("Graphical Lasso Cross-Validation Error vs. Lambda (Cluster 1 wheat)")


X2_res=as.data.frame(X2$cv1)
X2_res$lambda=X2$lambda1
names(X2_res)=c("cv","lambda")

lambda_min=X2_res$lambda[which.min(X2_res$cv)]

# Create the ggplot object
px2=ggplot(data = X2_res, aes(x = lambda, y = cv)) +
  # Add points
  geom_point() +
  geom_line()+
  # Add labels
  labs(x = "Lambda", y = "Cross-Validation Error") +
  # Add title
  geom_vline(xintercept = lambda_min, linetype = "dashed", color = "red")+
  annotate("text", x = Inf, y = Inf, label = "(B)", vjust = 1, hjust = 5.5)+
  # Add title
  ggtitle("Graphical Lasso Cross-Validation Error vs. Lambda (Cluster 2 wheat)")


X3_res=as.data.frame(X3$cv1)
X3_res$lambda=X3$lambda1
names(X3_res)=c("cv","lambda")

lambda_min=X3_res$lambda[which.min(X3_res$cv)]

# Create the ggplot object
px3=ggplot(data = X3_res, aes(x = lambda, y = cv)) +
  # Add points
  geom_point() +
  geom_line()+
  # Add labels
  labs(x = "Lambda", y = "Cross-Validation Error") +
  # Add title
  geom_vline(xintercept = lambda_min, linetype = "dashed", color = "red")+
  annotate("text", x = Inf, y = Inf, label = "(C)", vjust = 1, hjust = 5.5)+
  # Add title
  ggtitle("Graphical Lasso Cross-Validation Error vs. Lambda (Cluster 3 wheat)")

plot_grid(px1, px2,px3, ncol=1,rel_heights = c(5,5),
          rel_widths = c(5,5))

# Extract the precision matrix (inverse of the covariance matrix)
filtered_correlation=cov2cor(X1$w) # change X here for either cluster 1, 2 or 3
filtered_correlation[filtered_correlation <= 0.3 & filtered_correlation >= -0.3] <- 0
filtered_correlation[is.nan(filtered_correlation)] <- 0

rownames(filtered_correlation)=colnames(dfc1_t[,c(idx0_c1_plus)]) #
rownames(filtered_correlation)=colnames(dfc2_t[,c(idx0_c2_plus)]) 
rownames(filtered_correlation)=colnames(dfc3_t[,c(idx0_c3_plus)])


# Create an igraph graph object from the adjacency matrix
g <- graph_from_adjacency_matrix(filtered_correlation, mode = "undirected", weighted = TRUE, diag = FALSE)
E(g)$cor <- E(g)$weight
E(g)$weight <- abs(E(g)$cor)
E(g)$color <- ifelse(E(g)$cor < 0, "blue", "red")
E(g)$width <- 1.1*asin(E(g)$weight)

# Format vertices
V(g)$size <- 0.005*abs(rowSums(filtered_correlation))
V(g)$color <- "grey"
V(g)$label.color <- "black"

parts <- unlist(strsplit(rownames(filtered_correlation), ";"))
g_element <- grep("^g__", parts, value = TRUE)
rownames(filtered_correlation) <- g_element 
idx_dub=which(duplicated(rownames(filtered_correlation))==TRUE)
rownames(filtered_correlation)[idx_dub]="g__Tetracladium_439" # cluster 1
rownames(filtered_correlation)=sub(".*g__", "", rownames(filtered_correlation))

V(g)$label.cex=0.5
V(g)$label=rownames(filtered_correlation)#rownames(filtered_correlation)

# Chose the layout function
custom.layout <- function(g, ...) {
  # layout.drl(g, weights = E(g)$weight, ...) # For bigger graphs
  layout.kamada.kawai(g, weights = E(g)$weight, ...)
}

# Do the plot

library(visNetwork)
plot(g, layout = custom.layout, main="Cluster 3 sparse network plot (wheat)",rescale = TRUE,cex.main=1)
data <- toVisNetworkData(g)
net=visNetwork(nodes = data$nodes, edges = data$edges,main="Cluster 2 sparse network")%>%
  visEdges(smooth = FALSE)

#visExport(net,type="pdf")

V(g)$name=rownames(filtered_correlation)
V(g)$label.cex=0.50

# fancy interactive network plot (on zoom in)
visIgraph(g,layout = "layout_with_kk")
eigen <- eigen_centrality(g)$vector  # Eigenvector centrality
sort(eigen,decreasing = T)[1:10]

# get node influence
library(influential)
inf_cluster1=sort(influential::ivi(g),decreasing = T)
inf_cluster2=sort(influential::ivi(g),decreasing = T)
inf_cluster3=sort(influential::ivi(g),decreasing = T)


inf_cluster1[1:10]
inf_cluster2[1:10]
inf_cluster3[1:10]




