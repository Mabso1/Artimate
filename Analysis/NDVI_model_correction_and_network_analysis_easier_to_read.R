# read files with grouped climate data
library(readr)
library(dplyr)
setwd("~/Desktop/soil data analysis/climate_data_expanded_full/corrected_grouped_names") # set to your dir

air_temp_grouped_correct <- read_csv("air_temp_grouped_correct.csv") # air_temp
soil_temp_grouped_correct <- read_csv("soil_temp_grouped_correct.csv") # soil_temp
soil_soiltype_grouped_correct <- read_csv("soil_type_grouped_correct.csv") # soil_type
soil_water_grouped_correct <- read_csv("soil_water_grouped_correct.csv") # moisture measurement

# read in the NDVI values and lucas soil data connected to the values
setwd("~/Desktop/lucas_annoations") # set to your dir
dat <- read_csv("lucas_soil_ndvi_new8_postive.csv") # lucas soil data + NDVI values

# find list of missing ids from the climate data (some shape file coordinates could not be found)
idx_share=c()
for (i in 1:length(dat$Point_id)){
  tryCatch({
    idx_share[i]= which(dat$Point_id[i]==air_temp_grouped_correct$Sample_ID)
  }, error=function(e){"missing ID"}
  )
}

# remove the data points not found in the climate data
#idx_share=which(dat$Point_id %in% air_temp_grouped_correct$Sample_ID)
idx_share=na.omit(idx_share)
dat=dat[idx_share,]

#T2=which(dat$Point_id %in% ID_X)
#ss=which(dat1$Point_id[T1] %in% dat$Point_id[T2])
#dat1$Point_id[T2][-ss]
#which(air_temp_grouped_correct$Sample_ID==dat1$Point_id[T2][-ss][1])


# combine the soil properties, NDVI values and climate data
dat$soil_type=soil_soiltype_grouped_correct$soil_type
dat$soil_type=as.factor(dat$soil_type)

dat=cbind(dat,soil_temp_grouped_correct[,-1],soil_water_grouped_correct[,-1],air_temp_grouped_correct[,-1])

# choose crop types of interest (see LUCAS doucmentation for crop type naming)
dat=dat[dat$LC %in% c("B11","B13","B16"), ] # "B13", "B16" "B81","B82" ,"B31","B32","B81","B82"  B11= wheat, B13= barley, B16=maize, B31=

# assign seasons based on dates
library(lubridate)
# Convert the date strings to a Date object
dates <- as.Date(dat$date, format = "%d-%m-%y")

# add months
dat$month <- as.POSIXlt(dat$date)$mon + 1

# add seasons
dat$season <- cut(dat$month,
                  breaks = c(0, 3, 6, 9, 12),
                  labels = c("Winter", "Spring", "Summer", "Fall"),
                  include.lowest = TRUE)

# add dates in correct format
dat$dates= dates

# set crop types, seasons and months as factors
dat$LC=factor(dat$LC,levels=unique(dat$LC))
dat$season=factor(dat$season,levels=unique(dat$season))
dat$month=factor(dat$month,levels=sort(unique(dat$month)))

# add climate zone to the dataset based on lat and lon coordinates
library(kgc)
data=data.frame(dat$TH_LAT,dat$TH_LONG)
names(data)=c("Latitude","Longitude")
data <- data.frame(data, rndCoord.lat = RoundCoordinates(data$Latitude), rndCoord.lon = RoundCoordinates(data$Longitude))
dat_climate=data.frame(data,ClimateZ=LookupCZ(data))

dat$climate=dat_climate$ClimateZ # climate zones

# detach to aviod bugs
detach("package:kgc", unload = TRUE)
detach("package:plyr", unload = TRUE)

# collect relevant features to a new data frame (soil properties)
soil_pro=data.frame(
  dat$climate,
  dat$LC,
  dat$month,
  dat$Mean,
  dat$season,
  dat$pH_CaCl2,
  dat$pH_H2O,
  dat$EC,
  dat$OC,
  dat$CaCO3,
  dat$P,
  dat$N*1000, # set to same unit as the other soil nutrients
  dat$K,
  dat$soil_type,
  dat[36:47], # soil mean temp
  dat[72:83], # soil moisture
  dat[108:119], # air temperature 
  dat$Ox_Al,
  dat$Ox_Fe,
  dat$Point_id)


#soil_pro <- replace(soil_pro, soil_pro == "< LOD", NA) # set obs below limit of detection to NA

soil_pro <- replace(soil_pro, soil_pro == "< LOD", 0.0) # set obs below limit of detection to 0

names(soil_pro)=c("climate","crop_type","month","NDVI","season","CaCl2","pH2O","EC","OC","CaCO3","P","N","K","soil_type",
                  names(dat[36:47]),names(dat[72:83]),names(dat[108:119]),"Ox_Al","Ox_Fe","ID") # name the columns accoordingly

# set all numeric features as numeric
soil_pro_temp <- as.data.frame(lapply(soil_pro[,-c(1,2,3,5,14)], as.numeric))

# add back categorical features one by one
soil_pro_temp=cbind(soil_pro_temp,soil_pro$crop_type)
soil_pro_temp=cbind(soil_pro_temp,soil_pro$season)
soil_pro_temp=cbind(soil_pro_temp,soil_pro$month)
soil_pro_temp=cbind(soil_pro_temp,soil_pro$climate)
soil_pro_temp=cbind(soil_pro_temp,soil_pro$soil_type)

# name the categorical features
names(soil_pro_temp)[c(49:53)]=c("crop_type","season","month","climate","soil_type")
soil_pro=soil_pro_temp
soil_pro$dates=dat$dates

# make sure seasons and months are correctly encoded
soil_pro$month <- as.POSIXlt(soil_pro$dates)$mon + 1

soil_pro$season=NA

soil_pro[soil_pro$month %in% c(12,1,2),]$season="Winter"
soil_pro[soil_pro$month %in% c(3,4,5),]$season="Spring"
soil_pro[soil_pro$month %in% c(6,7,8),]$season="Summer"
soil_pro[soil_pro$month %in% c(9,10,11),]$season="Fall"

#soil_pro$month=as.factor(soil_pro$month)
#soil_pro$season=as.factor(soil_pro$season)

# add previous month air temperature,  soil temperature and soil moisture
soil_pro$prev_air_temp=NA
soil_pro$prev_soil_temp=NA
soil_pro$prev_soil_water=NA
for (i in 1:nrow(soil_pro)){
  month_value <- soil_pro$month[i]  # Replace 'month_column' with the actual name of your month column
  # air temperature
  previous_month_temp <- soil_pro[i, paste0("mean_t2m_2018.", sprintf("%02d", month_value))]
  soil_pro$prev_air_temp[i]=previous_month_temp
  # soil temp
  previous_month_soil_temp <- soil_pro[i, paste0("mean_soiltemp_2018.", sprintf("%02d", month_value))]
  soil_pro$prev_soil_temp[i]=previous_month_soil_temp
  # soil water content
  previous_month_soil_water <- soil_pro[i, paste0("mean_soilwater_2018.", sprintf("%02d", month_value))]
  soil_pro$prev_soil_water[i]=previous_month_soil_water
}

# check names fit within the cycle of a year
names(soil_pro[,10:21])
names(soil_pro[,22:33])
names(soil_pro[,34:45])

# get the yearly average for the soil moisure, soil temp and air temp
soil_pro$soil_temp_all=rowMeans(soil_pro[,10:21], na.rm = TRUE)
soil_pro$soil_water_all=rowMeans(soil_pro[,22:33], na.rm = TRUE)
soil_pro$air_temp_all=rowMeans(soil_pro[,34:45], na.rm = TRUE)

# collect the relevant features
soil_pro_grp1=cbind(soil_pro[,c(1:9)],
                    soil_pro$soil_water_all,
                    soil_pro$soil_temp_all,
                    soil_pro$air_temp_all,
                    soil_pro$prev_air_temp,
                    soil_pro$prev_soil_temp,
                    soil_pro$prev_soil_water,
                    soil_pro$climate,
                    soil_pro$crop_type,
                    soil_pro$month,
                    soil_pro$dates,
                    soil_pro$season,
                    soil_pro$soil_type,
                    soil_pro$Ox_Al,
                    soil_pro$Ox_Fe,
                    soil_pro$ID)
# set correct names
names(soil_pro_grp1)=c(names(soil_pro[,c(1:9)]),"soil_water_all","soil_temp_all","air_temp_all","prev_air_temp","prev_soil_temp",
                       "prev_soil_water","climate","crop_type","month","dates","season","soil_type","Ox_Al","Ox_Fe","ID")

#### small exploratory analysis ######
library(ggplot2)
library(dplyr)
library(corrplot)

# remove all points from october and prior to may
soil_pro_grp2=soil_pro_grp1[soil_pro_grp1$month %in% c("5","6","7","8","9"), ] # losing all winter months - remove 8 and 9 too

# remove points with no climate zone infromation
rm_idx=which(soil_pro_grp2$climate=="Climate Zone info missing")
soil_pro_grp2=soil_pro_grp2[-rm_idx,]

# plot correlation of numerical features
correlation_matrix <- cor((soil_pro_grp2[,-c(16,17,18,19,20,21,24)]),use = "complete.obs",method="pearson")
corrplot(correlation_matrix)

# change B11, B13 and B16 to wheat barley and maize
crop_type_natural=rep(NA,length(soil_pro_grp2$crop_type))

B11_idx=which(soil_pro_grp2$crop_type=="B11")
B13_idx=which(soil_pro_grp2$crop_type=="B13")
B16_idx=which(soil_pro_grp2$crop_type=="B16")

crop_type_natural[B11_idx]=rep("Wheat",length(B11_idx))
crop_type_natural[B13_idx]=rep("Barley",length(B13_idx))
crop_type_natural[B16_idx]=rep("Maize",length(B16_idx))

soil_pro_grp2$crop_type_natural=crop_type_natural

table(soil_pro_grp2$climate)

#rm_idx1=which(soil_pro_grp2$climate=="Climate Zone info missing")
rm_idx1=which(soil_pro_grp2$climate=="BSh")
rm_idx2=which(soil_pro_grp2$climate=="ET")
#rm_idx4=which(soil_pro_grp2$climate=="Cfc")

soil_pro_grp2=soil_pro_grp2[-c(rm_idx1,rm_idx2),]

soil_pro_grp2$climate=as.factor(as.character(soil_pro_grp2$climate))

# encode climate zone dummy variable
# Create a named vector for mapping
idx_BSk=which(soil_pro_grp2$climate=="BSk")
idx_Csa=which(soil_pro_grp2$climate=="Csa")
idx_Csb=which(soil_pro_grp2$climate=="Csb")
idx_Cfa=which(soil_pro_grp2$climate=="Cfa")
idx_Cfb=which(soil_pro_grp2$climate=="Cfb")
idx_Dfb=which(soil_pro_grp2$climate=="Dfb")
idx_Dfc=which(soil_pro_grp2$climate=="Dfc")

soil_pro_grp2$climate_full=rep(NA,length(soil_pro_grp2$climate))
soil_pro_grp2$climate_full[idx_BSk]="Bsk (Cold semi-arid climate)"
soil_pro_grp2$climate_full[idx_Csa]="Csa (Hot-summer Mediterranean climate)"
soil_pro_grp2$climate_full[idx_Csb]="Csb (Warm-summer Mediterranean climate)"
soil_pro_grp2$climate_full[idx_Cfa]="Cfa (Humid subtropical climate)"
soil_pro_grp2$climate_full[idx_Cfb]="Cfb (Temperate oceanic climate)"
soil_pro_grp2$climate_full[idx_Dfb]="Dfb (Warm-summer humid continental climate)"
soil_pro_grp2$climate_full[idx_Dfc]="Dfc (Subarctic climate)"

soil_pro_grp2$climate_full=as.factor(soil_pro_grp2$climate_full)

# plot NDVI vs time sepeately for each crop
ggplot(soil_pro_grp2, aes(x = dates, y = NDVI, color = climate_full)) +
  geom_point() +  # Scatter points
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +  # Add single trend line across all crops
  labs(title = "Time Series: NDVI for Different Crop Types", 
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

# plot NDVI vs time for all crops
ggplot(soil_pro_grp2, aes(x = dates)) +
  geom_point(aes(y = NDVI)) +
  labs(title = "Time series: NDVI measured for all crops", y = "NDVI") +
  theme_minimal()

# plot NDVI vs time sepeately for each crop
ggplot(soil_pro_grp2, aes(x = dates, y = NDVI, color = climate)) +
  geom_point() +
  labs(title = "Time Series: NDVI for Different Crop Types", y = "NDVI", color = "Climate zones") +
  labs(x = "Dates", y = "NDVI")+
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  theme(legend.text = element_text(size=12))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  facet_grid(crop_type_natural ~ ., scales = "free_y", switch = "y")+
  theme(strip.text.y = element_text(size = 12, colour = "black", angle = 90))


# climate vs NDVI
ggplot(soil_pro_grp2, aes(x = as.factor(climate), y = NDVI)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of NDVI by Climate", x = "Climate", y = "NDVI") +
  theme_minimal()

# crop type vs NDVI
ggplot(soil_pro_grp2, aes(x = as.factor(crop_type), y = NDVI)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of NDVI by Crop type", x = "Crop_type", y = "NDVI") +
  theme_minimal()

# crop vs month
ggplot(soil_pro_grp2, aes(x = as.factor(season), y = NDVI, fill = crop_type)) +
  geom_boxplot() +
  labs(title = "Boxplot of NDVI by month and crop type", x = "Season", y = "NDVI") +
  theme_minimal() +
  facet_wrap(~ as.factor(crop_type), scales = "free_y")

# climate vs crop type
ggplot(soil_pro_grp2, aes(x = climate, y = NDVI, fill = crop_type)) +
  geom_boxplot() +
  labs(title = "Boxplot of NDVI by climate and crop type", x = "Climate", y = "NDVI") +
  theme_minimal() +
  facet_wrap(~ as.factor(crop_type), scales = "free_y")

#### view how the NDVI values are clustering when not correcting for any variables ####
# read in the soil microbiome data (binary matrix)
tax_meta_soil_combo <- read_csv("~/Desktop/soil data analysis/meta_soil.csv")

# subset the matrix to the targeted crops
crop_type_idx=which(tax_meta_soil_combo$LC1_2018 %in% c("B11","B13","B16"))#,)) # "B11","B13", "B16","B31","B32","B81","B82"
tax_meta_soil_combo=tax_meta_soil_combo[crop_type_idx,]

# find the missing points
length(which(tax_meta_soil_combo$LUCAS_ID %in% dat$Point_id)) # from 237 down to 190 - 
length(which(tax_meta_soil_combo$LUCAS_ID %in% soil_pro_grp2$ID)) # from 190 down to 175 (samples lost in winter months) down to 96 without atum


# set the taxon data to only include sample from which the NDVI values are availability 
idx_NDVI=which(tax_meta_soil_combo$LUCAS_ID %in% soil_pro_grp2$ID)
idx_barcode=which(soil_pro_grp2$ID %in%  tax_meta_soil_combo$LUCAS_ID)

dat=soil_pro_grp2
# subset of bio-data where NDVI values are present 
bio_subset=dat[idx_barcode,]
bio_subset$barcode_id=tax_meta_soil_combo$BARCODE_ID[idx_NDVI]

#### make NDVI correction model ####

# linear modelling
library(car)
# remove variables that contain  many NA values
soil_pro_grp2$CaCO3=NULL
soil_pro_grp2$Ox_Al=NULL
soil_pro_grp2$Ox_Fe=NULL
soil_pro_grp2$pH2O=NULL

# remove all nan values
result=na.omit(soil_pro_grp2) # remove nan

# remove high outliers
rm_idx=which(result$EC>500)
result=result[-rm_idx,] 

# how many biosamples left
length(which(bio_subset$ID%in% result$ID)) # from 175 down to 143 

# split data into train and test
library(caret)
library(Metrics)

set.seed(15)
trainIndex <- createDataPartition(result$NDVI, p = 0.8, list = FALSE)

# Create training and testing datasets
train_dat <- result[trainIndex, ]
test_dat <- result[-trainIndex, ]


# make simple model 
model_lm <- lm(NDVI~.-crop_type_natural-climate_full-ID-dates-month+season-OC-N+log(N)-EC+log(EC)-soil_water_all-soil_temp_all-air_temp_all-prev_air_temp  , data = train_dat)
#crPlots(model_lm)
#plot(model_lm)
summary(model_lm)
# view model status
Anova(model_lm,type="III")


# expand the model
model_lm=update(model_lm,~(.)^2,data=train_dat)
summary(model_lm)

model_lm2=update(model_lm,~.-climate:log(N)-crop_type:log(N)-prev_soil_water:log(EC)-CaCl2:prev_soil_water-soil_type:log(EC)-soil_type:log(N)-CaCl2:log(N)-log(N):log(EC)-climate:season-K:crop_type-P:log(N)-K:log(N)-prev_soil_temp:climate-season:log(EC)-P:crop_type-prev_soil_water:crop_type-CaCl2:climate-prev_soil_water:climate-P:season-P:K-K:season -CaCl2:season-prev_soil_temp:log(EC)-K:log(EC)-CaCl2:P-prev_soil_water:log(N)-K:soil_type-CaCl2:log(EC)-prev_soil_water:soil_type-K:prev_soil_water-prev_soil_water:season-K:prev_soil_temp-CaCl2:K-prev_soil_temp:soil_type-P:prev_soil_water-season:log(N)-P:soil_type-CaCl2:soil_type-P:log(EC)-prev_soil_temp:log(N),data=train_dat)

# remove terms 
ss=drop1(model_lm2,test = "Chi")
var_to_remove <- rownames(ss)[which.max(ss$`Pr(>Chi)`)]
print(var_to_remove)
print(ss)

anova(model_lm2)
par(mfrow=c(1,2))
plot(model_lm,which = c(1,2))
mtext("Model Diagnostics", side = 3, line = - 2, outer = TRUE)

# test set preds
lm_pred_test=predict(model_lm2,test_dat)
rmse(lm_pred_test,test_dat$NDVI)

# extract MSE and R^2
summary(model_lm2)


# RF model
library(randomForest)
library(ranger)

param_grid=expand.grid(mtry = c(1:10),
                       splitrule = c( "variance"),
                       min.node.size = c(1, 5, 10, 15))
cv_scheme <- trainControl(method = "cv",
                          number = 5,
                          verboseIter = FALSE)
x=train_dat[,-1]
y=train_dat[,1]

x$ID=NULL
x$dates=NULL
x$month=NULL

models=list()
for (ntree in c(10,100,200,400,800)){
  set.seed(15)
  rf_model <- train(y=y,
                    x=x,
                    method = "ranger",
                    trControl = cv_scheme,
                    tuneGrid = param_grid,
                    num.trees=ntree)
  name=paste0(ntree,"_tr_model")
  models[[name]]=rf_model
}

models$"10_tr_model"$bestTune
models$"100_tr_model"$bestTune
models$"200_tr_model"$bestTune
models$"400_tr_model"$bestTune
models$"800_tr_model"$bestTune

models$"10_tr_model"$results$RMSE
models$"10_tr_model"$results$mtry
models$"10_tr_model"$results$min.node.size

ggplot(models$"10_tr_model")+ggtitle("Random forest model tunning results Ntrees=10")
ggplot(models$"100_tr_model")+ggtitle("Random forest model tunning results Ntrees=100")
ggplot(models$"200_tr_model")+ggtitle("Random forest model tunning results Ntrees=200")
ggplot(models$"400_tr_model")+ggtitle("Random forest model tunning results Ntrees=400")
ggplot(models$"800_tr_model")+ggtitle("Random forest model tunning results Ntrees=800")

# insert best model here 
model_RF=randomForest(NDVI~.-climate_full-crop_type_natural-dates-ID+season-month,data=train_dat,ntree=800,nodesize=15,mtry=9,importance=TRUE)
RF_test_pred=predict(model_RF,test_dat)

rmse(RF_test_pred,test_dat$NDVI) # test data eval
1-rmse(RF_test_pred,test_dat$NDVI)^2/var(test_dat$NDVI)

# RF model with all data
set.seed(15)
model_lm2=randomForest(NDVI~.-climate_full-crop_type_natural-dates-ID+season-month,data=result,ntree=800,nodesize=15,mtry=9,importance=TRUE)

# varible importance plot
V=varImp(model_lm2,scale=TRUE)
ggplot2::ggplot(V, aes(x=reorder(rownames(V),Overall), y=Overall)) +
  geom_point( color="blue", size=4, alpha=0.6)+
  geom_segment( aes(x=rownames(V), xend=rownames(V), y=0, yend=Overall), 
                color='skyblue') +
  ggtitle("Varible Importance plot")+
  xlab('Variable')+
  ylab('%IncMSE')+
  coord_flip() +
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# extract RF model R^2 and RMSE - results may differ from a little from time to time (RF is stochastic)
model_lm2 # find R^2 here
rmse(predict(model_lm2,result),result$NDVI) # rmse full data


# get nice residual plots
# Predict fitted values
fitted_values <- predict(model_lm2,newdata = result)

# Calculate residuals
residuals <- result$NDVI - fitted_values

# Add fitted values and residuals to the dataset
result1=result
result1$fitted <- fitted_values
result1$residuals <- residuals


library(ggplot2)
for (var in names(result1)) {
  if (is.factor(result1[[var]]) || is.character(result1[[var]])) {  # Check if the variable is categorical
    p <- ggplot(result1, aes_string(x = var, y = "residuals")) +
      geom_boxplot(alpha = 0.6) +  # Use boxplot for categorical predictors
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = paste("Residuals vs", var), x = var, y = "Residuals")
    print(p)
    #pp[[var]] <- p  # Store the plot in the list
  }
}

# remove influence of non-mcrio biome predictors
preds_all=data.frame(NDVI_pred=predict(model_lm2,newdata = result))

pred_res_all=cbind(result,preds_all)

cor_NDVI=na.omit(result$NDVI)-preds_all$NDVI_pred

new_result=cbind(cor_NDVI,result)

# Create the scatter plot
ggplot(pred_res_all, aes(x = NDVI_pred, y = NDVI)) +
  geom_point() +
  # Add labels and customize the plot
  labs(x = "Predicted NDVI", y = "Observed NDVI", title = "Observed NDVI vs. Predicted NDVI")+
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

##### link the corrected NDVI to the OTU clusters #### 
cluster_relation_count_Hclust <- read_csv("~/Desktop/soil data analysis/bio_data/count_hierchical_result_cut1_3crops_grain_new_method.csv") # 3 crops

bar_code_temp=cluster_relation_count_Hclust$...1
cluster_temp=cluster_relation_count_Hclust $x

idx_match=which(bar_code_temp %in% bio_subset$barcode_id)

cluster_hc=cluster_relation_count_Hclust[idx_match,]

bio_subset_hc_count=bio_subset[idx_match,]

bio_subset_hc_count$cluster=cluster_relation_count_Hclust$x

# non-corrected data
boxplot(NDVI ~ cluster, data = bio_subset_hc_count, main = "Boxplot of uncorrected NDVI by Cluster", xlab = "Cluster", ylab = "uncorrected NDVI")
summary(aov(NDVI~as.factor(cluster),data=bio_subset_hc_count))

# find where biosubseet and corrected NDVI values matches
idx_subset=which(new_result$ID %in% bio_subset_hc_count$ID )

bio_subset_hc_count$NDVI_cor=new_result$cor_NDVI[idx_subset]

#ID_X=bio_subset_hc_count$ID

# for handling outliers when investigating wheat (skip for all crops)
clust_num="2" # change cluster number to 1, 2 and 3 when doing this (store in idx_total1, idx_total2 and idx_total3)
IQR_c2=IQR(bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$NDVI_cor)
lower_lim=quantile(bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$NDVI_cor,0.25)-1.5*IQR_c2
upper_lim=quantile(bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$NDVI_cor,0.75)+1.5*IQR_c2

idx_upper=which(bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$NDVI_cor>upper_lim)
idx_upper_barcode=bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$barcode_id[idx_upper]

idx_lower=which(bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$NDVI_cor<lower_lim)
idx_lower_barcode=bio_subset_hc_count[which(bio_subset_hc_count$cluster==clust_num),]$barcode_id[idx_lower]

idx_total1=c(idx_lower_barcode,idx_upper_barcode)
idx_total2=c(idx_lower_barcode,idx_upper_barcode)
idx_total3=c(idx_lower_barcode,idx_upper_barcode)


idx_total=c(idx_total1,idx_total2)#,idx_total3)

idx_out=which(bio_subset_hc_count$barcode_id %in% idx_total) # apply to data when investigating wheat

#### view effects of NDVI corrections #### 
# view box plots (set as bio_subset_hc_count[-idx_out,] when investigating wheat)
ggplot(data = bio_subset_hc_count[-idx_out,], aes(x = factor(cluster), y = NDVI_cor, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "Boxplot of corrected NDVI by Cluster (wheat)",
       x = "Cluster",
       y = "Corrected NDVI",
       color = "Cluster")+
  scale_fill_manual(values = c("blue", "orange","red"),name="Cluster (wheat)")+
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

res=lm(NDVI_cor~as.factor(cluster),data=bio_subset_hc_count[-idx_out,]) #[-idx_out,], insert when investigating wheat
anova(res)


######### Relative abudance analysis ######## 

# read in full OTU count data matrix 
library(readr)
setwd("~/Desktop/soil data analysis/bio_data") # set your wd
#genus_dat=read_csv("genus_dat_binary.csv")
genus_dat=read_csv("genus_dat_count.csv")
genus_dat=read_csv("genus_data_updated.csv")

genus_dat=genus_dat[,-1]

index_match=match(bio_subset$barcode_id,genus_dat$barcode) # match with NDVI values

Y=bio_subset$NDVI_cor
X=genus_dat[index_match,]

# remove outliers
bio_subset_hc_count=bio_subset_hc_count#[-idx_out,] 

# cluster 2
idx_clust2=which(bio_subset_hc_count$cluster==2)

idx_clust2_barcode=bio_subset_hc_count[idx_clust2,]$barcode_id

samples_clust2_idx=which(X$barcode %in% idx_clust2_barcode )

samples_clust2=X[samples_clust2_idx,]

c2_croptype=bio_subset_hc_count[idx_clust2,]$crop_type #add crop type as well

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



