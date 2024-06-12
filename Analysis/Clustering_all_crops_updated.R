library(readr)
OTU_new <- read_delim("~/Desktop/soil data analysis/OTU.taxonomy.genus.filt.tsv", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE)

setwd("~/Desktop/soil data analysis/bio_data/OTU.ITS.filt.tsv-20231127T124608Z-001")
OTU_ITS_filt <- read_delim("OTU.ITS.filt.tsv", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)


idx_tax=which(OTU_ITS_filt$`#OTU ID` %in% OTU_new$OTU ==TRUE)
idx_OTU=which( OTU_new$OTU %in% OTU_ITS_filt$`#OTU ID` ==TRUE)


OTU_ITS_sub=OTU_ITS_filt[idx_OTU,]
OTU_ITS_sub$taxnomy=OTU_new[idx_tax,]$taxonomy

# find where OTU is less than 2
OTU_ITS_sub[,-c(1,898)][OTU_ITS_sub[,-c(1,898)]<2]=NA

# insert 0 into NA
OTU_ITS_sub[,-c(1,898)][is.na(OTU_ITS_sub[,-c(1,898)])]=0

# remove colnames that contain a
idx_col_rm=which(grepl("a", colnames(OTU_ITS_sub))==TRUE)
OTU_ITS_sub=OTU_ITS_sub[,-c(idx_col_rm)]
OTU_ITS_sub$taxnomy=OTU_new[idx_tax,]$taxonomy
OTU_ITS_sub=OTU_ITS_sub[,-1]

uni_tax=unique(OTU_ITS_sub$taxnomy)

OTU_uni_idx=list()
for (i in 1:length(uni_tax)){
  OTU_uni_idx[[i]]=which(OTU_ITS_sub$taxnomy %in% uni_tax[i] ==TRUE)
  print(i)
}

OTU_sums=list()
for (i in 1:length(uni_tax)){
  OTU_sums[[i]]=colSums(OTU_ITS_sub[OTU_uni_idx[[i]],][,-c(886)])
  print(i)
}

OTU_sums_mat=do.call("rbind",OTU_sums)
OTU_sums_mat=as.data.frame(OTU_sums_mat)
OTU_sums_mat$taxonomy=uni_tax

# remove all OTU count below 2
idx_sedis=which(grepl("Incertae_sedis", OTU_sums_mat$taxonomy))

OTU_sums_mat=OTU_sums_mat[-idx_sedis,]

# remove rows with rowsum=0
idx_rm=which(rowSums(OTU_sums_mat[,-c(886)])==0)
OTU_sums_mat_filter=OTU_sums_mat[-idx_rm,]

idx_rm=which(OTU_sums_mat$taxonomy=="k__Fungi")

OTU_sums_mat_filter=OTU_sums_mat_filter[-idx_rm,]

contains_g <- grepl("g__", OTU_sums_mat_filter$taxonomy)

OTU_sums_mat_filter=OTU_sums_mat_filter[contains_g,]


XC_new=as.data.frame(t(OTU_sums_mat_filter[,-886]))
colnames(XC_new)=OTU_sums_mat_filter$taxonomy

count_subset1=XC_new[which(tax_meta_soil_combo$LUCAS_ID %in% soil_pro_grp2$ID),] # coverage without winter

names_with_g1 <- grep("g__", colnames(XC_new), value = TRUE)

columns_with_g_count1 <- count_subset1[, colnames(XC_new) %in% names_with_g1]

idx01=which(colSums(columns_with_g_count1)==0)
columns_with_g_count1=columns_with_g_count1[,-idx01]


# only consider rows in which NDVI values are present (remeber to read in the soil_pro_grp2 data from the other script)
idx_NDVI=which(tax_meta_soil_combo$LUCAS_ID %in% soil_pro_grp2$ID) 


##### make clusters based on the the count OTU data #####

# make clusters based on the the count data, use relative abudance
library(ggrepel)
library(factoextra)
library(cluster)

df=columns_with_g_count1
rownames(df)=tax_meta_soil_combo$BARCODE_ID[idx_NDVI]

idx_rm=which(rownames(df) %in% c("5","629")) # remove outliers
#idx_rm=which(rownames(df) %in% c("740","764","65","532","656","719","348"))
#idx_rm=which(rownames(df) %in% c("719"))

df=df[-idx_rm,]

library(vegan)
df=decostand(df,"hellinger") # scale via hellinger transformation
#df=decostand(df,"total") # scale via hellinger transformation
dist_mat <- dist(df) # genus mat

# chose linkage method
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(df, method = x)$ac
}

sapply(m,ac)

clust <- agnes(dist_mat, method = "ward",diss=T)

# choose number of clusters
fviz_nbclust(df,k.max=20, FUN = hcut, method = "silhouette") # silhouette method
# save the cluster info
hc5=as.hclust(clust)
sub_grp <- cutree(hc5, k = 2)

# make data the correct format
df_t=(df)
cluster_assignment <- data.frame(Observation = 1:nrow(df_t), Cluster = sub_grp)

# plot the clusters agianst other labels 
clust.vec.5<-cutree(hc5, k=2) 

idx_sub=which(tax_meta_soil_combo[idx_NDVI,]$BARCODE_ID %in%  as.numeric(names(clust.vec.5)) )

tax_meta_sub=tax_meta_soil_combo[idx_NDVI,]
names(clust.vec.5)=tax_meta_sub[idx_sub,]$LC1_2018
names(clust.vec.5)[which(names(clust.vec.5)=="B11")]="W"
names(clust.vec.5)[which(names(clust.vec.5)=="B13")]="B"
names(clust.vec.5)[which(names(clust.vec.5)=="B16")]="M"

# plot names
fviz_cluster(list(data = dist_mat, cluster = clust.vec.5),geom="point", main = "Hierarchical Clustering") +
  geom_text_repel(aes(label = names(clust.vec.5)), size = 3,max.overlaps = 30, show.legend = F)+
  labs(x="PC1 (28.5%)", y="PC2 (4.5%)")+
  theme(legend.text = element_text(size=12),
        plot.tag.position = c(0.91,.7),
        plot.tag = element_text(hjust =0, size=10))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  theme(plot.title = element_text(size = 16))+
  scale_colour_manual(values = c("blue", "orange")) +
  scale_fill_manual(values = c("blue", "orange"))+
  theme(legend.title = element_text(color = "black",
                                    size = 14))+
  labs(tag ="W=Wheat\nB=Barley\nM=Maize")

write.csv(clust.vec.5, "count_hierchical_result_cut1_3crops_grain_new_method.csv")

PC=prcomp(dist_mat,center = T,scale. = T)
plot(PC$x)

which(PC$sdev^2>1)

PC=prcomp(dist_mat,center = T,scale. = T)
library(fastICA)
IC=fastICA(as.matrix(dist_mat),114)

cormat=cor(PC$x[,1:25],bio_subset_hc_count$NDVI_cor)
corrplot(cormat)

cormat=cor(IC$S[,1:25],bio_subset_hc_count$NDVI_cor)
corrplot(cormat)




library(pls)
#### IQR boostrap filtering ####### 
dfc1_t=df
data_matrix=df # change data matrix, eg dfc2_t= cluster 2, dfc1_t= cluster 1, to match cluster (run this for each cluster)

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

df=df[,c(idx0_c1_plus)]

df2=decostand(df,"hellinger")

library(pls)
df1=data.frame(X=I(as.matrix(df2)),Y=I(as.matrix(bio_subset_hc_count$NDVI_cor)))


model_PLS=plsr(Y~.,ncomp=40,data=df1,validation= "CV")

plot(RMSEP(model_PLS), legendpos = "topright")

model_PLS=mvrCv(Y~.,data=df1,method = "oscorespls",validation= "LOO")

plot(seq(1:106),model_PLS$validation$pred)

cumsum(explvar(model_PLS))
drop(RMSEP(model_PLS, estimate = "train", intercept = FALSE)$val)

mod <- plsr(sensory[,1] ~ chemical, data = oliveoil, validation = "LOO")
plot(RMSEP(model_PLS))

summary(model_PLS)
model_PLS$fitted.values
model_PLS$validation$pred

model_PLS$Yscores
qqnorm(model_PLS$residuals)

library(mdatools)
?pls()
