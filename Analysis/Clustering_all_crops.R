library(readr)
# note run the other script until line 276 to make this script work 

setwd("~/Desktop/soil data analysis/bio_data")
OTU_int=read.csv("OTU_names_process_new.csv")
OTU_num=read.csv("OTU_counts_process_new.csv")

# remove incedies enteries 
tax_names=unique(OTU_int$tax_id)

idx_sedis=which(grepl("Incertae_sedis", OTU_int$tax_id))

OTU_num=OTU_num[-idx_sedis,]
OTU_int=OTU_int[-idx_sedis,]

# OTU 
# find where OTU>2
OTU_num[,-c(1,898)][OTU_num[,-c(1,898)]<2]=NA

# insert 0 into NA
OTU_num[,-c(1,898)][is.na(OTU_num[,-c(1,898)])]=0

# remove rows with rowsum=0
idx_rm=which(rowSums(OTU_num[,-c(1,898)])==0)
OTU_num=OTU_num[-idx_rm,]

# make count OTU matrix 

# collapse non-unique rows
unique_ids=unique(OTU_num$tax_id)
ITS_names=colnames(OTU_num)

result_mat_num=matrix(NA,nrow=length(unique_ids),ncol=length(ITS_names))
result_mat_num=as.data.frame(result_mat_num)
colnames(result_mat_num)=ITS_names


# colsum of all counted ITUs in each sample 
for (i in 1:length(unique_ids)){
  tax_id=which(OTU_num$tax_id==unique_ids[i])
  result_mat_num[i,-c(1,898)]=colSums(OTU_num[tax_id,-c(1,898)],na.rm = T)
}

result_mat_num$tax_id=unique_ids
result_mat_num=result_mat_num[,-1]

df=t(result_mat_num)
df=as.data.frame(df)
colnames(df)=result_mat_num$tax_id

# make binary matrix of OTUs

# collapse non-unique rows
XX=result_mat_num[,-897]

XX[XX != 0]=1

XX$tax_id=result_mat_num$tax_id

# filter out ITS (retain 1-885)

# binary matrix
idx=which(grepl("a", colnames(XX)))
XX=XX[,-idx]

rownames(XX)=result_mat_num$tax_id
XX=as.data.frame(t(XX))

# OTU counts
idx=which(grepl("a", colnames(t(df))))
df=t(df)[,-idx]
df=as.data.frame(t(df))
XC=df

# figure out how many samples that has satelite coverage (see other scipt) - to ge the tax_meta_soil_combo dataset, run the other script 
count_subset=XC[which(tax_meta_soil_combo$LUCAS_ID %in% soil_pro_grp2$ID),] # coverage without winter
binary_subset=XX[which(tax_meta_soil_combo$LUCAS_ID %in% soil_pro_grp2$ID),] 

#### check how these are clustering #####
tax_names1=colnames(binary_subset)
#tax_names2=colnames(count_subset)
length(tax_names1)

# go to genus level 
names_with_g <- grep("g__", tax_names1, value = TRUE)

#crop_idx=which(tax_meta_soil_combo$LC_simpl_2018=="Cropland")
columns_with_g_binary <- binary_subset[, tax_names1 %in% names_with_g]
columns_with_g_count <- count_subset[, tax_names1 %in% names_with_g]

library(ggrepel)
library(factoextra)
library(cluster)

# remove 0 columns
idx0=which(colSums(columns_with_g_binary)==0)
columns_with_g_binary=columns_with_g_binary[,-idx0]

# make sure columns in count are numeric
columns_with_g_count[]=lapply(columns_with_g_count, as.numeric)

idx0=which(colSums(columns_with_g_count)==0)
columns_with_g_count=columns_with_g_count[,-idx0]

# only consider rows in which NDVI values are present (remeber to read in the soil_pro_grp2 data from the other script)
idx_NDVI=which(tax_meta_soil_combo$LUCAS_ID %in% soil_pro_grp2$ID) 


##### make clusters based on the the count OTU data #####

# make clusters based on the the count data, use relative abudance
df=columns_with_g_count
rownames(df)=tax_meta_soil_combo$BARCODE_ID[idx_NDVI]

idx_rm=which(rownames(df) %in% c("5","629")) # remove outliers
df=df[-idx_rm,]

library(vegan)
df=decostand(df,"hellinger") # scale via hellinger transformation
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
  labs(x="PC1 (34%)", y="PC2 (10.3%)")+
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

write.csv(clust.vec.5, "count_hierchical_result_cut1_3crops_grain.csv")




