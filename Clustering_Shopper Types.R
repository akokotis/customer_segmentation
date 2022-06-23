# Load Database
#detach(package:splitstackshape, unload = TRUE)
library(nzr)
nzConnect(machine='netezza.rei.com', database='DIGITAL_RETAIL_ANALYSIS', user='glowney', password='xxxx')

#https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html


data=nzQuery('SELECT * FROM DIGITAL_RETAIL_ANALYSIS..RH_ECOM_SHOPPER_CLUSTER_DATASET where PAGE_VIEWS is not null and PAGE_VIEWS <= 500 order by RANDOM() limit 1000000')
data_limited=nzQuery('SELECT * FROM DIGITAL_RETAIL_ANALYSIS..RH_ECOM_SHOPPER_CLUSTER_DATASET where PAGE_VIEWS is not null and PAGE_VIEWS <= 200 order by RANDOM() limit 1000000')

# Disconnect
nzDisconnect()


#Change var types
for (i in 5:ncol(data_limited)){
  data_limited[,i]=as.numeric(data_limited[,i])
}


#Investigate
sample_limited=data_limited[,c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","CHECKOUT","ORDERS","DEMAND")]
sample_limited_nonscaled=data_limited[,c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","CHECKOUT","ORDERS","DEMAND")]

sample_limited=as.data.frame(scale(sample_limited))
summary(sample_limited)



sample_limited2=data_limited[,c("PDP_ENTRY_FLAG","UNPAID_CHANNEL_FLAG","PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","CHECKOUT","ORDERS","DEMAND")]
sample_limited_nonscaled2=data_limited[,c("PDP_ENTRY_FLAG","UNPAID_CHANNEL_FLAG","PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","CHECKOUT","ORDERS","DEMAND")]

sample_limited2=as.data.frame(scale(sample_limited2))

# weird=sample[which(sample$PAGE_VIEWS >1000),]
# sample$weird_flag=0
# sample$weird_flag[which(sample$PAGE_VIEWS >1000)]=1
# table(sample$weird_flag)
# 
# sample$weird_flag=as.factor(sample$weird_flag)
# color_easy = c("black", "red")[sample$weird_flag]
# plot(sample$SEARCH_DEALS, sample$SEARCH_OFF_PRICE, col=color_easy)
# abline(h=100,v=100)
# 
# hist(sample$PAGE_VIEWS)
# h = hist(sample$PAGE_VIEWS[which(sample$PAGE_VIEWS < 50)]) # or hist(x,plot=FALSE) to avoid the plot of the histogram
# h$density = h$counts/sum(h$counts)*100
# plot(h,freq=FALSE)

correlation=cor(sample_limited)
#Outlet and deals = off price
#get rid of SEARCH_DEALS, SEARCH_OUTLET

sample_uncorr=data_limited[,c("PDP_ENTRY_FLAG","UNPAID_CHANNEL_FLAG","PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","CHECKOUT","ORDERS","DEMAND")]

#check eigenvalues
#http://www2.gsu.edu/~mkteer/npdmatri.html
data_input=sample_uncorr
x<-0
min_nc <- 2
max_nc <- 10
jeu1 <- as.matrix(data_input)
numberObsBefore <- dim(jeu1)[1]
jeu <- na.omit(jeu1) # returns the object with incomplete cases removed 
nn <- numberObsAfter <- dim(jeu)[1]
pp <- dim(jeu)[2]
TT <- t(jeu)%*%jeu   
sizeEigenTT <- length(eigen(TT)$value)
eigenValues <- eigen(TT/(nn-1))$value
for (i in 1:sizeEigenTT) 
{
  if (eigenValues[i] < 0) {
    print(paste("There are only", numberObsAfter,"nonmissing observations out of a possible", numberObsBefore ,"observations."))
    stop("The TSS matrix is indefinite. There must be too many missing values. The index cannot be calculated.")
  } 
}
length(eigenValues[eigenValues<0])
eigenValues[eigenValues<0]




#Elbow Method
k.max=10
#sample=data[,c("PDP_ENTRY_FLAG","UNPAID_CHANNEL_FLAG","PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_DEALS","SEARCH_OUTLET","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","CART_VIEW","CHECKOUT","ORDERS","DEMAND")]
wss=sapply(1:k.max, 
              function(k){kmeans(sample_limited, k, iter.max = 15)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#Another Method
require("NbClust")
nb=NbClust(as.matrix(sample_uncorr), distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans", index = c("frey", "mcclain", "cindex", "sihouette", "dunn"))
#### negative eigenvalue found -> not positive definite matrix
######possible issues: vars are linearly dependent (check corr)
# Visualize the result
library(factoextra)
fviz_nbclust(nb) + theme_minimal()




#kmeans
centers=5
k_model=kmeans(sample_limited, centers=centers)

write.csv(k_model$centers,"Cluster-5_limited_v2.csv")

# Explore K-Means Centers/Behaviors
cluster_data_nonscale=cbind(sample_limited_nonscaled,cluster=k_model$cluster)

cluster_data=cbind(sample_limited,cluster=k_model$cluster)
table(cluster_data$cluster)
for(i in 1:5){
  cat("Cluster",i,"------------------------------------------------------------------------------------\n")
  print(summary(cluster_data_nonscale[which(cluster_data_nonscale$cluster==i),]))
  cat("END",i,"------------------------------------------------------------------------------------\n")
}


#FCM
install.packages("fclust")
require("fclust")
f_model=Fclust (sample, k=centers, stand=1, distance)


#Validation

#stratified sample
require("splitstackshape")
set.seed(1)
strat_sample=stratified(cluster_data, c("cluster"), 12000)



#ind_ss=sample(1:nrow(sample_limited),60000)
sample_limited_cluster=cbind(sample_limited,cluster=k_model$cluster)
table(sample_limited_cluster$cluster)
subset=sample_limited_cluster[which(sample_limited_cluster$cluster %in% c(1,2,3)),]
cluster_four=sample_limited_cluster[which(sample_limited_cluster$cluster==4),]
cluster_five=sample_limited_cluster[which(sample_limited_cluster$cluster==5),]
subset_four=cluster_four[sample(1:nrow(cluster_four),22000),]
subset_five=cluster_five[sample(1:nrow(cluster_five),22000),]

subset_full=rbind(subset, subset_four, subset_five)


require("cluster")
#sil=silhouette(k_model$cluster[ind_ss], dist(sample_limited[ind_ss,])^2)
#summary(sil)
#summary(sil)$clus.avg.widths
#https://www.rdocumentation.org/packages/cluster/versions/2.0.9/topics/silhouette
#plot(sil, col=1:5, border=NA)

sil_subset=silhouette(subset_full$cluster, dist(subset_full)^2)
summary(sil_subset)
summary(sil_subset)$clus.avg.widths

sil_subset2=silhouette(subset_full$cluster, dist(subset_full)^2)
summary(sil_subset2)
summary(sil_subset2)$clus.avg.widths


sil_strat=silhouette(strat_sample$cluster, dist(strat_sample)^2)
summary(sil_strat)
summary(sil_strat)$clus.avg.widths


require("clues")
sil_v2=get_Silhouette(y = (sample_limited), mem = k_model$cluster, as.numeric)

require("clusterCrit")
metrics=intCriteria(as.matrix(sample_limited),k_model$cluster,c("Silhouette","Calinski_Harabasz","Dunn"))

require("fpc")

