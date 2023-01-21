library(nzr)
nzConnect(machine='', database='', user='', password='xxxx')
original=nzQuery('SELECT * FROM ECOM_SHOPPER_CLUSTER_DATASET where PAGE_VIEWS is not null and PAGE_VIEWS <= 200 and ORDERS in (0,1) and CHECKOUT < 20 order by RANDOM()')
#Change var types
for (i in 5:ncol(original)){original[,i]=as.numeric(original[,i])}
summary(original[,5:ncol(original)])
# Disconnect
nzDisconnect()

#Add Device var that is numeric
# 1 - Desktop
# 2 - Mobile Web
# 3 - App
original$device_num[which(original$DEVICE_TYPE=="Desktop")]=1
original$device_num[which(original$DEVICE_TYPE=="Mobile Web")]=2
original$device_num[which(original$DEVICE_TYPE=="Mobile App")]=3
save(original, file = "cluster_data_full.RData")

#Sample
size=400
ind_ss=sample(1:nrow(data),size)


#Choose number of clusters
cluster_number=function(data=sample_limited,
                        id=1,
                         vars=c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","CHECKOUT","ORDERS","DEMAND"),
                         set.seed=56,
                        scale=T)
{
  require("cluster")
  require("fpc")
  require("vegan")
  require("mclust")
  
  data=data[,vars]
  
  output=list()
  output$datasetID=id
  output$vars=colnames(data)
  output$cutoff=paste0("Max pageviews are cut off at: ", max(data$PAGE_VIEWS))
  
  #Scale data
  if (scale==T) {
    data=as.data.frame(scale(data))
    output$scaled="T"
  } else{output$scaled="F"}
  
  #Elbow Method
  wss=sapply(1:10, 
                 function(k){kmeans(data, k, iter.max = 15)$tot.withinss})
  plot(1:10, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main="output$elbow$plot",sub=paste0("DatasetID: ",id))
  output$elbow=list(results=wss, plot="output$elbow$plot")

  #PAM and Silhouette
  #pamk.best=pamk(data)
  #plot(pam(data, pamk.best$nc),main="output$pam$plot",sub=paste0("DatasetID: ",id))
  #output$pam=list(results=paste0("Optimum Centers: ",pamk.best$nc), plot="output$pam$plot")
  
  #Gap Statistic
  #gapstat=clusGap(data, FUN =kmeans, K.max =10, B = 5, verbose = interactive())
  #plot(gapstat, main="output$gap$plot",sub=paste0("DatasetID: ",id))
  #output$gap=list(results=gapstat, plot="output$gap$plot")
  
  #Hierarchical Clustering
  #data_dist=dist(as.matrix(data))   # find distance matrix
  #plot(hclust(data_dist),main="output$hier$plot",sub=paste0("DatasetID: ",id))
  #output$hier=list(plot="output$hier$plot")
  
  #CALINSKY CRITERION
  cal_fit=cascadeKM(data, 1, 10, iter = 200)
  output$calinsky=paste0("Optimum Centers: ",as.numeric(which.max(cal_fit$results[2,])))
  
  #Model based clustering
  d_clust=Mclust(as.matrix(data))
  output$mclust=list(results=paste0("Optimum Centers: ",dim(d_clust$z)[2]), summary=summary(d_clust))
  
  return(output)
  
}


#####################################
########## Run models ###############
#####################################

#Sample
size=60000
ind_ss=sample(1:nrow(original),size)

eval1=cluster_number(data=original[ind_ss,c("device_num", "PDP_ENTRY_FLAG", "UNPAID_CHANNEL_FLAG","PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND")],
                        id=1,
                        vars=c("device_num", "PDP_ENTRY_FLAG", "UNPAID_CHANNEL_FLAG","PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND"),
                        set.seed=56,
                        scale=T)
#5
#6
#2
#4

eval2=cluster_number(data=original[ind_ss,c("device_num", "PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND")],
                     id=2,
                     vars=c("device_num", "PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND"),
                     set.seed=56,
                     scale=T)

#2-3
#2
#7

eval3=cluster_number(data=original[ind_ss,c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW")],
                     id=3,
                     vars=c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW"),
                     set.seed=56,
                     scale=F)

#3
#3
#8


eval3_v2=cluster_number(data=original[ind_ss,c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW")],
                     id="3_v2",
                     vars=c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW"),
                     set.seed=56,
                     scale=T)





eval4_full=cluster_number(data=original[,c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND")],
                     id="4_full",
                     vars=c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND"),
                     set.seed=56,
                     scale=T)
#3
#2
#2

###############################
########## Clustering #########
###############################
seg.mean=function(data,groups){
  aggregate(data,list(groups), function(x) mean(as.numeric(x)))
}

cluster_process=function(data=sample_limited,
                        id=1,
                        vars=c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","CHECKOUT","ORDERS","DEMAND"),
                        centers=3,
                        set.seed=56,
                        scale=T,
                        sample_size=25000){
  require("cluster")

  data=data[,vars]
  orig=data
  
  output=list()
  output$datasetID=id
  output$centers=centers
  output$vars=colnames(data)

  #Scale data
  if (scale==T) {
    data=as.data.frame(scale(data))
    output$scaled="T"
  } else{output$scaled="F"}
  
  #Kmeans
  k_model=kmeans(data,centers=centers)
  
  #Cluster Summaries
  cluster_means=seg.mean(orig,k_model$cluster)
  
  output$cluster=k_model$cluster
  output$cluster_summary=cluster_means
  
  #plot cluster
  size=sample_size
  ind_ss=sample(1:nrow(data),size)
  clusplot(orig[ind_ss,],k_model$cluster[ind_ss],color=T,shade=T, col.p=k_model$cluster[ind_ss], main=paste0("Data: ",id))
  
  return(output)

}


eval1_model=cluster_process(data=original[,c("device_num", "PDP_ENTRY_FLAG", "UNPAID_CHANNEL_FLAG","PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND")],
                id=1,
                vars=c("device_num", "PDP_ENTRY_FLAG", "UNPAID_CHANNEL_FLAG","PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND"),
                centers=5,
                         set.seed=56,
                         scale=T,
                         sample_size=25000)

eval2_model=cluster_process(data=original[,c("device_num", "PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND")],
                     id=2,
                     vars=c("device_num", "PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND"),
                     centers=3,
                     set.seed=56,
                     scale=T,
                     sample_size=25000)



eval4_model=cluster_process(data=original[,c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND")],
                            id=4,
                            vars=c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND"),
                            centers=4,
                           set.seed=56,
                           scale=T,
                           sample_size=25000)

eval4_model_v2=cluster_process(data=original[,c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND")],
                            id="4_v2",
                            vars=c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND"),
                            centers=3,
                            set.seed=56,
                            scale=T,
                            sample_size=25000)

eval2_model_v2=cluster_process(data=original[,c("device_num", "PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND")],
                            id="2_v2",
                            vars=c("device_num", "PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND"),
                            centers=2,
                            set.seed=56,
                            scale=T,
                            sample_size=25000)


eval2_model_v3=cluster_process(data=original[,c("device_num", "PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND")],
                               id="2_v3",
                               vars=c("device_num", "PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND"),
                               centers=5,
                               set.seed=56,
                               scale=T,
                               sample_size=25000)

eval2_model_v4=cluster_process(data=original[,c("device_num", "PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND")],
                            id="2_v4",
                            vars=c("device_num", "PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW","DEMAND"),
                            centers=7,
                            set.seed=56,
                            scale=T,
                            sample_size=25000)


eval3_model=cluster_process(data=original[,c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW")],
                            id=3,
                            vars=c("PAGE_VIEWS","SEARCH_BROWSE","SEARCH_KEYWORD","SEARCH_OFF_PRICE","PRODUCT_CORE","PRODUCT_OUTLET","ADD_TO_CART","REMOVE_FROM_CART","CART_VIEW"),
                            centers=6,
                            set.seed=56,
                            scale=T,
                            sample_size=25000)
  
  
  


#Look into:
eval4_model
eval4_model_v2
eval3_model

# Compare model labels
table(eval4_model$cluster,eval2_model_v4$cluster)
table(eval3_model$cluster,eval2_model_v4$cluster)
