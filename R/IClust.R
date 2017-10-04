#' IClust (Imblanaced Clustering)
#'
#' @description 
#' \code{IClust} is the function which discovers clusters that are of highly different (imbalanced) sizes. 
#' First, the initial clusters are found by using an existing clustering method. Then,
#' the merging procedure is applied in order to merge two close clusters. The merging
#' procedure employs Local Outlier Factor (Breunig et.al, 2000) for assessing if two
#' clusters can be merged.
#'
#' @param data A data matrix of standardized values.
#' @param method An existing clustering method used to create a
#' set of initial clusters which are then merged. The default is "ward"
#' (Ward's hierarchical clustering), other options are "kmeans", "pam",
#' "complete","mclust".
#' @param k.init The number of initial clusters to find, using an initial
#' (existing) clustering method. The default is 10*log(nrow(data)).
#' @param cv The type of the critical value for merging
#' two close clusters. The value is determined based on scores of Local Oultier Factor (LOF).
#' @param q.max The maximal number of the nearest neighbors to calculate LOF for following merging.
#'
#' @details The mergining procedure incorporating LOF is used to evaluate
#' whether or not two close clusters share the same local density 
#' - are from the same group. If so,
#' the two clusters are merged. The procedure is applied untill no two
#' cluster can be merged, see references.
#'
#' @examples 
#' data(ExampleData)
#' res <- IClust(data)
#' table(label,res)
#' 
#' hc <- hclust(dist(data),method="ward")
#' cl <- cutree(hc,k=4)
#' table(label,cl)
#' 
#' 
#' @seealso \code{\link{ExampleData}}
#'
#' @return A resulting cluster membership for each observation.
#' 
#' @author Sarka Brodinova <sarka.brodinova@tuwien.ac.at>
#'
#' @references S. Brodinova, M. Zaharieva, P. Filzmoser, T. Ortner,
#' C. Breiteneder. (2017). Clustering of imbalanced high-dimensional media data. 
#' Advances in Data Analysis and Classification. To appear. Available at http://arxiv.org/abs/1709.10330.
#'
#' @references Breunig, M., Kriegel, H., Ng, R., and Sander, J. (2000).
#' LOF: identifying density-based local outliers.
#' In ACM Int. Conf. on Management of Data, pages 93-104.
#' 
#' @examples 
#' data('ExampleData')
#' res <- IClust(data)
#' table(label,res)
#'
#' @import mclust 
#' @import cluster 
#'
#' @export
IClust <- function(data,method=NULL,k.init=NULL,cv=NULL,q.max=NULL){
rownames(data) <- 1:nrow(data)

# default setting
if(is.null(k.init)){k.init <- round(10* log(nrow(data)) ) } # the number of intial clusters
if(is.null(method)){method <- "ward"} # the initial clustering method
if(is.null(cv)){cv <- 1}
if(is.null(q.max)){q.max <- 5}

#set.seed(seed)
if(method=="kmeans"){
  km <- kmeans(data,centers=k.init)
  cl <- km$cluster
}else if(method=="pam"){
  pam.cl <- pam(data,k=k.init)
  cl <- pam.cl$cluster
}else if(method=="complete"){
  hc <- hclust(dist(data),method="complete")
  cl <- cutree(hc,k=k.init)
}else if(method=="ward"){
  hc <- hclust(dist(data),method="ward.D2")
  cl <- cutree(hc,k=k.init)
}else if(method=="mclust"){
  mc <- Mclust(data,G=k.init)
  cl <- mc$classification
}

DistMatrix<- as.matrix(dist(data))
D <- DistMatrix
diag(D) <- NA
index <- ComputeDistCl1Cl2(cl=cl,dist.matrix=D)

index.i <- NULL
index.j <- NULL
index.i.previous<-0
k <- 0
repeat{
  index.i.old <- unique(index.i)
  index.j.old <- unique(index.j)
  index.i <- index$ind.i
  index.j <- index$ind.j

  for(l in 1:length(index.i)){
    (is.part <- ComputeLofCl1Cl2(i=index.i[l],j=index.j[l],cl=cl,dist.matrix=D,data=data,cv=cv,q.max=q.max))

    if(index.i.previous!=index.j[l]){
      index.i.previous <- index.i[l]
      if(is.part$part & length(is.part$id.cl2)==1){ # cluster size of one
        is.merge <- TRUE
      }else if(is.part$part){ # back direction
        (is.part.back <- ComputeLofCl1Cl2(i=index.j[l],j=index.i[l],cl=cl,dist.matrix=D,data=data,cv=cv,q.max=q.max))
        is.merge <- ifelse(is.part.back$part,TRUE,FALSE)
      }else{
        is.merge <- FALSE
      }
      if(is.merge){
        k <- k+1
        merge.id <- c(is.part$id.cl1,is.part$id.cl2)
        merge.cl <- as.character(c(index.i[l],index.j[l]))
        cl[names(cl) %in% merge.id] <- paste("cl",k,sep="")

        #reculculating the dist between clusters and also the order of distances between clusters
        index <- ReComputeDistCl1Cl2(m.cl=merge.cl,new.cl= paste("cl",k,sep=""), cl=cl,dist.matrix=D,Dist.cl=index$Dist.cl)
        break
      }
    }
  }
  if((length(unique(index.j))==length(index.j.old) &  length(unique(index.i))==length(index.i.old))| length(table(cl))==1){
    break
  }
} # end of repeat
return(clusters=cl)
}# end of function

