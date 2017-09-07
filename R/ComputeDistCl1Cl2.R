ComputeDistCl1Cl2 <- function(cl,dist.matrix){
  id.cl1.cl2 <- names(table(cl))
  Dist <- matrix(NA,nrow=length(id.cl1.cl2),ncol=length(id.cl1.cl2))
  for(i in 1:(length(id.cl1.cl2)-1)){
    for(j in (1+i):length(id.cl1.cl2)){
      cl1 <- names(which(cl==id.cl1.cl2[i]))
      cl2 <- names(which(cl==id.cl1.cl2[j]))

      # the frist cluster cannot be size of one
      # the distance between the same clusters is not computed
      if(length(cl1)==1 & length(cl2)==1){
        Dist[i,j] <- NA
        Dist[j,i] <- NA
      }else if(length(cl1)==1){
        Dist[i,j] <- NA
        Dist[j,i] <- min(dist.matrix[cl1,cl2])
        next
      }else if(length(cl2)==1){
        Dist[i,j] <- min(dist.matrix[cl1,cl2])
        Dist[j,i] <- NA
      }else{
        cl2 <- (which(cl==id.cl1.cl2[j]))
        Dist[i,j] <- min(dist.matrix[cl1,cl2])
        Dist[j,i] <- min(dist.matrix[cl1,cl2])
      }
    }
  }
  rownames(Dist) <- id.cl1.cl2
  colnames(Dist) <- id.cl1.cl2
  order.dist <- order(as.vector(Dist),na.last=NA)
  sort.dist <- as.vector(Dist)[order.dist] # store sorted distance
  index.i <- rep(rownames(Dist),ncol(Dist))[order.dist]
  index.j <- rep(rownames(Dist),each=nrow(Dist))[order.dist]
  return(list(ind.i=index.i,ind.j=index.j,sort.dist=sort.dist,Dist.cl=Dist))
}



