loffun <- function(data,k,DistMatrix){
  D <- DistMatrix
  kNNs.p.x <- list()
  N_k.p <- list()
  dist_k.p <- c()
  for(i in 1:dim(D)[1]){
    kNNs.p <- sort(D[,i])[-1]
    N_k.p[[i]] <- names(kNNs.p)[kNNs.p <= kNNs.p[(k)]] #N_k
    kNNs.p.x[[i]] <- kNNs.p[1:(length(N_k.p[[i]]))]
    dist_k.p[i] <- max(kNNs.p.x[[i]])
  }

  loc_reach_dist_k.p <- c()
  for(i in 1:dim(D)[1]){
    reach_dist_k.p.o <- c()
    for (j in 1: length(kNNs.p.x[[i]])){
      o1 <- as.numeric(N_k.p[[i]][j])
      reach_dist_k.p.o[j] <- max(dist_k.p[o1], D[i,o1])
    }
    loc_reach_dist_k.p[i] <- length(N_k.p[[i]])/sum(reach_dist_k.p.o)
  }

  lof <-numeric(nrow(D))
  for(i in 1:nrow(D)){

    lof[i] <- sum(loc_reach_dist_k.p[as.numeric(N_k.p[[i]])])/( loc_reach_dist_k.p[i] * length(N_k.p[[i]]))
  }
  return(list(lof=lof))

}
