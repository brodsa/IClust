ReComputeDistCl1Cl2 <- function(m.cl,new.cl, cl,dist.matrix,Dist.cl){
  Dist.cl <- Dist.cl[!rownames(Dist.cl)==m.cl[1],!rownames(Dist.cl)==m.cl[1]]
  cl1 <- names(which(cl==new.cl))

  id.cl2 <- names(table(cl))[!names(table(cl)) %in% new.cl]
  for(i in 1:length(id.cl2)){
    if(id.cl2[i]!=new.cl){
      cl2 <- names(which(cl==id.cl2[i]))
      if(length(cl2)==1){
        Dist.cl[id.cl2[i],m.cl[2]] <- NA
        Dist.cl[m.cl[2],id.cl2[i]] <- min(dist.matrix[cl1,cl2])
      }else{
        Dist.cl[id.cl2[i],m.cl[2]] <- min(dist.matrix[cl1,cl2])
        Dist.cl[m.cl[2],id.cl2[i]] <- min(dist.matrix[cl1,cl2])
      }
    }else{
      Dist.cl[m.cl[2],id.cl2[i]] <-NA
    }
  }
  rownames(Dist.cl)[which(rownames(Dist.cl) %in% m.cl[2])] <- new.cl
  colnames(Dist.cl)[which(colnames(Dist.cl) %in% m.cl[2])] <- new.cl

  order.dist <- order(as.vector(Dist.cl),na.last=NA)
  sort.dist <- as.vector(Dist.cl)[order.dist] # store sorted distance
  index.i <- rep(rownames(Dist.cl),ncol(Dist.cl))[order.dist]
  index.j <- rep(colnames(Dist.cl),each=nrow(Dist.cl))[order.dist]
  return(list(ind.i=index.i,ind.j=index.j,sort.dist=sort.dist,Dist.cl=Dist.cl))
}
