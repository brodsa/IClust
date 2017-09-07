ComputeLofCl1Cl2 <- function(i,j,cl,dist.matrix,data,cv=NULL,q.max=NULL){
  # id of observation in the first cluster cl1
  id.cl1 <- names(which(cl==i))
  data.cl1 <- data[id.cl1,]

  # id of observation in the second cluster cl2
  id.cl2 <- names(which(cl==j))
  data.cl2 <- data[id.cl2,]

  # select a point p from cluster cl2 being the closest to cl1
  # at least one of the cluster has to be size of larger than 1
  tp <- ifelse((length(id.cl2)==1 | length(id.cl1)==1),FALSE,TRUE)
  id.p <- if(tp){
    rownames(which(dist.matrix[id.cl2,id.cl1]==min(dist.matrix[id.cl2,id.cl1]),arr.ind=tp))
  }else{
    names(which(cl==j))
  }

  # take the distance matrix for specific cluster and poin p
  D.lof <- dist.matrix[c(id.cl1,id.p),c(id.cl1,id.p)]
  diag(D.lof) <- 0
  rownames(D.lof) <- 1:nrow(D.lof) ; colnames(D.lof) <- 1:nrow(D.lof)

  # calculating lof for cl1 and point p, considering the appropriate number of
  # nearest neighbors
  kk <- ifelse((nrow(D.lof)-1)>q.max,q.max,(nrow(D.lof)-1))
  mp <- 1:kk
  lof.step <-matrix(NA,nrow=length(mp),ncol=nrow(D.lof))
  for(l in 1:length(mp)){ #
    lof.step[l,] <- loffun(data=data[c(id.cl1,id.p),],DistMatrix=D.lof,k=mp[l])$lof
  }

  # calculate a representative value for each observations
  lof.aver <- apply(lof.step,2,mean,na.rm=TRUE)
  names(lof.aver) <- c(id.cl1,id.p)


  # calculate critical value, we have different ways to estimate that
  if(cv==1){crit.val <- median(as.vector(lof.step))+2*mad(as.vector(lof.step))
  }else if(cv==2){crit.val <- mean(as.vector(lof.step))+2*sd(as.vector(lof.step))
  }else if(cv==3){crit.val <- median(lof.aver)+2*mad(lof.aver)
  }else if(cv==4){crit.val <- mean(lof.aver)+2*sd(lof.aver)}
  part <- (lof.aver[id.p]<=crit.val)

  return(list(part=part,id.p=id.p,id.cl1=id.cl1,id.cl2=id.cl2,cv=crit.val,lof.aver=lof.aver))
}
