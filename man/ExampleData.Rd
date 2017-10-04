\name{ExampleData}
\alias{ExampleData}
\docType{data}
\title{Illustrative Example
%%   ~~ data name/kind ... ~~
}
\description{
A simple illustrative data example constisting of 4 highly imbalanced groups. The groups are described be a large number of variables - both informative and noise. The informative variables follows a finite mixture of Gaussian distributions.
}
\usage{data("ExampleData")}
\format{
  Two objects are uploaded.
  \describe{
    \item{\code{data}}{A data frame consisting of standardized values with 343 observations and 300 variables. The observations form 4 groups of highly different size. The group structure is given by the first 100 variables and the remaining variables represent noise. }
    \item{\code{label}}{A numeric vector reflecting a group memberhip of each observation.}
  }
}
\details{
The code generating data is visible in example, see below.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ExampleData)
dim(data)
table(label)

\dontrun{
 require(MASS)     # multivariate normal distribution
 require(mixAK)    # rotation matrix
 require(ggplot2)  # a grafical tool
 
 # dimensionality of data set
 p_inf <- 100 
 p_noise <- 200
 n_k <- c(300,30,10,3)
 
 # covariances of groups
 sigma1 <- matrix(0.3,nrow=p_inf,ncol=p_inf)
 diag(sigma1) <- 1
 set.seed(12)
 R <- rRotationMatrix(n=1, dim=p_inf)
 sigma1 <- R %*% sigma1 %*% t(R)
 
 sigma2 <- matrix(0.0,nrow=p_inf,ncol=p_inf)
 diag(sigma1) <- 5
 set.seed(35)
 R <- rRotationMatrix(n=1, dim=p_inf)
 sigma2 <- R %*% sigma2 %*% t(R)
 
 sigma3 <- diag(p_inf)
 diag(sigma3) <- 2
 set.seed(68)
 R <- rRotationMatrix(n=1, dim=p_inf)
 sigma3 <- R %*% sigma3 %*% t(R)
 
 sigma4 <- diag(p_inf)
 diag(sigma4) <- 3
 set.seed(98)
 R <- rRotationMatrix(n=1, dim=p)
 sigma4 <- R %*% sigma4 %*% t(R)
 
 # groups follow Gaussion distribution
 g1 <- mvrnorm(n_k[1],mu=rep(0,p), Sigma=sigma1)
 g2 <- mvrnorm(n_k[2],mu=rep(c(0,-3),p/2),Sigma=sigma2)
 g3 <- mvrnorm(n_k[3],mu=rep(-3,p),Sigma=sigma3)
 g4 <- mvrnorm(n_k[4],mu=rep(c(-5,5),p/2),Sigma=sigma4)
 X <- rbind(cl1,cl2,cl3,cl4)
 
 # group membership
 label <- c(rep(1,n_k[1]),rep(2,n_k[2]),rep(3,n_k[3]),rep(4,n_k[4])) 
 # additing noise part
 X <- cbind(X, matrix(rnorm(p_noise * sum(n_k), mean = 0), ncol = p_noise))
 data <- scale(X)
}
}