# Funções para pré tratamento de dados para PCA
#' @param X matriz de entrada
#' @export
# Auto escalamento dos dados
auto_scale <- function(X){
  m<- nrow(X)
  xm<- colMeans(X)
  A<-matrix(1,nrow = m,ncol = 1)%*%xm
  dp<- sqrt(colSums((X-A)^2)/(m-1))
  B<-matrix(1,nrow = m,ncol = 1)%*%dp
  Xauto<-(X-A)/B

}

# Centrando na média os dados

center_median <- function(X){
  m<- nrow(X)
  xm<- colMeans(X)
  A<-matrix(1,nrow = m,ncol = 1)%*%xm
  Xcm<-X-A
}
