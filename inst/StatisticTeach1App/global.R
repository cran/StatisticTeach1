# Load custom functions
kurtosis <- function(x) {
  m4 <- mean((x - mean(x))^4)
  kurtosis <- m4/(sd(x)^4) - 3
  kurtosis
}

kurtosis_standardized <- function(x) {
  n <- length(x)
  m4 <- mean((x - mean(x))^4)
  kurtosis_standardized <- (m4/(sd(x)^4)) - 3 * ((n-1)/((n-2)*(n-3)))
  kurtosis_standardized
}


skewness <-  function(x) {
  m3 <- mean((x - mean(x))^3)
  skewness <- m3/(sd(x)^3)
  skewness
}
skewness_standardized <- function(x) {
  m3 <- mean((x - mean(x))^3)
  s3 <- sd(x)^3
  n <- length(x)
  skewness <- m3/s3
  asimetria_estandarizada <- skewness * sqrt(n * (n-1)) / (n-2)
  asimetria_estandarizada
}




geometric.mean <-
  function(x,na.rm=TRUE){ if (is.null(nrow(x))) {exp(mean(log(x),na.rm=na.rm)) } else {
    exp(apply(log(x),2,mean,na.rm=na.rm))} }

bsModalNoClose <- function(...) {
  b <- shinyBS::bsModal(...)
  b[[2]]$`data-backdrop` <- "static"
  b[[2]]$`data-keyboard` <- "false"
  return(b)
}

# temas ggplot
themesb <- list(
  "grey" = ggplot2::theme_grey(),
  "minimal" = ggplot2::theme_minimal(),
  "bw" = ggplot2::theme_bw(),
  "classic" = ggplot2::theme_classic(),
  "linedraw" = ggplot2::theme_linedraw(),
  "light" = ggplot2::theme_light(),
  "dark" = ggplot2::theme_dark(),
  "test" = ggplot2::theme_test(),
  "void" = ggplot2::theme_void()
)

fuentes <- list(
  "mono" ="mono",
  "sans" = "sans",
  "serif" = "serif",
  "Courier"= "Courier" ,
  "Helvetica"="Helvetica",
  "Times"= "Times",
  "Palatino" ="Palatino"
   )

fuentes <- list(
  "mono" ="mono",
  "sans" = "sans",
  "serif" = "serif",
  "Courier"= "Courier" ,
  "Helvetica"="Helvetica",
  "Times"= "Times",
  "Palatino" ="Palatino"
)

paletas <- c("Accent",
             "Dark2",
             "Paired",
             "Pastel1",
             "Pastel2",
             "Set1",
             "Set2",
  "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys",
  "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples",
  "RdPu" ,"Reds" ,"YlGn", "YlGnBu","YlOrBr", "YlOrRd",
  "BrBG" ,"PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
  "RdYlGn", "Spectral","Set3")


Multiva <-function(datos){
  n<-nrow(datos)
  n
  p<-ncol(datos)
  p
  #vector de unos
  vector.uno<-matrix(rep(1,n),ncol=1)
  vector.uno

  ## a.- EL VECTOR DE MEDIAS(xprom) = (x'1n)/n
  vector.media<-(t(datos)%*%vector.uno)/n
  vector.media

  ## b.- Matriz de covarianza S=1/n(x'*x)-((vector.media)(vector.media)')
  matriz.covarianza<-S<-cov(datos)*(n-1)/n
  matriz.covarianza

  #Inversa de la matriz de covarianzas
  SI<-solve(S)
  SI

  ##c.-Matriz de Correlación R=D^-1/2*S*D^-1/2
  DN<-diag(c(1/sqrt(diag(S))))
  DN
  matriz.correlacion<-DN%*%S%*%DN

  ##d.-Medidas Globales de variabilidad

  ##d.1.-Varianza total : VT=traza(S)
  VT<-sum(diag(S))
  VT

  ##d.2.-Varianza media : VM=VT/p
  VM<-VT/p
  VM

  ##d.3.- varianza generalizada : VG = det(S)
  VG<-det(S)
  VG

  ##d.4.-Varianza efectiva : VE=[det(S)]^1/p
  VE<-(det(S))^(1/p)
  VE

  ##e.- Distancia de Mahalanobis
  x<-as.matrix(datos)
  media<-colMeans(datos)
  matriz.media<-matrix(media,nrow = n,ncol=p,byrow = TRUE)
  distm<-((x-matriz.media)%*%SI)%*%t((x-matriz.media))

  cat("----------------------------------------------------------------","\n")
  cat("-Global measures of variability","\n",
      "1.- Total variance =", VT, "\n",
      "2.- Mean variance =",VM, "\n",
      "3.- Generalized Variance=",VG, "\n",
      "4.- Effective Variance =",VE, "\n")
  cat("----------------------------------------------------------------","\n")

}


# Función para encontrar el valor de a correspondiente a una probabilidad dada para diferentes distribuciones
# distribución: cadena de texto que indica la distribución ("Normal", "t-Student", "Chi^2", "F-Fisher", "Exponential", "Logistics", "Log-Normal", "Gamma", "Weibull" o "Beta")
# probabilidad: probabilidad deseada (0 < probabilidad < 1)
# grados_libertad: número de grados de libertad para la distribución t-Student o la chi cuadrada
# param1, param2: parámetros de forma y escala para las distribuciones exponencial, log-normal, gamma, weibull y beta
# Retorna el valor de a correspondiente a la probabilidad deseada

valor_x_lower <- function(distr, prob, df = NULL, param1 = NULL, param2 = NULL) {
  if(distr == "Normal") {
    return(qnorm(prob))
  } else if(distr == "t-Student") {
    return(qt(prob, df = df,lower.tail = TRUE))
  } else if(distr == "Chi^2") {
    return(qchisq(prob, df = df,lower.tail = TRUE))
  } else if(distr == "F-Fisher") {
    return(qf(prob, df1 = param1, df2 = param2,lower.tail = TRUE))
  } else if(distr == "Exponential") {
    return(qexp(prob, rate = param1,lower.tail = TRUE))
  } else if(distr == "Logistics") {
    return(qlogis(prob, location = param1, scale = param2,lower.tail = TRUE))
  } else if(distr == "Log-Normal") {
    return(qlnorm(prob, meanlog = param1, sdlog = param2,lower.tail = TRUE))
  } else if(distr == "Gamma") {
    return(qgamma(prob, shape = param1, rate = param2,lower.tail = TRUE))
  } else if(distr == "Weibull") {
    return(qweibull(prob, shape = param1, scale = param2,lower.tail = TRUE))
  } else if(distr == "Beta") {
    return(qbeta(prob, shape1 = param1, shape2 = param2,lower.tail = TRUE))
  } else {
    stop("Distribución no soportada.")
  }
}
