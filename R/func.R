tema_mapa <- function(){
  list(
    ggplot2::theme(
      panel.background = ggplot2::element_rect(color="black",fill = "white"),
      panel.grid.major = ggplot2::element_line(color="gray",linetype = 3)),
    ggspatial::annotation_scale(
      location="bl",
      height = ggplot2::unit(0.2,"cm")),
    ggspatial::annotation_north_arrow(
      location="tr",
      style = ggspatial::north_arrow_nautical,
      height = ggplot2::unit(1.5,"cm"),
      width =  ggplot2::unit(1.5,"cm"))
  )
}

limites_outlier <- function(x, coefi=1.5){
  med = median(x)
  li = med - coefi*IQR(x)
  ls = med + coefi*IQR(x)
  c(Lim_Inferior = li, Lim_Superior = ls)
}



## definição de um ponto dentro de um polígono
def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}


meu_cv <- function(x){
  100*sd(x)/mean(x)
}

meu_erro_padrao <- function(x){
  sd(x)/sqrt(length(x))
}


est_descritiva <- function(x){
  n <- length(x)
  n_na <- sum(is.na(x)) # <<<<<<<------------
  x<- na.omit(x)
  m <- mean(x)
  dp <- sd(x)
  md <- median(x) # quantile(x, 0.50)
  cv <- meu_cv(x)
  mini <- min(x)
  maxi <- max(x)
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  s2 <- var(x)
  g1 <- agricolae::skewness(x)
  g2 <- agricolae::kurtosis(x)
  epm <- meu_erro_padrao(x)
  
  
  return(c(N = n,
           N_perdidos = n_na, # <<<<<<<<<--------
           Media = m,Mediana = md,
           Min = mini,Max = maxi,
           Var = s2,DP = dp,
           Q1 = q1,Q3 = q3,
           CV = cv,EPM = epm,
           G1 = g1,G2 = g2))
}

