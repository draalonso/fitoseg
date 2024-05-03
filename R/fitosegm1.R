#' @title fitoseg1
#' @description Esta función toma una imagen como entrada y devuelve una imagen
#'      segmentada. La segmentación se basa en la diferencia entre los canales
#'      rojo y verde de la imagen original.
#' @param I La imagen de entrada, imagen RGB (rojo, verde, azul).
#' @return Devuelve la imagen segmentada
#' @export
#' @examples
#' library(EBImage)
#' I<-readImage("IMG_20230606_114757.jpg")
#' fitoseg1 <- function(I)
#'{
#' R <-EBImage::channel(I, "red")
#' G <-EBImage::channel(I, "green")
#' B <-EBImage::channel(I, "blue")
#' x<-(R-G)/(G + R-B)
#' b<-scales::rescale(matrix(x, ncol = ncol(x), nrow=nrow(x)))
#' o<-EBImage::otsu(b)
#' p<-b<o
#' Rx<-p*R
#' Gx<-p*G
#' Bx<-p*B
#' If<-EBImage::rgbImage(Rx,Gx,Bx)
#' return(If)
#'}
#' x<-fitoseg1(I)
#' display(x)


fitoseg1 <- function(I)
  
{ 
  
  R <-EBImage::channel(I, "red")
  G <-EBImage::channel(I, "green")
  B <-EBImage::channel(I, "blue")
  x<-(R-G)/(G + R-B)
  b<-scales::rescale(matrix(x, ncol = ncol(x), nrow=nrow(x)))
  o<-EBImage::otsu(b)
  p<-b<o

  Rx<-p*R
  Gx<-p*G
  Bx<-p*B
  If<-EBImage::rgbImage(Rx,Gx,Bx)

  return(If)
  
}

