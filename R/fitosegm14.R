#' @title fitoseg14
#' @description Esta función toma una imagen como entrada y devuelve una imagen
#'      segmentada. La segmentación se basa en la diferencia entre los canales
#'      rojo y verde de la imagen original.
#' @param I La imagen de entrada, imagen RGB (rojo, verde, azul).
#' @return Devuelve la imagen segmentada
#' @export
#' @examples
#' library(EBImage)
#' I<-readImage("IMG_20230606_114941.jpg")
#' fitoseg14<- function(I)
#'{
#' R <-EBImage::channel(I, "red")
#' G <-EBImage:: channel(I, "green")
#' B <-EBImage::channel(I, "blue")
#' x<-0.441*R-0.811*G+0.385*B+18.78745
#' b<-scales::rescale(matrix(x, ncol = ncol(x), nrow=nrow(x)))
#' o<-EBImage::otsu(b)
#' p<-b<o
#' Rx<-p*R
#' Gx<-p*G
#' Bx<-p*B
#' If<-EBImage::rgbImage(Rx,Gx,Bx)
#' return(If)
#'}
#' x<-fitoseg14(I)
#' display(x)


fitoseg14<- function(I)
{
  R <-EBImage::channel(I, "red")
  G <-EBImage:: channel(I, "green")
  B <-EBImage::channel(I, "blue")
  x<-0.441*R-0.811*G+0.385*B+18.78745
  b<-scales::rescale(matrix(x, ncol = ncol(x), nrow=nrow(x)))
  o<-EBImage::otsu(b)
  p<-b<o
 
  Rx<-p*R
  Gx<-p*G
  Bx<-p*B
  If<-EBImage::rgbImage(Rx,Gx,Bx)
  
  return(If)
}


