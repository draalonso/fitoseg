#' @title fitoseg13
#' @description Esta función toma una imagen como entrada y devuelve una imagen
#'      segmentada. La segmentación se basa en la diferencia entre los canales
#'      rojo y verde de la imagen original.
#' @param I La imagen de entrada, imagen RGB (rojo, verde, azul).
#' @return Devuelve la imagen segmentada
#' @export
#' @examples
#' library(EBImage)
#' I<-readImage("IMG_20230606_114757.jpg")
#' fitoseg13<- function(I)
#'{
#' R <-EBImage::channel(I,"red")
#' G <-EBImage::channel(I, "green")
#' B <-EBImage::channel(I, "blue")
#' x<- 1.262*G-0.884*R-0.311*B
#' b<-scales::rescale(matrix(x, ncol = ncol(x), nrow=nrow(x)))
#' o<-EBImage::otsu(b)
#' p<-b>o
#' Rx<-p*R
#' Gx<-p*G
#' Bx<-p*B
#' If<-EBImage::rgbImage(Rx,Gx,Bx)
#' return(If)
#'}
#' x<-fitoseg13(I)
#' display(x)


fitoseg13<- function(I)
{
  R <-EBImage::channel(I,"red")
  G <-EBImage::channel(I, "green")
  B <-EBImage::channel(I, "blue")
  x<- 1.262*G-0.884*R-0.311*B 
  b<-scales::rescale(matrix(x, ncol = ncol(x), nrow=nrow(x)))
  o<-EBImage::otsu(b)
  p<-b>o
 
  Rx<-p*R
  Gx<-p*G
  Bx<-p*B
  If<-EBImage::rgbImage(Rx,Gx,Bx)
  
  return(If)
}


