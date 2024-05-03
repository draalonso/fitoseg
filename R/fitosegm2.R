#' @title fitoseg2
#' @description Esta función toma una imagen como entrada y devuelve una imagen
#'      segmentada. La segmentación se basa en la diferencia entre los canales
#'      rojo y verde de la imagen original.
#' @param I La imagen de entrada, imagen RGB (rojo, verde, azul).
#' @return Devuelve la imagen segmentada
#' @export
#' @examples
#' library(EBImage)
#' I<-readImage("IMG_20230606_114757.jpg")
#' fitoseg2<-function(I)
#'{
#' R <-EBImage::channel(I, "red")
#' G <-EBImage::channel(I, "green")
#' B <-EBImage::channel(I, "blue")
#' x<-2.5*(B-G)/(B+6*G-7.5*R+1)
#' b<-scales::rescale(matrix(x, ncol = ncol(x), nrow=nrow(x)))
#' o<-EBImage::otsu(b)
#' p<-b<o
#' Rx<-p*R
#' Gx<-p*G
#' Bx<-p*B
#' If<-EBImage::rgbImage(Rx,Gx,Bx)
#' return(If)
#'}
#' x<-fitoseg2(x)
#' display(x)


fitoseg2<-function(I)
{
  
  R <-EBImage::channel(I, "red")
  G <-EBImage::channel(I, "green")
  B <-EBImage::channel(I, "blue")
  x<-2.5*(B-G)/(B+6*G-7.5*R+1)
  b<-scales::rescale(matrix(x, ncol = ncol(x), nrow=nrow(x)))
  o<-EBImage::otsu(b)
  p<-b<o

  Rx<-p*R
  Gx<-p*G
  Bx<-p*B
  If<-EBImage::rgbImage(Rx,Gx,Bx)

  return(If)
}

