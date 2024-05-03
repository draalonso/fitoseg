#' @title fitoseg16
#' @description Esta función toma una imagen como entrada y devuelve una imagen
#'      segmentada. La segmentación se basa en la diferencia entre los canales
#'      rojo y verde de la imagen original.
#' @param I La imagen de entrada, imagen RGB (rojo, verde, azul).
#' @param c contraste (valor predeterminado: 1)
#' @param b brillo (valor predeterminado: 0.2)
#' @param tamaño (valor predeterminado: 31): Este parámetro se relaciona con el
#'      tamaño de máscara que se utiliza durante el procesamiento.
#' @param sigma (valor predeterminado: 11): Es probable que este parámetro esté
#'      relacionado con un filtro Gaussiano que se aplica en algún punto del
#'      proceso
#' @return Devuelve la imagen segmentada
#' @export
#' @examples
#' library(EBImage)
#' I<-readImage("IMG_20230606_114954.jpg")
#' fitoseg16<-function(I,c=1, b=.2, tamaño = 31, sigma=11)
#'{
#' f<-EBIamge::filter2(I, EBImage::makeBrush(size=999, "Gaussian", sigma=21))
#' r<-EBImage::channel(f,"red")
#' g<-EBImage::channel(f,"green")
#' b<-EBImage::channel(f,"blue")
#' x<-g>r&g>b
#' Rx<-x*EBImage::channel(I,"red")
#' Gx<-x*EBImage::channel(I,"green")
#' Bx<-x*EBImage::channel(I,"blue")
#' If<-EBImage::rgbImage(Rx,Gx,Bx)
#' return(If)
#'}
#' x<-fitoseg16(I)
#' display(x)


fitoseg16<-function(I,c=1, b=.2, tamaño = 31, sigma=11)
  
{
  f<-EBImage::filter2(I, EBImage::makeBrush(size=999, "Gaussian", sigma=21))
  r<-EBImage::channel(f,"red")
  g<-EBImage::channel(f,"green")
  b<-EBImage::channel(f,"blue")
  x<-g>r&g>b
  Rx<-x*EBImage::channel(I,"red")
  Gx<-x*EBImage::channel(I,"green")
  Bx<-x*EBImage::channel(I,"blue")
  If<-EBImage::rgbImage(Rx,Gx,Bx)
  return(If)
  
}

