% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitosegm5.R
\name{fitoseg5}
\alias{fitoseg5}
\title{fitoseg5}
\usage{
fitoseg5(I)
}
\arguments{
\item{I}{La imagen de entrada, imagen RGB (rojo, verde, azul).}
}
\value{
Devuelve la imagen segmentada
}
\description{
Esta función toma una imagen como entrada y devuelve una imagen
     segmentada. La segmentación se basa en la diferencia entre los canales
     rojo y verde de la imagen original.
}
\examples{
library(EBImage)
I<-readImage("IMG_20230606_114757.jpg")
fitoseg5<-function(I)
{
R <-EBImage::channel(I, "red")
G <-EBImage::channel(I, "green")
B <-EBImage::channel(I, "blue")
x<-(G-R)*(G+R)
b<-scales::rescale(matrix(x, ncol = ncol(x), nrow=nrow(x)))
o<-EBImage::otsu(b)
p<-b>o
Rx<-p*R
Gx<-p*G
Bx<-p*B
If<-EBImage::rgbImage(Rx,Gx,Bx)
return(If)
}
x<-fitoseg5(I)
display(x)
}
