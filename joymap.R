# PENDIENTE:
# Encapsular en una función con
# Remuestreo bilinear para reducir Y
# Opciones de:
#   con/sin borde, y par. stroke (y degradado?)
#   con/sin sólido, y par. con/sin degradado
# Cuidar extremos (stroke, altura)

# JoyMap
# www.overfitting.net
# https://www.overfitting.net/2017/12/visualizaciones-ad-hoc-de-sonido-con-r.html

library(tiff)


# Generic array functions that work both for matrix (grayscale images)
# and 3-channel arrays (colour images)
# Resample
arrayresample=function(img, DIMX, DIMY, method='bilinear') {
    # method=c('near', 'bilinear', 'cubic', 'cubicspline', 'lanczos')
    
    require(terra)
    
    raster=rast(img)
    rasterrs=rast(nrows=round(DIMY), ncols=round(DIMX), extent=ext(raster))
    rasterrs=resample(raster, rasterrs, method=method, threads=TRUE)
    
    if (is.matrix(img)) return (matrix(as.array(rasterrs), nrow=nrow(rasterrs)))    
    return (as.array(rasterrs))  # convert back to array
}


###########################################################

# Examples

img=readTIFF("tenerife_downsample40.tif")
img=img-min(img)
img=img/max(img)

DIMY=nrow(img)
DIMX=ncol(img)
DOWNSAMPLE=40  # curves separation
HEIGHT=800  # max height in pixels


# TOAST SOLID
imgout=array(0, c(DIMY*DOWNSAMPLE+1, DIMX))
for (y in 1:DIMY) {
    for (x in 1:DIMX) {
        if (img[y,x]) {
            for (i in 0:round(img[y,x]*HEIGHT)) imgout[y*DOWNSAMPLE-i, x]=i/HEIGHT  # fade
            LY=round(y*DOWNSAMPLE-img[y,x]*HEIGHT)
            imgout[(LY-1):(LY+1), x]=0  # black lines
        }
    }
    print(paste0("Calculated row: ", y, "/", DIMY))
}
imgout=imgout[1:(nrow(imgout)-3),]

writeTIFF(imgout, "tenerife_toastsolid.tif",
          bits.per.sample=16, compression="LZW")


# TRON WIREFRAME
imgout=array(0, c(DIMY*DOWNSAMPLE+1, DIMX))
for (y in 1:DIMY) {
    for (x in 1:DIMX) {
        if (img[y,x]) {
            for (i in 0:round(img[y,x]*HEIGHT)) imgout[y*DOWNSAMPLE-i, x]=0
            LY=round(y*DOWNSAMPLE-img[y,x]*HEIGHT)
            imgout[(LY-1):(LY+1), x]=img[y,x]
        }
    }
    print(paste0("Calculated row: ", y, "/", DIMY))
}
imgout=imgout[1:(nrow(imgout)-3),]

writeTIFF(imgout, "tenerife_tronewireframe.tif",
          bits.per.sample=16, compression="LZW")


# JOY DIVISION
img=readTIFF("tenerife2.tif")
img=arrayresample(img, DIMX=5764, DIMY=64)  # reduce Y dimension to 64 rows

img=img-min(img)+0.05
img=img  #+runif(length(img))/80  # adding noise can be nice
img=img/max(img)
writeTIFF(img, "tenerife2resample.tif",
          bits.per.sample=16, compression="LZW")

DIMY=nrow(img)
DIMX=ncol(img)
DOWNSAMPLE=80  # curves separation
HEIGHT=1600  # 800  # max height in pixels

SAFE=2
imgout=array(0, c(DIMY*DOWNSAMPLE+1, DIMX))
for (y in (1+SAFE*3):(DIMY-SAFE*0)) {
    for (x in (1+SAFE):(DIMX-SAFE)) {
        if (img[y,x]) {
            for (i in 0:round(img[y,x]*HEIGHT)) imgout[y*DOWNSAMPLE-i, x]=0
            LY=round(y*DOWNSAMPLE-img[y,x]*HEIGHT)
            imgout[(LY-2):(LY+2), (x-2):(x+2)]=1
        }
    }
    print(paste0("Calculated row: ", y, "/", DIMY))
}
imgout=imgout[1:(nrow(imgout)-3),]

writeTIFF(imgout, "tenerife_joydivision.tif",
          bits.per.sample=16, compression="LZW")
