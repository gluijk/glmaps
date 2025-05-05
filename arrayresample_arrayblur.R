# Generic array resample and blur functions based on terra
# www.overfitting.net
# https://www.overfitting.net/2024/01/reescalado-de-imagenes-con-el-paquete.html

library(terra)
library(png)
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
    return (as.array(rasterrs))  # convert back to matrix/array
}


# Blur
# https://stackoverflow.com/questions/70429190/how-can-i-perform-neighborhood-analysis-in-terra-or-raster-and-keep-the-same-na
arrayblur=function(img, radius=10, kernel='gaussian', fun='mean') {
    # radius: radius of the averaging window
    # kernel: 'gaussian', 'spherical'. Otherwise all 1's
    
    require(terra)
    
    # Build circular kernel
    D=2*radius+1  # D will always be an odd number as required by focal()
    sigma=radius/3  # keep gaussian shape for every radius
    w=matrix(1, nrow=D, ncol=D)
    if (kernel=='gaussian') {  # gaussian filter
        w=exp(-((row(w)-(radius+1))^2 + (col(w)-(radius+1))^2) / (2 * sigma^2))
    } else if (kernel=='spherical') {  # spherical filter
        w=1 - ((row(w)-(radius+1))^2 + (col(w)-(radius+1))^2) / (radius+1)^2        
    }
    # Ignore values out of the radius
    w[(row(w)-(radius+1))^2 + (col(w)-(radius+1))^2 > (radius+1)^2]=NA
    writePNG(w, "blurkernel.png")
    
    raster=rast(img)  # process as raster
    rasterblur=focal(raster, w=w, fun=fun, na.rm=TRUE, na.policy='omit')
    
    if (is.matrix(img)) return (matrix(as.array(rasterblur), nrow=nrow(rasterblur)))
    else return (as.array(rasterblur))  # convert back to matrix/array
}


###########################################################

# Examples

img=readTIFF("tenerife.tif")

# Resample
imgrs=arrayresample(img, DIMX*0.4, DIMY*0.4)
imgrs[imgrs<0]=0  # clip saturated levels
imgrs[imgrs>1]=1  # created by interpolation
writeTIFF(imgrs, "tenerifers.tif", bits.per.sample=16)

# Blur (sea pixels don't participate in blur calculation)
imgrsNA=img
imgrsNA[imgrsNA==0]=NA  # NA pixels will be ignored in bluring
imgblur=arrayblur(imgrsNA, radius=40)
imgblur[is.na(imgblur)]=0
writeTIFF(imgblur, "tenerifeblur.tif", bits.per.sample=16)
