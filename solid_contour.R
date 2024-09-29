# solid() and contour() auxiliar functions
# www.overfitting.net
# https://www.overfitting.net/

library(terra)  # read GeoTIFF, reprojection, crop and resample
library(tiff)  # save 16-bit TIFF's
library(png)  # save 8-bit PNG's


solid=function(DEM, altitude=0, isnan=0) {
    # solid() calculates a solid version of a DEM
    #
    # altitude: DEM altitude level contour
    # isnan: value assigned to NaN data

    DIMY=nrow(DEM)
    DIMX=ncol(DEM)
    
    # If array turn its first dimension into a genuine matrix
    if (!is.matrix(DEM)) {
        print("WARNING: input DEM is not a matrix but an array. First dimension is used")
        DEM=matrix(DEM[,,1], nrow=DIMY, ncol=DIMX)
    }
    DEM[is.nan(DEM)]=isnan
    
    # Calculate solid map from DEM
    solidmap=DEM*0
    solidmap[DEM > altitude]=1
    
    return(solidmap)
}

contour=function(DEM, stroke=1) {
    # contour() calculates the contours of any colour change
    #
    # stroke: line width (pixels)
    
    DIMY=nrow(DEM)    
    DIMX=ncol(DEM)
    
    # Calculate outline map from solid map
    outline=DEM*0
    
    # 1 pixel stroke outline
    outline[2:(DIMY-1), 2:(DIMX-1)]=
        abs(DEM[1:(DIMY-2), 2:(DIMX-1)] -
            DEM[2:(DIMY-1), 2:(DIMX-1)]) +
        abs(DEM[2:(DIMY-1), 1:(DIMX-2)] -
            DEM[2:(DIMY-1), 2:(DIMX-1)])
    
    # 2 pixel stroke outline
    if (stroke==2 | stroke>3) {
        outline[2:(DIMY-1), 2:(DIMX-1)]=outline[2:(DIMY-1), 2:(DIMX-1)] +
        outline[1:(DIMY-2), 2:(DIMX-1)]+outline[2:(DIMY-1), 3:(DIMX-0)]
    }
    
    # 3 pixel stroke outline
    if (stroke>2) {
        outline[2:(DIMY-1), 2:(DIMX-1)]=outline[2:(DIMY-1), 2:(DIMX-1)] +
        outline[1:(DIMY-2), 2:(DIMX-1)]+outline[3:(DIMY-0), 2:(DIMX-1)] +
        outline[2:(DIMY-1), 1:(DIMX-2)]+outline[2:(DIMY-1), 3:(DIMX-0)]
    }
    
    outline[outline!=0]=1
    return(outline)
}


###########################################################

# Examples

baleares=rast("PNOA_MDT200_ETRS89_HU31_Baleares.tif")  # (985x1544 px)

# DEM as matrix
DEM=as.array(baleares)
DIMY=nrow(DEM)    
DIMX=ncol(DEM)
DEM=matrix(DEM, nrow=DIMY, ncol=DIMX)
DEM[is.nan(DEM)]=0  # undefined points belong to sea
DEM[DEM<0]=0  # clip negative altitudes to 0 (sea level)
writeTIFF(DEM/max(DEM), "baleares.tif")

# Solid map
mapsolid=solid(DEM)
writePNG(mapsolid, "mapsolid.png")

# Contour map
mapcontour=contour(mapsolid)
writePNG(mapcontour, "mapcontour.png")

    