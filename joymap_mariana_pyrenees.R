# Pyrenees. Practice with GeoTIFF format and raster package
# www.overfitting.net
# https://www.overfitting.net/


library(raster)  # read GeoTIFF, reprojection, crop and resample
library(tiff)  # save 16-bit TIFF's
library(png)  # save 8-bit PNG's


# Building MP4:
# ffmpeg -framerate 30 -i img%4d.png -i oblivionwakingup.wav
#        -c:v libx264 -crf 18 -pix_fmt yuv420p deepwaters.mp4


##################################################################
# PYRENEES

# https://download.gebco.net/
# The GEBCO_2023 Grid is a global terrain model for ocean and land,
# providing elevation data, in meters, on a 15 arc-second interval grid
# of 43200 rows x 86400 columns, giving 3,732,480,000 data points.
# The data values are pixel-centre registered i.e. they refer to elevations,
# in meters, at the centre of grid cells.
pyrenees=raster("gebco_2023_n44.2365_s41.0504_w-2.2852_e4.1199.tif")  # Pyrenees
pyrenees
plot(pyrenees)


# Convert to matrix and save as TIFF
DEM=as.matrix(pyrenees)
DIMY=nrow(pyrenees)
DIMX=ncol(pyrenees)

# Calculate solid map contour
solid=DEM
solid[ !is.na(solid) & solid>=0]=1  # set >=0 areas to 1 (land in a map)
solid[(!is.na(solid) & solid<0) | is.na(solid)]=0  # set NA/negative areas to 0 (water in a map)
writePNG(solid, "mapsolid.png")

# Calculate outline map (0/1) from solid map
outline=solid*0
# 1 pixel thickness outline
outline[2:(DIMY-1), 2:(DIMX-1)]=
    abs(solid[1:(DIMY-2), 2:(DIMX-1)] -
            solid[2:(DIMY-1), 2:(DIMX-1)]) +
    abs(solid[2:(DIMY-1), 1:(DIMX-2)] -
            solid[2:(DIMY-1), 2:(DIMX-1)])
# increase to 2 pixel thickness outline
outline[2:(DIMY-1), 2:(DIMX-1)]=outline[2:(DIMY-1), 2:(DIMX-1)]+
    outline[1:(DIMY-2), 2:(DIMX-1)]+outline[2:(DIMY-1), 3:(DIMX-0)]
# increase to 3 pixel thickness outline
outline[2:(DIMY-1), 2:(DIMX-1)]=outline[2:(DIMY-1), 2:(DIMX-1)]+
    outline[1:(DIMY-2), 2:(DIMX-1)]+outline[3:(DIMY-0), 2:(DIMX-1)]+
    outline[2:(DIMY-1), 1:(DIMX-2)]+outline[2:(DIMY-1), 3:(DIMX-0)]
outline[outline!=0]=1
writePNG(outline, "mapoutline.png")

# Generate final TIFF
hist(DEM, breaks=1000)
DEM[DEM<0]=0
writeTIFF(DEM/max(DEM), "pyrenees.tif",
          compression='LZW', bits.per.sample=16)

# Photoshop: rotate, crop, scale to final size
DEM=readTIFF("pyrenees_rotated_scaled.tif")
dim(DEM)

# Output Joy Division wireframe
DEM=DEM/max(DEM)

DIMY=nrow(DEM)
DIMX=ncol(DEM)
DOWNSAMPLE=2*10  #*2  # curves separation
HEIGHT=80  # max height in pixels


# TRON WIREFRAME
imgout=array(0, c(DIMY*DOWNSAMPLE+1, DIMX))
DEM=DEM^2
for (y in 2:DIMY) {
    for (x in 1:DIMX) {
        if (1) {  # (DEM[y,x]) {
            for (i in 0:round(DEM[y,x]*HEIGHT)) imgout[y*DOWNSAMPLE-i, x]=0
            LY=round(y*DOWNSAMPLE-DEM[y,x]*HEIGHT)
            imgout[(LY-0):(LY+1), x]=1  # DEM[y,x]
        }
    }
    print(paste0("Calculated row: ", y, "/", DIMY))
}
imgout=imgout[1:(nrow(imgout)-3),]

writeTIFF(imgout, "pyrenee_joydivision.tif",
          bits.per.sample=16, compression="LZW")



# TOAST SOLID
imgout=array(0, c(DIMY*DOWNSAMPLE+1, DIMX))
for (y in 2:DIMY) {
    for (x in 1:DIMX) {
        if (DEM[y,x]) {
            for (i in 0:round(DEM[y,x]*HEIGHT)) if (y*DOWNSAMPLE-i>0) imgout[y*DOWNSAMPLE-i, x]=i/HEIGHT
            # print(paste0("Pintando para y desde ", y*DOWNSAMPLE," hasta ", y*DOWNSAMPLE-round(DEM[y,x]*HEIGHT)))
            #LY=round(y*DOWNSAMPLE-DEM[y,x]*HEIGHT)
            # imgout[(LY-0):(LY+1), x]=0
        } 
    }
    print(paste0("Calculated row: ", y, "/", DIMY))
}
imgout=imgout[1:(nrow(imgout)-3),]

writeTIFF(imgout, "pyrenee_joydivision.tif",
          bits.per.sample=16, compression="LZW")


# Photoshop: resize down, colour, add border to proper dimensions
marco=readPNG("pyrenees_marco.png")
datos=readTIFF("pyrenees_final.tif")

DIMY=768
DIMX=600


# Create sequence
NFRAMES=1895  # 157.9s audio track at 12fps
for (frame in 0:(NFRAMES-1)) {
    frm=marco
    ROWEND=nrow(datos)-frame
    ROWINI=ROWEND-DIMY+1
    frm[201:(200+DIMY), 66:(65+DIMX), ]=datos[ROWINI:ROWEND,,]
    
    writePNG(frm, paste0("img", ifelse(frame<10, "000",
        ifelse(frame<100, "00",
        ifelse(frame<1000, "0", ""))), frame, ".png"))
    print(paste0(frame+1, "/", NFRAMES))
}


##################################################################
# MARIANA TRENCH

# https://download.gebco.net/
# The GEBCO_2023 Grid is a global terrain model for ocean and land,
# providing elevation data, in meters, on a 15 arc-second interval grid
# of 43200 rows x 86400 columns, giving 3,732,480,000 data points.
# The data values are pixel-centre registered i.e. they refer to elevations,
# in meters, at the centre of grid cells.
mariana=raster("gebco_2023_n42.4731_s9.9536_w133.9014_e151.3916.tif")  # Mariana Trench
mariana
plot(mariana)


# Convert to matrix and save as TIFF
DEM=as.matrix(mariana)
DIMY=nrow(mariana)
DIMX=ncol(mariana)

MIN=min(DEM)  # -10921 (Challenger Deep)
MAX=max(DEM)  # 3679 (Mount Fuji)
LEVEL0=-MIN/(MAX-MIN)

# Calculate solid map contour
solid=DEM
solid[ !is.na(solid) & solid>=0]=1  # set >=0 areas to 1 (land in a map)
solid[(!is.na(solid) & solid<0) | is.na(solid)]=0  # set NA/negative areas to 0 (water in a map)
writePNG(solid, "mapsolid.png")

# Calculate outline map (0/1) from solid map
outline=solid*0
# 1 pixel thickness outline
outline[2:(DIMY-1), 2:(DIMX-1)]=
    abs(solid[1:(DIMY-2), 2:(DIMX-1)] -
            solid[2:(DIMY-1), 2:(DIMX-1)]) +
    abs(solid[2:(DIMY-1), 1:(DIMX-2)] -
            solid[2:(DIMY-1), 2:(DIMX-1)])
# increase to 2 pixel thickness outline
outline[2:(DIMY-1), 2:(DIMX-1)]=outline[2:(DIMY-1), 2:(DIMX-1)]+
    outline[1:(DIMY-2), 2:(DIMX-1)]+outline[2:(DIMY-1), 3:(DIMX-0)]
# increase to 3 pixel thickness outline
outline[2:(DIMY-1), 2:(DIMX-1)]=outline[2:(DIMY-1), 2:(DIMX-1)]+
    outline[1:(DIMY-2), 2:(DIMX-1)]+outline[3:(DIMY-0), 2:(DIMX-1)]+
    outline[2:(DIMY-1), 1:(DIMX-2)]+outline[2:(DIMY-1), 3:(DIMX-0)]
outline[outline!=0]=1
writePNG(outline, "mapoutline.png")

# Generate final TIFF
hist(DEM, breaks=1000)
DEM=DEM-min(DEM)
DEM[DEM<0]=0
writeTIFF(DEM/max(DEM), "mariana.tif",
          compression='LZW', bits.per.sample=16)

# Photoshop: rotate, crop, scale to final size
DEM=readTIFF("mariana_scaled.tif")
dim(DEM)

# Output Joy Division wireframe
DEM=DEM-min(DEM)
DEM=DEM/max(DEM)

DIMY=nrow(DEM)
DIMX=ncol(DEM)
DOWNSAMPLE=3*10  #*2  # curves separation
HEIGHT=300  # max height in pixels


# TRON WIREFRAME
imgout=array(0, c(DIMY*DOWNSAMPLE+1, DIMX))
for (y in 2:DIMY) {
    for (x in 1:DIMX) {
        if (1) {  # (DEM[y,x]) {
            for (i in 0:round(DEM[y,x]*HEIGHT)) imgout[y*DOWNSAMPLE-i, x]=0
            LY=round(y*DOWNSAMPLE-DEM[y,x]*HEIGHT)
            imgout[(LY-0):(LY+1), x]=1  # DEM[y,x]
        }
    }
    print(paste0("Calculated row: ", y, "/", DIMY))
}
imgout=imgout[1:(nrow(imgout)-3),]

writeTIFF(imgout, "mariana_joydivision.tif",
          bits.per.sample=16, compression="LZW")



# TOAST SOLID
imgout=array(0, c(DIMY*DOWNSAMPLE+1, DIMX))
imgout2=imgout
for (y in 2:DIMY) {
    for (x in 1:DIMX) {
        if (DEM[y,x]) {
            for (i in 0:round(DEM[y,x]*HEIGHT)) if (y*DOWNSAMPLE-i>0) imgout[y*DOWNSAMPLE-i, x]=i/HEIGHT                
            if (DEM[y,x]>=LEVEL0) {
                for (i in round(LEVEL0*HEIGHT):round(DEM[y,x]*HEIGHT)) if (y*DOWNSAMPLE-i>0) imgout2[y*DOWNSAMPLE-i, x]=i/HEIGHT   # 1-i*0.7/HEIGHT               
            }
            #LY=round(y*DOWNSAMPLE-DEM[y,x]*HEIGHT)
            # imgout[(LY-0):(LY+1), x]=0
        } 
    }
    print(paste0("Calculated row: ", y, "/", DIMY))
}
imgout=imgout[1:(nrow(imgout)-3),]
imgout2=imgout2[1:(nrow(imgout2)-3),]

writeTIFF(imgout, "mariana_joydivision.tif",
          bits.per.sample=16, compression="LZW")
writeTIFF(imgout2, "mariana_joydivision2.tif",
          bits.per.sample=16, compression="LZW")


# Photoshop: resize down, colour, add border to proper dimensions
marco=readPNG("mariana_marco.png")
datos=readTIFF("mariana_final.tif")

DIMY=768
DIMX=600


# Create sequence
NFRAMES=1500  # 95.233s audio track at 19.90fps
for (frame in 1:(NFRAMES-1)) {
    frm=marco
    ROWEND=nrow(datos)-frame
    ROWINI=ROWEND-DIMY+1
    frm[201:(200+DIMY), 66:(65+DIMX), ]=datos[ROWINI:ROWEND,,]
    
    writePNG(frm, paste0("img", ifelse(frame<10, "000",
                                       ifelse(frame<100, "00",
                                              ifelse(frame<1000, "0", ""))), frame, ".png"))
    print(paste0(frame+1, "/", NFRAMES))
}
