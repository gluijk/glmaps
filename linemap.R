# Line map from a telecommunications table
# www.overfitting.net
# https://www.overfitting.net/

library(data.table)
library(tiff)


# Por Carlos Gil Bellosta
indices.drawline=function(x0, y0, x1, y1) {
    x0=round(x0); y0=round(y0)
    x1=round(x1); y1=round(y1)
    
    if (y0 == y1) return(cbind(x0:x1, y0)) # Recta de m=0 o un punto
    if (abs(x1 - x0) >= abs(y1 - y0)) { # Recta de 0 < |m| <= 1
        m = (y1 - y0) / (x1 - x0)
        cbind(x0:x1, round(y0 + m * ((x0:x1) - x0)))
    } else indices.drawline(y0, x0, y1, x1)[, 2:1]  # Recta de |m| > 1
    # Llamada traspuesta recursiva y traspuesta
}

linemap=function(DT, WIDTH=1920, HEIGHT=1080) {
    # DT: dataframe with 5 columns (x0, y0, x1, y1, num)
    # WIDTH x HEIGHT: output matrix size
    # x0, x1 must be in the 1..WIDTH range
    # y0, y1 must be in the 1..HEIGHT range

    MDT=as.matrix(DT)  # Loop data as a matrix is faster
    img=array(0, c(WIDTH, HEIGHT))  # Destination image
    for (k in 1:nrow(MDT)) {
        i=indices.drawline(MDT[k,1], MDT[k,2], MDT[k,3], MDT[k,4])
        img[i]=img[i]+MDT[k,5]
    }

    return (t(img[,ncol(img):1]))  # transpose rows/columns
}


####################################################
# BUILD LINE MAP FROM DT

# Read calls as data.table
DT=fread("llamadasmes_masde1llamada.txt", header=TRUE, sep="\t")
# Group (x0,y0)-(x1,y1) pairs to minimize rows
DT=DT[, list(sumallamadas=sum(sumallamadas)), by=.(x0,y0,x1,y1)]

img=linemap(DT)
# Time difference of 25.77944 secs -> ~1 million rows, ~40K lines/s

writeTIFF((img/max(img))^0.3, "vodafone.tif", bits.per.sample = 16)
