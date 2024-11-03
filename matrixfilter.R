# Matrix vectorized convolution by kernel
# www.overfitting.net
# https://www.overfitting.net/


matrixfilter=function(img, kernel=matrix(c(0,0.25,0, 0.25,0,0.25, 0,0.25,0),
                                         nrow=3, ncol=3)) {
    KY=nrow(kernel); DIMY=nrow(img)
    KX=ncol(kernel); DIMX=ncol(img)
    
    imgoutpre=matrix(0, nrow=DIMY-KY+1, ncol=DIMX-KX+1)  # inner subset of img
    # Convolution: loop the kernel/vectorize the matrix
    for (j in 1:KY) {
        for (i in 1:KX) {
            if (kernel[j,i]) imgoutpre=imgoutpre +
                    img[(1+j-1):(j+DIMY-KY), (1+i-1):(i+DIMX-KX)]*kernel[j,i]
        }
    }
    
    imgout=img  # keep unfiltered img borders
    imgout[(1+(KY-1)/2):(DIMY-(KY-1)/2),  # overwrite filtered area
           (1+(KX-1)/2):(DIMX-(KX-1)/2)]=imgoutpre

    return (imgout)
}
