# Colourize and contrast matrix images
# www.overfitting.net
# https://www.overfitting.net/


colourmatrix=function(img, colour=c(0.75, 0.5, 0.25), gamma=1) {
    # img must be a grayscale matrix
    
    # Gamma is applied before colouring
    img=replicate(3, img^(1/gamma))
    
    # Now the middle gray (0.5) becomes colour[]
    colour[colour<0.01]=0.01  # clip very low/high values
    colour[colour>0.99]=0.99
    colourgamma=log(0.5)/log(colour)
    for (i in 1:3) img[,,i]=img[,,i]^(1/colourgamma[i])

    return(img)
}

contrast=function(x, a=0.5, b=0.5, m=0, E=2) {
    if (a==0 | a==1) {  # if x=0 or x=1 the identity function is applied
        return(x)
    } else {
        f=log(b)/log(a)  # precalculate factor
        return(
            ifelse(  # vectorized ifelse
                x <= a,
                (m * x + (1 - m) * a * (x / a)^E) ^ f,
                (m * x + (1 - m) * (1 - (1 - a) * ((1 - x) / (1 - a))^E)) ^ f )
        )
    }
}
