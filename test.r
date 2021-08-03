## start a 3d scene function
site3d <- function(bg="#ffffff", linecol="#000000",
                    xlim=c(-15,15), ylim=c(-15,15), zlim=c(0,5),
                    theta=-45, phi=30, plot=TRUE) {
    
    par(mar=c(1,1,1,1), bg=bg)
    tm <- persp(matrix(rep(0,4), nrow=2),
                xlim=xlim, ylim=ylim, zlim=zlim,
                col="#00000000", border=NA, theta=theta, phi=phi, xlab="x", scale=FALSE, box=FALSE)
    if (!plot) {
        dev.off()
        invisible(return(tm))
    }

    ##return the transformation matrix to add any other points
    invisible(tm)
}


## rotate around axis function
rotpts <- function(x, axis, angle) {

    angle <- angle * pi/180
    
    if(axis == "x") {
        rmat <- matrix(c(1,0,0,0,cos(angle),sin(angle),0,-sin(angle),cos(angle)), nrow=3)
    } else
        if(axis == "y") {
            rmat <- matrix(c(cos(angle),0,-sin(angle),0,1,0,sin(angle),0,cos(angle)), nrow=3)
        } else
            if(axis == "z") {
                rmat <- matrix(c(cos(angle),sin(angle),0,-sin(angle),cos(angle),0,0,0,1), nrow=3)
            }

    res <- t(rmat %*% t(x))
    split(res, c("x","y","z")[col(res)])
}


## translate (move) function
transpts <- function(x, xa=0, ya=0, za=0) {
    x$x <- x$x + xa
    x$y <- x$y + ya
    x$z <- x$z + za
    x
}

## TODO - scale function??

## create a simplified trans3d() function to take x/y/z as a single input
t3d <- function(x,pmat) do.call(trans3d, c(list(pmat=pmat), x) )


## start the plot and save the transformation matrix for x/y/z to x/y for plotting
tm <- site3d()


## floor
flr <- data.frame(x=c(-5,5,5,-5), y=c(-5,-5,5,5), z=c(0,0,0,0))
polygon(t3d(flr, tm))

## roof - floor + 3 metres
polygon(t3d(transpts(flr, za=3), tm))

## wall sides - always rotated around (0,0), so if start at one edge, just rotate once
wall <- data.frame(x=c(-5,5,5,-5), y=c(-5,-5,-5,-5), z=c(0,0,3,3))
polygon(t3d(wall, tm))
polygon(t3d(rotpts(wall, axis="z", angle=90), tm))
polygon(t3d(rotpts(wall, axis="z", angle=-90), tm))
polygon(t3d(rotpts(wall, axis="z", angle=180), tm))


## shade awning
awn <- list(
    main = data.frame(x=c(0,0,2,2,2,2), y=c(-2,2,2,2,-2,-2), z=c(1.5,1.5,0.5,0,0,0.5)),
    s1   = data.frame(x=c(0,2,2,0),     y=c(2,2,2,2),        z=c(1.5,0.5,0,0)),
    s2   = data.frame(x=c(0,2,2,0),     y=c(-2,-2,-2,-2),    z=c(1.5,0.5,0,0))
)

lapply(awn, function(x) polygon(t3d(transpts(rotpts(x, axis="z", angle=180), xa=-5, za=1.5), pmat=tm)) )
lapply(awn, function(x) polygon(t3d(transpts(rotpts(x, axis="z", angle=-90),  ya=-5, za=1.5), pmat=tm)) )


## standard door
door <- data.frame(x=c(-0.41,-0.41,0.41,0.41), y=c(0,0,0,0), z=c(0,2.04,2.04,0))

polygon(t3d(transpts(door, ya=-5, xa=-4), pmat=tm))






