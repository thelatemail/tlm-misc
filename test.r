## start a 3d scene function
site3d <- function(bg="#ffffff", linecol="#000000",
                    xlim=c(-15,15), ylim=c(-15,15), zlim=c(0,5),
                    theta=-45, phi=20, plot=TRUE) {
    
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

## TODO scale function



## create a simplified trans3d() function to take x/y/z as a single input
t3d <- function(x,pmat) do.call(trans3d, c(list(pmat=pmat), x) )


## start the plot and save the transformation matrix for x/y/z to x/y for plotting
tm <- site3d(theta=-45, phi=25)


## floor
flr <- data.frame(x=c(-5,5,5,-5), y=c(-5,-5,5,5), z=c(0,0,0,0))
polygon(t3d(flr, tm))

## wall sides - always rotated around (0,0), so if start at one edge, just rotate once
wall <- data.frame(x=c(-5,5,5,-5), y=c(-5,-5,-5,-5), z=c(0,0,3,3))
wallcol <- "#bf7e2400"

polygon(t3d(wall, tm), col=wallcol)
polygon(t3d(rotpts(wall, axis="z", angle=90), tm), col=wallcol)
polygon(t3d(rotpts(wall, axis="z", angle=-90), tm), col=wallcol)
polygon(t3d(rotpts(wall, axis="z", angle=180), tm), col=wallcol)


## shade awning
awncol <- "#839b1800"
awn <- list(
    main = data.frame(x=c(0,0,2,2,2,2), y=c(-2,2,2,2,-2,-2), z=c(1.5,1.5,0.5,0,0,0.5)),
    s1   = data.frame(x=c(0,2,2,0),     y=c(2,2,2,2),        z=c(1.5,0.5,0,0)),
    s2   = data.frame(x=c(0,2,2,0),     y=c(-2,-2,-2,-2),    z=c(1.5,0.5,0,0))
)

lapply(awn, function(x) polygon(t3d(transpts(rotpts(x, axis="z", angle=180), xa=-5, za=1.5), pmat=tm), col=awncol) )
lapply(awn, function(x) polygon(t3d(transpts(rotpts(x, axis="z", angle=-90), ya=-5, za=1.5), pmat=tm), col=awncol) )

## roof - floor + 3 metres
roofcol <- "#cccccc00"
polygon(t3d(transpts(flr, za=3), tm), col=roofcol)


## standard door
doorcol <- "#ffffff00"
door <- data.frame(x=c(-0.41,-0.41,0.41,0.41), y=c(0,0,0,0), z=c(0,2.04,2.04,0))
polygon(t3d(transpts(door, ya=-5, xa=-4), pmat=tm), col=doorcol)


## add some trees
treecol = "#119332"

treept <- data.frame(
    x = c(-0.15, -0.17, -0.4, -0.75, -0.81, -1.08, 
          -1.57, -1.5, -1.75, -1.58, -1.3, -1.15, -0.8, -0.48, -0.18, -0.05, 
          0, 0.05, 0.18, 0.48, 0.8, 1.15, 1.3, 1.58, 1.75, 1.5, 1.57, 1.08, 
          0.81, 0.75, 0.4, 0.17, 0.15),
    y = 0,
    z = c(0, 1.12, 1.24, 1.17, 1.4, 
          1.49, 1.58, 1.77, 1.95, 2.1, 2.29, 2.55, 2.67, 2.76, 2.86, 2.91, 
          2.93, 2.91, 2.86, 2.76, 2.67, 2.55, 2.29, 2.1, 1.95, 1.77, 1.58, 
          1.49, 1.4, 1.17, 1.24, 1.12, 0)
)

##tree <- lapply(c(0,45,90,135), function(a) rotpts(treept, angle=a, axis="z"))
##invisible(lapply(lapply(tree, transpts, ya=-9), function(x) polygon(t3d(x, pmat=tm), border=treecol)))


Map(
    function(x, xa, ya) {
        x <- lapply(x, transpts, xa=xa, ya=ya)
        lapply(x, function(x) polygon(t3d(x, pmat=tm), border=treecol))
    },
    list(tree),
    c(-5,-2,1,3),
    -11
)


   










## for drawing an object in a 2d plane and then
## rotating around an axis like a lathe to make it 3d
##dev.new()
##plot(NA, xlim=c(-3,3), ylim=c(0,3))
##abline(v=0, h=0, col="red")
##pts <- locator()

