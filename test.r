####################
## Core functions ##
####################

## start a 3d scene function
site3d <- function(bg="#ffffff", linecol="#000000",
                    xlim=c(-15,15), ylim=c(-15,15), zlim=c(0,5),
                    theta=-45, phi=20, r=sqrt(3), plot=TRUE, box=FALSE,...) {
    
    par(mar=c(1,1,1,1), bg=bg)
    tm <- persp(matrix(rep(0,4), nrow=2),
                xlim=xlim, ylim=ylim, zlim=zlim,
                col="#00000000", border=NA, theta=theta, phi=phi, xlab="x", scale=FALSE, box=box, ...)
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


## functions for surface normal calculation

## crossproduct in 3D
vector.cross <- function(a, b) {
    if(length(a)!=3 || length(b)!=3){
        stop("Cross product is only defined for 3D vectors.");
    }
    i1 <- c(2,3,1)
    i2 <- c(3,1,2)
    return (a[i1]*b[i2] - a[i2]*b[i1])
}

## actual calculation of surface normal
normcalc <- function(x, chunk=FALSE) {
    if(!is.data.frame(x)) x <- as.data.frame(x)
    i <- x[2:3,] - x[c(1,1),]
    iA <- unlist(i[1,])
    iB <- unlist(i[2,])
    setNames(as.list(vector.cross(iA,iB)), c("x","y","z"))
}

## n-sided polygon / circle function (if n=high enough, approximates a circle)
npoly <- function(r, n) {
  angles <- seq(0, 2*pi, length.out=n)
  data.frame(x = r*cos(angles), y = r*sin(angles), z=0)
}

## create a simplified trans3d() function to take x/y/z as a single input
t3d <- function(x,pmat) do.call(trans3d, c(list(pmat=pmat), x) )


###########
## Setup ##
###########


## start the plot and save the transformation matrix for x/y/z to x/y for plotting
tm <- site3d(theta=-45, phi=25, r=0, box=TRUE)


##########
## Plot ##
##########

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
roof <- transpts(flr, za=3)
polygon(t3d(transpts(flr, za=3), tm), col=roofcol)


## standard door
doorcol <- "#ffffff00"
door <- data.frame(x=c(-0.41,-0.41,0.41,0.41), y=c(0,0,0,0), z=c(0,2.04,2.04,0))
polygon(t3d(transpts(door, ya=-5, xa=-4), pmat=tm), col=doorcol)


## add some trees
treecol = "#000000"

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

tree <- lapply(c(0,45,90,135), function(a) rotpts(treept, angle=a, axis="z"))

## add some slightly randomised trees
invisible(Map(
    function(x, xa, ya) {
        x <- lapply(x, transpts, xa=xa, ya=ya)
        ## trees aren't identical so jitter them
        x <- lapply(x, function(x) lapply(x, jitter, amount=0.1) )
        ## actually plot
        lapply(x, function(x) polygon(t3d(x, pmat=tm), border=treecol))
    },
    list(tree),
    jitter(c(-6,-3,2,4), 0.15),
    jitter(-11, 0.15)
))


## add a couple of outdoor dining tables
outtab <- list(
    top  = transpts(npoly(r=1, n=50), za=0.75),
    cntr = data.frame(x=c(0,0), y=c(0,0), z=c(0,0.75)),
    leg1 = data.frame(x=c(-0.5,0.5), y=c(0,0), z=c(0,0)),
    leg2 = data.frame(x=c(0,0), y=c(-0.5,0.5), z=c(0,0))
)

invisible(Map(
    function(x, xa, ya) {
        x <- lapply(x, transpts, xa=xa, ya=ya)
        lapply(x, function(x) polygon(t3d(x, pmat=tm)))
    },
    list(outtab),
    -8,
    c(4,1,-2,-5)
))







## test normals
##normcol <- "grey50"
##normlty <- 3
##
##lines(t3d(rbind(wall[1,], normcalc(wall)), tm), col=normcol, lty=normlty)
##lines(t3d(rbind.data.frame(
##    rotpts(wall[1,], axis="z", angle=90),
##    normcalc(rotpts(wall, axis="z", angle=90))),tm), col=normcol, lty=normlty)
##lines(t3d(rbind.data.frame(
##    rotpts(wall[1,], axis="z", angle=180),
##    normcalc(rotpts(wall, axis="z", angle=180))),tm), col=normcol, lty=normlty)
##lines(t3d(rbind.data.frame(
##    rotpts(wall[1,], axis="z", angle=270),
##    normcalc(rotpts(wall, axis="z", angle=270))),tm), col=normcol, lty=normlty)
##
##lines(t3d(rbind(flr[1,], normcalc(flr)), tm), col=normcol, lty=normlty)

light <- list(x=-15, y=-15, z=5)
points(t3d(light, tm), col="red", pch=19)

tri <- rbind(replace(wall[1,],,c(0,-5,0)), light, normcalc(wall))
polygon(t3d(cbind(tri[c(1,2)], z=0), tm), lty=2)

dtri <- dist(tri[1:2])
## horizontal angle
acos((dtri[1]^2 + dtri[2]^2 - dtri[3]^2) / (2 * dtri[1] * dtri[2])) * 180 / pi

## for totally flat lighting purely on the rotational angle, then
## the angle needs to be not based on the angle of point-to-point, but
## rather the angle between the PLANE the light sits on and PLANE of the surface
## This is currently point lighting, but that is okay


dtri <- dist(tri[c(2,3)])
polygon(t3d(cbind(tri[0], x=0, tri[2:3]), tm), lty=2)

## vertical angle
acos((dtri[1]^2 + dtri[2]^2 - dtri[3]^2) / (2 * dtri[1] * dtri[2])) * 180 / pi




           









## for drawing an object in a 2d plane and then
## rotating around an axis like a lathe to make it 3d
##dev.new()
##plot(NA, xlim=c(-3,3), ylim=c(0,3))
##abline(v=0, h=0, col="red")
##pts <- locator()




##Example:
##
##    Find a normal to the plane that passes through the points (1, 0, 2), (2, 3, 0), and (1, 2, 4) 
##
##Solution:

##    By direct substitution, A = ( 2, 3, 0 ) - ( 1, 0, 2) = ( 1, 3, -2 ) and B = (1, 2, 4) - (1, 0, 2) = (0, 2, 2),
##so their cross product n = ( 10, -2, 2 ).
##
##dat <- rbind(c(1, 0, 2), c(2, 3, 0), c(1, 2, 4))
##o <- dat[c(2,3),] - dat[c(1,1),]
##vector.cross(o[1,], o[2,])
##
##site3d(theta=70)
##points(t3d(unname(data.frame(dat)),tm), col=1:3)
##polygon(t3d(unname(data.frame(dat)),tm))
##lines(c(0.138,0.012),c(0.03159,-0.0027))
##
## the normal point seems to be perpendicular to the first point passed into the equation
## for a plane




