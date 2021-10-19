
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

t3d <- function(x,pmat) do.call(trans3d, c(list(pmat=pmat), x) )

tm <- site3d(theta=-50, phi=-20, r=0, zlim=c(-5,5), bg="#5e859e", box=FALSE)


## n-sided polygon / circle function (if n=high enough, approximates a circle)
npoly <- function(r, n) {
  angles <- seq(0, 2*pi, length.out=n)
  data.frame(x = r*cos(angles), y = r*sin(angles), z=0)
}

## translate (move) function
transpts <- function(x, xa=0, ya=0, za=0) {
    x$x <- x$x + xa
    x$y <- x$y + ya
    x$z <- x$z + za
    x
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
    data.frame(split(res, c("x","y","z")[col(res)]))
}


## fill colour
tricol <- "#0A307E88"


## flat middle bit
mid <- data.frame(x=c(-5,-5,5,5), y=c(-5,5,5,-5), z=0)
polygon(t3d(mid, pmat=tm), col=tricol)


## drill bit
polygon(t3d(data.frame(x=c(0), y=c(0), z=c(-7,-20)), pmat=tm), border="red", lwd=3)


## define triangles!
tri1 <- data.frame(x=c(-5,0,5),  y=c(-5,0,-5), z=c(0.1,7,0.1))
tri2 <- data.frame(x=c(-5,0,5),  y=c(5,0,5),   z=c(0.1,7,0.1))
tri3 <- data.frame(x=c(5,0,5),   y=c(-5,0,5),  z=c(0.1,7,0.1))
tri4 <- data.frame(x=c(-5,0,-5), y=c(5,0,-5),  z=c(0.1,7,0.1))

tri5 <- data.frame(x=c(-5,0,5),  y=c(-5,0,-5), z=c(0,-7,0))
tri6 <- data.frame(x=c(-5,0,5),  y=c(5,0,5),   z=c(0,-7,0))
tri7 <- data.frame(x=c(5,0,5),   y=c(-5,0,5),  z=c(0,-7,0))
tri8 <- data.frame(x=c(-5,0,-5), y=c(5,0,-5),  z=c(0,-7,0))


## plot!
polygon(t3d(tri1, pmat=tm), col=tricol, lwd=2)
polygon(t3d(tri2, pmat=tm), col=tricol, lwd=2)
polygon(t3d(tri3, pmat=tm), col=tricol, lwd=2)
polygon(t3d(tri4, pmat=tm), col=tricol, lwd=2)
polygon(t3d(tri5, pmat=tm), col=tricol, lwd=2)
polygon(t3d(tri6, pmat=tm), col=tricol, lwd=2)
polygon(t3d(tri7, pmat=tm), col=tricol, lwd=2)
polygon(t3d(tri8, pmat=tm), col=tricol, lwd=2)




## at field, 7 points, 6 sided
atfield <- npoly(r=10, n=7) |>
rotpts(axis="z", angle=120) |>
rotpts(axis="x", angle=0) |>
rotpts(axis="y", angle=90) |>
transpts(xa=-8) |>
t3d(pmat=tm)

polygon(atfield, border="#a8909b", lwd=5)
polygon(atfield, border="#c0b0b7", lwd=3)
polygon(atfield, border="#e7dbe1", lwd=2)
polygon(atfield, border="#ffffff33", lwd=1)
polygon(atfield, border=NA, col="#d4d0e730")


## cannon shot
lines(t3d(data.frame(x=c(-30,-8,-12), y=c(0,0,-13), z=0), pmat=tm), lwd=5, col="#ffffff")

## need to spline the above to make it smooth

