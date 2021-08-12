## test presentR functions.

source("presentR.R")
source("../drawInR/drawing_functions.R")

require('png')
code.1 <- readPNG("example_code.png")

## for code examples
source("../codeR/colorise_R.R")

## drawing source:
draw.source <- readLines("../drawInR/drawing_functions.R")

slides <- vector(mode='list', length=0)

slides[[1]] <- function(){
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    sc.text("Eating your own dogfood", 0.5, 0.5, cex=4)
}

slides[[2]] <- function(){
    x <- -20:20
    plot(x, x^3)
    points(x, x^2)
}

slides[[3]] <- function(){
    with(par(), print(usr))
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    sc.text("Pretty neat eh?", 0.5, 0.5, cex=4)
}

slides[[4]] <- function(){
    par(mfrow=c(2,2))
    slides[[1]]()
    slides[[2]]()
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    sc.text("and how about that?", 0.5, 0.5, cex=4)
    par(mfrow=c(1,1))
}

slides[[5]] <- function(){
    plot.new()
    plot.window(xlim=c(-1,1), ylim=c(-1,1), asp=1)
    a <- seq(0, 2*pi, length.out=100)
    polygon(cos(a), sin(a), col=rgb(0.3, 0, 0.6, 0.5))
}

slides[[6]] <- function(){
    sub.slides <- slides[c(5, 1, 2, 2)]
    org.par <- par(no.readonly=TRUE)
    par(mfrow=c(2,2))
    draw.slides(sub.slides, return.on.last=TRUE, preservePar=TRUE)
    par(org.par)
}

slides[[7]] <- slides[[5]]

slides[[8]] <- function(){
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    txt <- "A piece of text that we wish to put into a box of given dimensions\n"
    txt <- paste(txt, "And this should start from a newline or something like that", sep="")
    bx.txt <- boxText( c(25, 75, 25, 75), txt )
    NULL
}

hom.txt <- list("Homologues", list("Orthologoues", list("Common ancestor", "Arise due to speciation")),
            list("Paralogues", list("Homologoues within a species", "Arise as a result of gene duplication")),
            "Functional equivalents")
                               
slides[[9]] <- function(){
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 75, 75 )
    NULL
}

slides[[10]] <- function(){
    par(mfrow=c(1,2))
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 75, 75 )
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 75, 75 )
    NULL
}

slides[[11]] <- function(){
    par(mfrow=c(2,2))
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 75, 75 )
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 75, 75 )
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 75, 75 )
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 75, 75 )
    NULL
}

slides[[12]] <- function(){
    par(mfrow=c(1,4))
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 90, 75 )
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 90, 75 )
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 90, 75 )
    NULL
}

slides[[13]] <- function(){
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    bullet.list( hom.txt, 25, 75, max.w=75, max.h=60, bullet=FALSE, prefix.style=c('d', 'l', 'r'),
                prefix.sep=c(". ", ") ", ": "), line.spc=1.25 )
    NULL
}

slides[[14]] <- function(){
    plot.new()
    plot.window( xlim=c(0, 100), ylim=c(0, 100) )
    codes <- coloriseR(draw.source[3:22])
    draw.code( codes, 10, 90, cex=1, family='Source Code Pro Medium' )
##    draw.code( codes, 10, 90, cex=1, family='Hack' )
##    draw.code( codes, 10, 90, cex=1, family='PLRoman8' )
##    draw.code( codes, 10, 90, cex=1, family='PLSans' )
##    draw.code( codes, 10, 90, cex=1, family='Monospace' )
##    draw.code( codes, 10, 90, cex=1, family='Vn Nimbus Mono L' )
    NULL
}

slides[[15]] <- function(xlim=NULL, ylim=NULL){
    codes <- coloriseR(draw.source[3:22])
    if(is.null(xlim))
        xlim <- c(0,100)
    if(is.null(ylim))
        ylim <- c(0,100)
    print("xlim and ylim")
    print(xlim)
    print(ylim)
    plot.new()
    plot.window(xlim=xlim, ylim=ylim)
##    draw.code.box( codes, 10, 90, 80, 80, cex=1 )
##    draw.code.box( codes, 10, 90, 80, 80, cex=1, family='Source Code Pro Medium' )
    rect(10, 10, 90, 90)
    list(xlim=xlim, ylim=ylim)
}

    
##    draw.code( codes, 10, 90, cex=1, family='Hack' )
##    draw.code( codes, 10, 90, cex=1, family='PLRoman8' )
##    draw.code( codes, 10, 90, cex=1, family='PLSans' )
##    draw.code( codes, 10, 90, cex=1, family='Monospace' )
##    draw.code( codes, 10, 90, cex=1, family='Vn Nimbus Mono L' )


x11()

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
draw.slides(slides)

draw.slides(slides[15])

## I want to make bezier curves; these can make use of binomial coefficients; at least according to
## wikipedia. For this we can use choose()
## or alternatively my multinomial_coefficient function
dyn.load( "~/R/multinomial_coefficient/multinomial.so" )
choose.multi <- function(x){
    if(!is.matrix(x)){
        x <- as.integer(x)
        return( .Call("multinomial_coefficient", x) )
    }
    x <- matrix(as.integer(x), nrow=nrow(x))
    return(.Call("multinomial_coefficients", x))
}

## test the speed:
tmp <- rbind( 0:10000, 10000:0 )

system.time( choose.multi(tmp) )
##    user  system elapsed 
##   0.855   0.000   0.856 

system.time( choose( 10000, 0:10000 ))
##    user  system elapsed 
##   0.003   0.000   0.002

## so choose is much faster. That is good. Then we do not need any dependancy.

tmp <- rbind( c(50,50), c(50,100), c(100,100), c(100, 50))
t <- seq(0, 1, length.out = 10)


choose.multi( c(50, 50) )

## plot a raster image:
img <- array(data = runif(100*100*4, 0,1), dim=c(100, 100, 4))

## lets make this a bit more intersting
r <- sqrt( nrow(img)^2 + ncol(img)^2 ) / 2
r <- r * 0.8
for(y in 1:nrow(img)){
    img[y,,1] <- y/nrow(img)
    for(x in 1:ncol(img)){
        img[y,x,4] <- 1 - (sqrt( (ncol(img)/2-x)^2 + (nrow(img)/2-y)^2 ) / r)
        img[,x,3] <- y/ncol(img)
        img[y,x,2] <- 0
    }
}



for(y in 1:nrow(img))
    img[y,,1] <- y/nrow(img)

for(x in 1:ncol(img))
    img[,x,3] <- x / ncol(img)

img[ img < 0 ] <- 0

plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1), 'bg'=rgb(0,0,0), xaxs='i', yaxs='i')
##with(par(), rect(usr[1], usr[3], usr[2], usr[4], col=rgb(0.3,0,0.5)))

img[1:10,,4] <-  1
img[,1:10,4] <-  1
rasterImage(img, xleft=0, xright=0.5, ytop=0, ybottom=1, interpolate=FALSE)


plot.new()
img[,,4] <- 1
plot.window(xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i')
rasterImage(img, xleft=0.5, xright=1, ybottom=0.5, ytop=1, interpolate=FALSE)

plot.new()
plot.window(xlim=c(0,100), ylim=c(0,100))
placeImage(img, 10, 90)
with(par(), abline(h=c(90, 0)))
with(par(), abline(v=c(10, usr[4])))

plot.new()
plot.window(xlim=c(0,100), ylim=c(0,100))
placeImage(img, 0, 100)

plot.new()
plot.window(xlim=c(0,100), ylim=c(0,100))

placeImage(img, 20, 80, w=50)
placeImage(img, 40, 100, w=50)



plot.new()
plot.window(xlim=c(0,100), ylim=c(0,100))
placeImage(code.1, 20, 80, invert.y=FALSE)

plot.new()
plot.window(xlim=c(0,100), ylim=c(0,100))
placeImage(code.1, 20, 80, w=50, invert.y=FALSE)
abline(v=c(20, 70))
abline(h=80)

plot.new()
plot.window(xlim=c(0,100), ylim=c(0,100))
placeImage(code.1, 20, 80, h=50, invert.y=FALSE)
abline(h=c(80, 30))
abline(v=(20))




