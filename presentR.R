## A set of functions to make it possible to use R as a presentation software
## This should only include very basic functions that handle slide transition
## and mouse and keyboard interaction.

## The presentation will be run by calling a function that takes as its argument
## a list of functions. This function will call each function as a result of keyboard
## and mouse input.

## The presentR functions should not include specialised drawing functions; these
## should instead be obtained from independent packages / sources.

## a function for moving the plot around.
## not very useful, but it demonstrates some of the principles for handling user input.
## with inspiration taken from:
## http://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/getGraphicsEvent.html
## written by Duncan Murdoch
## (though I may modify it for playing arount with). ## to use this
## 1. savepar <- par()
## 2. dragplot( ) # with something that can be plotted
## press q to quit
## 3. getGraphicsEvent()  ## reset the handlers to null..  
## 4. par(savepar)
## dragplot <- function(..., xlim=NULL, ylim=NULL, xaxs="r", yaxs="r") {
##     plot(..., xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs)
##     startx <- NULL
##     starty <- NULL
##     usr <- NULL

##     devset <- function()
##         if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
        
##     dragmousedown <- function(buttons, x, y) {
##         startx <<- x
##         starty <<- y
##         devset()
##         usr <<- par("usr")
##         eventEnv$onMouseMove <- dragmousemove
##         NULL
##     }
    
##     dragmousemove <- function(buttons, x, y) {
##         devset()
##         deltax <- diff(grconvertX(c(startx,x), "ndc", "user"))
##         deltay <- diff(grconvertY(c(starty,y), "ndc", "user"))
##         plot(..., xlim=usr[1:2]-deltax, xaxs="i",
##                   ylim=usr[3:4]-deltay, yaxs="i")
##         NULL
##     }
    
##     mouseup <- function(buttons, x, y) {    
##     	eventEnv$onMouseMove <- NULL
##     }	
        
##     keydown <- function(key) {
##         if (key == "q") return(invisible(1))
##         eventEnv$onMouseMove <- NULL
##         NULL
##     }
    
##     setGraphicsEventHandlers(prompt="Click and drag, hit q to quit",
##                      onMouseDown = dragmousedown,
##                      onMouseUp = mouseup,
##                      onKeybd = keydown)
##     eventEnv <- getGraphicsEventEnv()
## }

new.slide <- function(xlim=c(0,100), ylim=c(0,100), clear=TRUE, xaxs='i', yaxs=xaxs, ...){
    if(clear)
        plot.new()
    plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, ...)
}

## ret.keys define a set of keys which will cause the function to return
## the key
## If the prefix key is pressed then collect keypresses until the suffix.key is
## entered. This allows several keys to be pressed; we could consider printing
## these to the screen?
## if key in mouse.keys is pressed, will return when nrow(eventEnv$m.points) == 2
## allowing the calling function to modify the plot.
## 'm', 'e' and 'r' are standard codes that should indicate, 'move', 'enlarge', 'reduce'
get.input <- function(ret.keys=c(' ', 'n', 'Right', 'p', 'Left'), mouse.keys=c('m', 'e', 'r'), state=NULL, point=NA,
                      prefix.key='a', suffix.key='\r', prompt="Waiting for input",
                      func=NULL, xlim=NULL, ylim=NULL){
    ## return if not X11 or windows
    dev.type <- names(dev.cur())
    if(!grepl("X11|windows", dev.type)){
        return(list("gr"=NULL, 'mp'=NULL, 'state'=NULL))
    }
    devset <- function()
        if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)

    mouse.down <- function(buttons, x, y){
        new.point <- c(grconvertX(x, "ndc", "user"), grconvertY(y, "ndc", "user"))
        eventEnv$m.points <- rbind(eventEnv$m.points, new.point)
        eventEnv$onMouseMove = mouse.move
        NULL
    }

    mouse.up <- function(buttons, x, y){
        eventEnv$onMouseMove = NULL
        eventEnv$state = NULL
    }

    mouse.move <- function(buttons, x, y){
        new.point <- c(grconvertX(x, "ndc", "user"), grconvertY(y, "ndc", "user"))
        l <- nrow(eventEnv$m.points)
        eventEnv$m.points <- rbind(eventEnv$m.points, new.point)
        l <- l:(l + 1)
        ## Trying to move the plot by replotting it at each mouse move
        ## event is far too slow in R. We have to do something else to allow
        ## panning and zooming.
        ## if(!is.null( eventEnv$state ) && eventEnv$state == 'm' && l > 1
        ##    && !is.null(eventEnv$xlim) && !is.null(eventEnv$ylim) && !is.null(func)){
        ##     delta <- eventEnv$m.points[l[1],] - eventEnv$m.points[l[2],]
        ##     eventEnv$xlim <- eventEnv$xlim + delta[1]
        ##     eventEnv$ylim <- eventEnv$ylim + delta[2]
        ##     func(xlim=eventEnv$xlim, ylim=eventEnv$ylim)
        ##     return(NULL)
        ## }
        lines( eventEnv$m.points[l,'x'], eventEnv$m.points[l,'y'] )
        NULL
    }

    keydown <- function(key){
        if(key == suffix.key){
            return(eventEnv$word)
        }
        if(!is.null(eventEnv$word)){
            eventEnv$word <- paste(eventEnv$word, key, sep="")
            return(NULL)
        }
        if(key == prefix.key){
            eventEnv$word = ""
            return(NULL)
        }
        if(key %in% mouse.keys){
            eventEnv$state = key
            return(NULL)
        }
        if(key %in% ret.keys)
            return(invisible(key))
        if(key == 'q') return(invisible('q'))
        if(key == 'c') return(invisible('c'))
        NULL
    }
    setGraphicsEventHandlers(prompt="left and right arrows to go to next",
                             onMouseDown=mouse.down,
                             onMouseUp=mouse.up,
                             onKeybd=keydown)
    eventEnv <- getGraphicsEventEnv()
    eventEnv$m.points <- matrix(point, ncol=2);
    colnames(eventEnv$m.points) <- c('x', 'y')
    eventEnv$state <- state
    eventEnv$word <- NULL
    eventEnv$xlim <- xlim
    eventEnv$ylim <- ylim
    ##    gr.ret <- getGraphicsEvent(prompt=prompt)
    ##    print(prompt)
    gr.ret <- getGraphicsEvent()
    list("gr"=gr.ret, 'mp'=eventEnv$m.points, 'state'=eventEnv$state)
}

## call this function to to draw things
## draw.f should be a function that draws the slide
## draw.f may return a list whose members can change the behaviour
## of the next slide (eg, preserve.par)
draw.screen <- function(draw.f, prompt="Waiting for input", slide.i=0, ...){
    if(slide.i > 0)
        dr.ret <- draw.f(slide.i=slide.i, ...)
    else
        dr.ret <- draw.f(...);
    ## for panning and zooming we need to have xlim and ylim defined and these must be settable.
    ## if they are returned, then we may assume that they are arguments to the function;
    if(!is.null(dr.ret$xlim) && !is.null(dr.ret$ylim)){
        gr.ret <- get.input(ret.keys=c("Right", "Left", "n", " ", "p", "c", "q"), prefix.key='a', prompt=prompt,
                            func=draw.f, xlim=dr.ret$xlim, ylim=dr.ret$xlim)
    }else{
        gr.ret <- get.input(ret.keys=c("Right", "Left", "n", " ", "p", "c", "q"), prefix.key='a', prompt=prompt)
    }
    c(gr.ret, dr.ret)
}

## slides should be a list of functions that do something cool.
draw.slides <- function(slds, return.on.last=FALSE, preservePar=FALSE, slide.i=FALSE, ...){
    i <- 1
    org.par <- par(no.readonly=TRUE)
    while(i <= length(slds)){
        print(paste( "Slide number:", i ))
        if(slide.i)
            ctl <- draw.screen( slds[[i]], prompt=paste("Slide", i), slide.i=i, ... )
        else
            ctl <- draw.screen( slds[[i]], prompt=paste("Slide", i), ... )
        if(!preservePar &&  (is.null(ctl$dr) || is.null(ctl$dr$preservePar)))
            par(org.par)
        j <- ctl$gr
        if(is.null(j)){
            i <- i + 1
            next
        }
        if(j %in% c("Right", "n", " "))
            i <- i + 1
        if(j %in% c("Left", "p"))
            i <- i - 1
        if(sum(grepl("^[0-9]+$", j)))
            i <- as.numeric(j)
        if(j == 'q')
            break
        if(j == 'c')
            next
        if(i > length(slds) && return.on.last)
            break
        i <- ifelse(i < 1, 1, i)
        i <- ifelse(i > length(slds), length(slds), i)
    }
}

    
