# fishbox.R

#' A toy example that shows the boxes within the One Degree Square '4269'
#' 
#' @export
#' @param x one 4 digit code
show_fishboxes <- function(x = 4269){
   
   opar <- par(no.readonly = TRUE)
   on.exit(par(opar))
   
   lat <- as.numeric(substring(x[1], 1,2))
   lon <- as.numeric(substring(x[1], 3,4))
   
   z <- expand.grid((lat-1):(lat+1), (lon-1):(lon+1))
   z <- paste0(z[,1], z[,2])
   
   P1 <- decode_polygons(z)
   
   P4 <- decode_polygons(paste0(x[1], 1:4))
   
   P36 <- decode_polygons(paste0(x[1], rep(1:6, times = 6), rep(1:6, each = 6)))

   C36 <- decode_polygons(paste0(x[1], rep(1:6, times = 6), rep(1:6, each = 6)), 
      form = 'center')
   c36names <- substring(rownames(C36@data), 5,6)
   
   par(las = 2, mar = c(4,4,1,1))
   sp::plot(P1, axes = TRUE, asp = 1)
   lines(P4, col = 'red', lwd=3)
   lines(P36, col = 'blue', lty = 'dashed')
   text(sp::coordinates(C36), c36names, cex = 0.7, col = 'blue')
   
}



#' Retrieve Polygons class objects derived from 4, 5 or 6 digit codes used to 
#' define One Degree, Quarter Degree or Ten Minute Squares (ODS, QDS, TMS).
#'
#' @export
#' @param x one or more numeric or character 4, 5 or 6 digit codes
#' @param crs a CRS class coordinate reference system object
#' @param asDataFrame logical, if TRUE return a SpatialPolygonsDataFrame
#' @param ... further arguments for \code{decode_id}
#' @return  sp::SpatialPolygons or sp::SpatialPolygonsDataFrame class object
decode_polygons <- function(x = c(426916, 42692, 4269, 387066, 38701, 3870), 
   crs = sp::CRS("+proj=longlat +datum=WGS84"), asDataFrame = TRUE, ...){
   xx <- decode_id(x, form = 'bounds', closed = TRUE)
   xx <- lapply(names(xx), function(id) sp::Polygons(list(sp::Polygon(xx[[id]])), id) )
   P <- sp::SpatialPolygons(xx, 1:length(xx), proj4string = crs)
   if (asDataFrame) sp::SpatialPolygonsDataFrame(P, 
      data = data.frame(ID = names(P), row.names = names(P)))
} # decode_polygons


#' Given a quadrisphere code, retrieve the multipliers for lat and lon
#' 
#' @export
#' @param what one of "nw", "sw", "ne", "se"
#' @return a 2 element vector of lat and lon multipliers
quadm <- function(what){
   w <- match.arg(tolower(what[1]), c("nw", "sw", "ne", "se"))
   switch(w,
      'nw' = c(lat=1,lon=-1),
      'ne' = c(lat=1,lon=1),
      'sw' = c(lat=-1,lon=-1),
      'se' = c(lat=-1,lon=1))
} # quadrisphere_mult

#' Given a 4, 5 or 6 digit code, retrieve the bounding coordinates
#' or center of a fish box.
#'
#' @export
#' @param x numeric or character one or more six digit TMS codes
#' @param quadrisphere character identifier where 'nw' is northern western quadrisphere
#' @param form character either 'bounds' or 'center'
#' @param closed logical if form is 'bounds' then close the bounds
#' @param a list of 2xn matrices where n = 1 for 'center' and n = 4 or 5 for 'bounds'
decode_id <- function(x = c(426916, 42692, 4269), 
   form = 'bounds', closed = TRUE, 
   quadrisphere = 'nw'){
   
   quad <- quadm(quadrisphere)
   form <- match.arg(form[1], c("bounds", "center"))
   
    
   # first we split into character length objects
   n <- nchar(x)
   nmx <- names(x) <- x
   nf <- factor(n)
   xx <- split(x, nf)
   
   for (n in names(xx)){
      xx[[n]] <- switch(n,
         '6' = lapply(xx[[n]], decode_6, quad = quad, form = form, closed = closed),
         '5' = lapply(xx[[n]], decode_5, quad = quad, form = form, closed = closed),
         '4' = lapply(xx[[n]], decode_4, quad = quad, form = form, closed = closed),
         NULL)
   } # n-loop
   z <- unlist(xx, recursive = FALSE, use.names = FALSE)
   names(z) <- x
   invisible(z)
}


#' Given a 4 digit code, retrieve the bounding coordinates
#' or center of a One Degree Square. One Degree Squares (ODS) are defined degree 
#' by latitude and degree longitude
#'
#' @export
#' @param x numeric or character one or more 4 digit ODS codes
#' @param quadrisphere character identifier where 'nw' is northern western quadrisphere
#' @param form character either 'bounds' or 'center'
#' @param closed logical if form is 'bounds' then close the bounds
#' @param a list of 2xn matrices where n = 1 for 'center' and n = 4 or 5 for 'bounds'
decode_4 <- function(x = 4269, 
   quad = c(lat=1,lon=1), form = 'bounds', closed = TRUE){
   
   mx <- quad[['lon']]
   my <- quad[['lat']]
   wx <- 1 * mx
   wy <- 1 * my
   lat <- as.numeric(substring(x,1,2)) * my
   lon <- as.numeric(substring(x, 3,4)) * mx
   switch(form,
      'bounds' = if (closed) {
            matrix(c(lon, lon, lon+wx, lon+wx, lon, 
               lat, lat+wy, lat+wy, lat, lat), 
               ncol = 2, dimnames = list(NULL, c('x', 'y')))
         } else {
            matrix(c(lon, lon, lon+wx, lon+wx,  
               lat, lat+wy, lat+wy, lat), 
               ncol = 2, dimnames = list(NULL, c('x', 'y')))
         },
      'center' = matrix(c(lon + wx/2, lat + wy/2), 
                  ncol = 2, dimnames = list(NULL, c('x', 'y')))
   )
} # decode_4



#' Given a 5 digit code, retrieve the bounding coordinates
#' or center of a Quarter Degree Square. Quarter Degree Squares (QDS) use degree 
#' Latitude and degree Longitude and quarter of the full degree square. 
#' The lower right quarter is 1, lower left 2, upper right 3 and upper left 4.
#'
#' @export
#' @param x numeric or character one or more 5 digit QDS codes
#' @param quadrisphere character identifier where 'nw' is northern western quadrisphere
#' @param form character either 'bounds' or 'center'
#' @param closed logical if form is 'bounds' then close the bounds
#' @param a list of 2xn matrices where n = 1 for 'center' and n = 4 or 5 for 'bounds'
decode_5 <- function(x = 42692, 
   quad = c(lat=1,lon=1), form = 'bounds', closed = TRUE){
   
   mx <- quad[['lon']]
   my <- quad[['lat']]
   # lower right defines the boxes, so we add for lat and subtract for lon
   qlen <- 2
   qw <- 1/qlen
   qwx <- qw * mx
   qwy <- qw * my
   lat <- as.numeric(substring(x,1,2)) * my
   lon <- as.numeric(substring(x, 3,4)) * mx
   qnum <- substring(x, 5,5)
   
   # move the lower right corner as required
   lat <- lat + switch(qnum,
      '3' = qwy, # upper row
      '4' = qwy, # upper row
      0.0)
   lon <- lon + switch(qnum,
      '2' = qwx, # left side
      '4' = qwx, # left side
      0.0)
   switch(form,
      'bounds' = if (closed) {
            matrix(c(lon, lon, lon+qwx,lon+qwx, lon, 
               lat, lat+qwy, lat+qwy, lat, lat), 
               ncol = 2, dimnames = list(NULL, c('x', 'y')))
         } else {
            matrix(c(lon, lon, lon+qwx,lon+qwx,  
               lat, lat+qwy, lat+qwy, lat), 
               ncol = 2, dimnames = list(NULL, c('x', 'y')))
         },
      'center' = matrix(c(lon + qwx/2, lat + qwy/2), 
                  ncol = 2, dimnames = list(NULL, c('x', 'y')))
   )
} # decode_5
      
      
#' Given a 6 digit code, retrieve the bounding coordinates
#' or center of a Ten Minute Square.  Ten Minute Squares (TMS) are labeled with
#' a sequence of 6 digits, LaLoCR, where La is whole degrees latitude, Lo is whole
#' degrees longitude, R is row and C is column.  Each row and column represents
#' 1/6th of a degree or 10 minutes.   Rows are number 1 through 6 north to south
#' while columns are numbered 1 through 6 west to east.
#'
#' @export
#' @param x numeric or character one or more six digit TMS codes
#' @param quadrisphere character identifier where 'nw' is northern western quadrisphere
#' @param form character either 'bounds' or 'center'
#' @param closed logical if form is 'bounds' then close the bounds
#' @param a list of 2xn matrices where n = 1 for 'center' and n = 4 or 5 for 'bounds'           
decode_6 <- function(x = 426916, 
   quad = c(lat=1,lon=1), form = 'bounds', closed = TRUE){

   mx <- quad[['lon']]
   my <- quad[['lat']]
   # upper left defines the box
   tmslen <- 6
   w <- 1/tmslen
   wx <- w * mx
   wy <- w * my
   tmsrows <- seq(from = 1 - w, to = 0, length.out = tmslen)
   tmscols <- seq(from = -1 + w, to = 0, length.out = tmslen)
   names(tmsrows) <- names(tmscols) <- 1:tmslen

   lat <- as.numeric(substring(x,1,2)) * my
   lon <- as.numeric(substring(x, 3,4)) * mx
   col <- tmscols[substring(x, 5,5)]
   row <- tmsrows[substring(x, 6,6)]
   lon <- lon + col
   lat <- lat + row
   switch(form,
      'bounds' = if (closed) {
            matrix(c(lon, lon, lon+wx,lon+wx, lon, 
               lat, lat+wy, lat+wy, lat, lat), 
               ncol = 2, dimnames = list(NULL, c('x', 'y')))
         } else {
            matrix(c(lon, lon, lon+wx,lon+wx,  
               lat, lat+wy, lat+wy, lat), 
               ncol = 2, dimnames = list(NULL, c('x', 'y')))
         },
      'center' = matrix(c(lon + wx/2, lat + wy/2), 
                  ncol = 2, dimnames = list(NULL, c('x', 'y')))
      )

}  # decode_6
