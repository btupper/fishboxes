# fishboxes
R tools for working with NMFS One Degree, Quarter Degree and Ten Minute Squares (ODS, QDS, and TMS) 

NMFS assigns fish reports to a grid system as shown below.

![NMFS Map](https://github.com/btupper/fishboxes/blob/master/inst/tenminutesquares_numbering.png)

#### Requirements

+ [R (>= 3.0)](http://cran.r-project.org/)
+ [sp](https://cran.r-project.org/web/packages/sp/index.html) package

#### Installation

It's easy to install using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```r
library(devtools)
install_github('btupper/fishboxes')
```

#### Usage

Here is a simple example that generates two Quarter Degree Squares within the 42N, 69W One Degree Box.  These are the lower left and upper right QDS.

```r
library(sp)
library(raster)
library(fishboxes)

P <- decode_polygons(x = c(42692, 42693))
P
# class       : SpatialPolygonsDataFrame 
# features    : 2 
# extent      : -70, -69, 42, 43  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
# variables   : 1
# names       :    ID 
# min values  : 42692 
# max values  : 42693  
```



The `show_fishboxes()` function shows how to produce a completed set of nested squares.

```r
show_fishboxes(x = 4269)
```

... that produces polygons as shown below.

![fishboxes](https://github.com/btupper/fishboxes/blob/master/inst/fishboxes.png)


The SpatialPolygon* objects produced by the can be used for extracting pixel values from a Raster* object.  First we draw some squares - this time using `ssp:spplot`.

```r
library(raster)
library(latticeExtra)

# read in an example raster that comes with the package
# The data is from [Ocean Color Web](http://oceancolor.gsfc.nasa.gov/cms/) 
# MODISA L3SMI daily monthly data selected at random.
   
filename <- system.file("NE-chlorophyl.grd", package = 'fishboxes')
r <- log10(raster::brick(filename))

# get a set of polygons (there are 6 - two pairs of three nested boxes)
P <- decode_polygons(x=c(426916, 42692, 4269, 387066, 38701, 3870))

# show them
sp::spplot(r) +
   sp::spplot(P, colorkey = FALSE, border = 'black', col.regions = NA)
```

![raster](https://github.com/btupper/fishboxes/blob/master/inst/NE-chlorophyl.png)

Now we can make a One Degree Square and its 4 Quarter Degree Squares.  We'll find the sum of the raster pixels within each and compare them.  If all goes well, the sum of the 4 quarters will exactly equal the sum across the entore One Degree Square.

```
# make a One Degree Square
P <- decode_polygons(x = 4269)

# and it's 4 Quarter Degree Squares
P4 <- lapply(paste0('4269', 1:4), decode_polygons)

# get the raster mean from each
S <- raster::extract(r, P, fun = sum, na.rm = TRUE)
S4 <- sapply(P4, function(x) raster::extract(r, x, fun = sum, na.rm = TRUE))

# these is no difference! (S is a matrix, we need the first element at [1,1])
identical(S[1,1], sum(S4))
# [1] TRUE
```



