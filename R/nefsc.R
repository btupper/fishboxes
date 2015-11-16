# nefsc.R

#' Retreive one or more shape files from NMFS's NEFSC public GIS products
#'
#' @export
#' @param what one or more names of ESRI shape files served by NEFCS via their
#'   ftp site.  Permitted names are BTS_Strata, EcoMon_Strata, Shellfish_Strata,
#'   Shrimp_Shrimp, Statistical_Areas or All
#' @param dest fully qualified path to destination directory, by default ".".
#'    If it doesn't exists then an attempt is made to create it.
#' @param ftp_url character ftp address, by default ftp://ftp.nefsc.noaa.gov/pub/gis
#' @param exts character vector of file extensions to download
#' @return named logical vector - one for each file downloaded
download_nefsc_gis <- function(what = 'Statistical_Areas',
   dest = '.',
   ftp_url = 'ftp://ftp.nefsc.noaa.gov/pub/gis',
   exts = c("dbf", "prj", "sbn", "sbx", "shp", "shp.xml","shx")){
   
   allowed <-  c('Statistical_Areas','BTS_Strata', 'EcoMon_Strata', 
      'Shellfish_Strata','Shrimp_Shrimp','All')
   
   thisone <- tolower(allowed) %in% tolower(what[1])
   if (!any(thisone)) stop("what must be one of ", paste(allowed, collapse = " "))
   wh <- allowed[thisone]
   
   if (!file.exists(dest[1])){
      stopifnot(dir.create(dest[1], recursive = TRUE))
   }
   
   ff <- paste(wh, exts, sep = ".") 
    
   for (f in ff){
      r <- try(download.file(file.path(ftp_url[1], f), 
         file.path(dest[1], f)) )
      if (inherits(r, 'try-error')){
         cat("error downloading", f, "\n")
      } else if (r != 0) {
         cat("error downloading", f, "with status", r, "\n")
      }
   }
   
   rff <- file.path(dest[1], ff)
   names(rff) <- ff
   sapply(rff, file.exists)
}
  