#' @title Mark occurrences with the municipality informed different from the coordinate
#' @name filt
#'
#' @description A function to mark occurrences with the municipality informed different from the coordinate.
#'
#' @param pts data.frame. Table with points of occurrence, including the municipalities informed on the label. the data frame must contain the following columns in this order: "species", "lon", "lat", "municipality", "adm1"
#' @param inverted logical. If TRUE (default), it will check if longitude and latitude are changed. For now this option may be slow when there are many records of occurrence.
#' @param shape.municipios It can be a shape of municipalities of Brazil in format "SpatialPolygonsDataFrame". If it is NULL, the Brazilian shape will be used available on the IBGE website.
#'
#' @details
#'
#' @return a data frame
#'
#' @author Diogo S. B. Rocha
#'
#'
#' @examples
#'
#' filt(euterpe)
#'
#' @import raster
#' @import dismo
#' @import maptools
#' @import rgdal
#' @import sp
#' @import textclean
#'
#' @export

filt <- function(pts,
                 inverted = TRUE,
                 shape.municipios = NULL,
                 name.col = c("nome", "stateProvince")) { #poderia ser name.col = c("NOME", "NOMEUF")
  if (class(pts) != "data.frame" & class(pts) != "matrix") {
    stop("Invalid format. Please enter 'data.frame' or 'matrix'.")
  }

  if (ncol(pts) != 6) {
    stop(
      "The 'pts' argument must have six columns: 'id', 'species', 'lon', 'lat', 'municipality', 'UF'"
    )
  }
   if (any(pts[, "lat"] > 90) |
       any(pts[, "lat"] < (-90))) {
       stop(
           "There is no latitude greater than 90° or less than -90°"
       )
       }
   if (any(pts[, "lon"] > 90) |
       any(pts[, "lon"] < (-90))) {
       stop(
           "There is no latitude greater than 90° or less than -90°"
       )
       }


  #pts <- na.exclude(pts)

  coordinates(pts) <- ~ lon + lat

  if (is.null(shape.municipios)) {
      br_mun
  }
  if (is.null(shape.municipios) == FALSE &
      class(shape.municipios) == "SpatialPolygonsDataFrame") {
    br_mun <- shape.municipios
  }
    if (is.null(proj4string(br_mun))) {
        proj4string(br_mun) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0, 0, 0")
  }

  if (is.na(proj4string(pts))) {
      proj4string(pts) <- proj4string(br_mun)
  }
  pts1 <- as.data.frame(pts)
  muni_shape  <- over(pts, br_mun)[, name.col]
  pts1  <- cbind(pts1, muni_shape)

  pts1 <- pts1 %>% mutate_at(5:ncol(pts1), .funs = function(x) ifelse(!is.na(x), textclean::replace_non_ascii(tolower(x)), x))

  
  pts1$filt <- "NA"

  for (i in 1:nrow(pts1)) {
    if (is.na(pts1$municipality[i])) {
      pts1[i, "filt"] <- "original municipality not informed"
    }
    if(is.na(pts1$municipality[i]) == F) {
      if (is.na(pts1$municipality[i] == pts1$nome[i]) == TRUE) {
        pts1[i, "filt"] = "outside Brazil"
        pts1[i, "nome"] = "outside Brazil"
        pts1[i, "stateProvince"] = "outside Brazil"
      }
      if ((pts1$municipality[i] == pts1$nome[i]) == FALSE) {
        pts1[i, "filt"] = "outside municipality"
      }
      if ((pts1[i, "nome"] == "outside Brazil")) {
        pts1[i, "filt"] = "outside Brazil"
        pts1[i, "nome"] = "not found"
        pts1[i, "stateProvince"] = "not found"
      }
    }
  }

  pts2 <- pts1[, -c(6, 8)]
  names(pts2) = c("ID",
                  "species",
                  "lon",
                  "lat",
                  "county.orig" ,
                  "county.shape",
                  "status")

  if (inverted == T) {
    for(i in 1:dim(pts2)[1]){
      if(pts2$status[i] != "Ok"){
        valor1 = pts2[i, c("lon", "lat")]
        coordinates(valor1) = ~lat+lon
        proj4string(valor1) = proj4string(br_mun)
        muni = as.vector(over(valor1, br_mun)[, 'nome'])
        if(is.na(muni)==FALSE){
          muni <- textclean::replace_non_ascii(muni)

          if(pts2$county.orig[i] == muni){
            pts2$status[i] = "inverted coordinates"
          }
        }
      }
    }
  }

  print(table(pts2$status))
  cat("\n\n")
  return(pts2)
}
