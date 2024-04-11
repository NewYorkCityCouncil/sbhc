## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "janitor","sf","readxl","jsonlite","stringr","ggiraph","mapview","RSocrata","geojsonsf","sf","leaflet","leaflet.extras","htmltools","scales","classInt", "readxl", "tigris",
                      "patchwork")

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#councildown.check <- "councildown" %in% installed.packages()[,"Package"]
councilverse.check <- "councilverse" %in% installed.packages()[,"Package"]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)
#if(councildown.check == FALSE) remotes::install_github("newyorkcitycouncil/councildown")
if(councilverse.check == FALSE) remotes::install_github("newyorkcitycouncil/councilverse")
  
# packages are loaded
lapply(c(list.of.packages,"councilverse"), require, character.only = TRUE)

# remove created variables for packages
rm(list.of.packages,new.packages,councilverse.check)

## Functions -----------------------------------------------
#geosearch------ 
Gcode<-function(Address, Boro){
  
  require(RCurl)
  p1=Address
  p1=gsub(" ","%20",p1)
  p2=Boro
  p2=gsub(" ","%20",p2)
  u1=c('https://geosearch.planninglabs.nyc/v2/autocomplete?text=')
  u4="&size=1"
  #url=paste(c(u1,p1,u2,p2,u3,p3,u4),collapse="")
  url=paste(c(u1,p1,"%20",p2,u4),collapse="")
  TMP=getURL(url)
  TMP=gsub("\"","",TMP)
  
  hn=strsplit(TMP,",housenumber:|,street:")[[1]][3]
  st=strsplit(TMP,",street:|,postalcode:")[[1]][3]
  ll=strsplit(TMP,"coordinates:|,properties")[[1]][2]
  ll=gsub("\\[|\\]|\\}","",ll)
  zip=strsplit(TMP,"postalcode:|,accuracy:")[[1]][2]
  boro=strsplit(TMP,"borough:|,borough_gid")[[1]][2]
  bbl=strsplit(TMP,"pad_bbl:|,pad_geomtype")[[1]][2]
  
  XY=c(hn,st,ll,zip,boro,bbl)
  names(XY)<-c("Number", "Street", "Coordinates", "Zip", "Borough", "BBL")
  return(ll)
}

# Function for (excel) proper capitalizations
proper=function(s) gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(s), perl=TRUE)

# Adding custom legend
addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, title,position = "topright",opacity = 0.5){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity,title = title,position = position))
}