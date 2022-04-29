######################################################################
# Generic window creator for R 
# Dan Suthers, January 28, 2018 
# Linux option added Feburary 1, 2018 with help from Kyle Hart
# title: a string or null
# width, height: numeric or null 
# TODO: Windows options
######################################################################

new_window <- function(title=NULL, width=NULL, height=NULL){
  platform <- Sys.info()[[1]]
  if (platform =="Darwin") quartz(title, width, height)
  else if (platform == "Linux") x11(title=title, width=width, height=height)
  else dev.new()
  return(dev.cur())
}

######################################################################
# Pau