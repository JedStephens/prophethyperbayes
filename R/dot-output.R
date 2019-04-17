library(tidyverse)
process_starting_row  <- 600
per_validation_period <- 30
number_of_validations <- 5

graphical_data <- data.frame(x= 1:(process_starting_row + 1 + (number_of_validations)*per_validation_period))

for (it in 1:number_of_validations) {

  # For this graph there is always a line and then a color component explaining each one...
  graphical_data[,paste0("iteration",it,"line")]   <- c(it)

  # First make the whole row grey and then "dolly up" the colors.
  graphical_data[,paste0("iteration",it,"colour")] <- "grey"
  graphical_data[1:(process_starting_row + (it-1)*per_validation_period), paste0("iteration",it,"colour")] <- "blue"
  graphical_data[(process_starting_row + 1 + (it)*per_validation_period), paste0("iteration",it,"colour")] <- "red"

}
#graphical_data

ggbase <- ggplot(data = graphical_data, aes(x=x)) +
  coord_cartesian(xlim = c(process_starting_row-1*per_validation_period, nrow(graphical_data))) +
  theme_bw()

gg_adding <- function(data, iteration_sub, color_sub){
  iteration_promise <- enquo(iteration_sub)
  colour_promise <- enquo(color_sub)
  gg  <- geom_point(data = data, aes(x= x, y= !! iteration_promise, color = !! colour_promise))
  return(gg)
}

ggaddfor <- function(data, gg){
  ggout <- gg
for (it in 1:number_of_validations) {
  #print(it)
  iterationsub <- paste0("iteration",it,"line")
  coloursub <- paste0("iteration",it,"colour")

  ggout <- ggout + gg_adding(data, iterationsub, coloursub)

  }
  return(ggout)
}

# Not working
ggaddfor(graphical_data, ggbase)
# VS
# Working...
ggadd <- ggbase
ggadd <- ggadd + gg_adding(graphical_data, iteration1line, iteration1colour)
ggadd <- ggadd + gg_adding(graphical_data, iteration2line, iteration2colour)
ggadd <- ggadd + gg_adding(graphical_data, iteration3line, iteration3colour)
ggadd <- ggadd + gg_adding(graphical_data, iteration4line, iteration4colour)
ggadd <- ggadd + gg_adding(graphical_data, iteration5line, iteration5colour)
#return(list(graph = ggout, data = graphical_data))
#https://dplyr.tidyverse.org/articles/programming.html
