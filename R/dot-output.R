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
  iteration_promise <- sym(iteration_sub)
  colour_promise <- sym(color_sub)
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
ggiterated <- ggaddfor(graphical_data, ggbase)
ggfinal <- ggiterated + labs(x="Data points", y = "Iteration") + scale_color_manual(values = c("blue" = "blue", "grey" = "grey", "red" = "red")) + theme(legend.position="none")
# All the blue points are used as training data in that iteration.
# The red dot marks the final test point for that particular iteration.
# The grey dots between the blue, but on the left of the red dot are the data that is predicted and compared against the actual test data.
# The grey to the right of the red dot is data ignored in that particular iteration.
ggfinal
