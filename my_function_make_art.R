
# FUNCTION: MAKE MODERN ART


# Function has two arguments: 
# Seed to reproduce the art, default to 1908 (pretty colours!)
# Stripes to control number of stripes per bar, default to 3000 (looks exciting!)

make_art <- function(seed = 1908, stripes = 3000) {
  
  library(ggplot2)
  
  set.seed(seed)
  
  # Data frame with random numbers which will be plotted later
  df <- as.data.frame(matrix(data = NA, stripes, 2))
  df[, 1] <- sample(1:10, stripes, replace = TRUE)
  
  # Allocate numbers to three bars
  df[1:(stripes - 2/3 * stripes), 2] <- 1
  df[(stripes - 2/3 * stripes):(stripes - 1/3 * stripes), 2] <- 2
  df[(stripes - 1/3 * stripes):stripes, 2] <- 3
  colnames(df) <- c("random", "ID")
  
  # Randomly sample two colours for the bars
  colour1 <- sample(colors(), 1)
  colour2 <- sample(colors(), 1)
  
  ggplot(df, aes(fill = random, x = ID, y = frequency(random))) +
    geom_bar(stat = "identity", width = 0.99) +
    theme_void() +
    theme(legend.position = "none") + 
    coord_flip() + 
    scale_fill_gradient(low = colour1, high = colour2)
  
}


