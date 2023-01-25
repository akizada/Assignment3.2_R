
# Q 3.1.1
grades <- rnorm(60, mean = 7, sd = 1)
hist(grades)


# Q 3.1.2
temp <- read.delim("https://bit.ly/3GLVQ86", sep = ",")
plot(temp$DATE, temp$TMAX)


# Q 3.1.3
library(titanic)
tit <- titanic_train

ggplot(tit, aes(
  
  # Different colours for Survived/Dead
  fill = factor(Survived), 
  y = frequency(Sex),
  x = Sex
)) +
  geom_bar(stat = "identity") +
  labs(y = "count") +
  
  # Include legend
  scale_fill_discrete(name = "How did it go?", labels = c("dead", "alive"))


# Q 3.1.4
# Reuse code from above and store it as base plot
base <- ggplot(tit,
               aes(
                 fill = factor(Survived),
                 y = frequency(Sex),
                 x = Sex
               )) +
  geom_bar(position = "stack", stat = "identity") +
  labs(y = "count") +
  scale_fill_discrete(name = "How did it go?", labels = c("dead", "alive"))

# Try out different themes
base + theme_bw()
base + theme_classic()
base + theme_dark()
base + theme_gray()
base + theme_light()
base + theme_minimal()
base + theme_void()
base + theme_linedraw()

# For this plot, I find both theme light and theme minimal (which are very
# similar anyways) the best, because they look very clean and aid the reader
# with grey lines without obstructing the process of understanding the plot.


# Q 3.1.5
# Original graph
plot(ToothGrowth$supp, ToothGrowth$len)

# Improved graph
ggplot(ToothGrowth, aes(x = supp, y = len, fill = supp)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Supplement", y = "Tooth Length") +
  scale_fill_discrete(name = "Supplement",
                      labels = c("Orange Juice", "Vitamin C"))

# 1) Added proper descriptions of the axes and the variables (legend)
# 2) Changed the background to include lines that aid the reader in
#     figuring out the exact values
# 3) Coloured the boxplots so that they are easily distinguishable
#     (and look more inviting)


# Q 3.1.6
library(ggplot2)

# Convert chicken ID to numeric value to be able to select it later
df <- ChickWeight
df$Chick <- as.character(df$Chick)
df$Chick <- as.numeric(df$Chick)


# Empty vector to store all maximum weights 
all_max <- c()

# Loop over all 50 chickens
for (i in 1:50) {
  
  # Calculate maximum weight and store it
  max_chick <- max(df$weight[df$Chick == i])
  all_max <- c(all_max, max_chick)
}


# Create data frame to retrieve weights and chicken IDs
all_max <- as.data.frame(list(as.character(1:50), all_max))

# Rename them for the plot
colnames(all_max) <- c("chick", "max_weight")

# Select and order chicken IDs
relevant_rows <- c("1", "20", "3", "40", "5")

# Plot selected chicken weights
p1 <-
  ggplot(all_max[relevant_rows,], aes(x = chick, y = max_weight)) +
  geom_bar(stat = "identity")
p1


# Q 3.1.7
ggplot(ChickWeight, aes(x = Time, y = weight)) +
  geom_smooth(method = lm)


# Q 3.1.8
library(patchwork)
library(dplyr)

# Start fresh: Convert chicken ID to character
df <- ChickWeight
df$Chick <- as.character(df$Chick)

# Reduce data set to relevant chicken IDs
df2 <- df %>% filter(
  Chick == 1 | Chick == 20 | Chick == 3 | Chick == 40 | Chick == 5
  )

# Plot individual growth curves
p2 <-
  ggplot(df2, aes(
    x = Time,
    y = weight,
    group = Chick,
    color = Chick
  )) +
  geom_line()

# Combine both plots
p1 + p2


# Q 3.1.9
library(ggplot2)
library(ggstatsplot)

ggbetweenstats(ToothGrowth,
               x = supp,
               y = len,
               title = "Tooth Growth in Guinea Pigs")

# The t-test is non-significant (p = 0.06) and the confidence interval
# also includes 0, therefore it is clear that the difference between the
# supplements is NOT significant.
# This indicates that the teeth do not grow better when taking Vitamin C
# through Orange Juice (vs. meds).


# Q 3.1.10
library(plotly)

humans <- read.delim(
  paste0(
    "https://raw.githubusercontent.com/hannesrosenbusch/",
    "schiphol_class/master/Body%20Measurements%20_%20original_CSV.csv"
  ),
  sep = ","
)

plot_ly(
  humans,
  x = ~ TotalHeight,
  y = ~ LegLength,
  z = ~ ShoulderToWaist
)


# Q 3.1.11
library(gganimate)
library(cranlogs)

# Package downloads in relevant time period  
downloads <- cran_downloads(
  packages = c("caret", "tidymodels"),
  from = "2013-08-31",
  to = "2023-01-01"
)

# Create plot for animation
down_anim <- ggplot(downloads, aes(
  x = date,
  y = count,
  group = package,
  color = package
)) +
  geom_line() +
  labs(title = "Package popularity over time", y = "Package Downloads") +
  theme_linedraw() +
  transition_reveal(along = date)    # Animation along the date stamp

# Create animation that looks most similar to the one provided (details, size)
animate(
  down_anim,
  nframes = 200,
  fps = 10,
  width = 300,
  height = 300,
  end_pause = 70
)


# Q 3.1.12
library(quantmod)

# Retrieve stock data of ExxonMobil
getSymbols("XOM", src = "yahoo")

# Plot stock price data of 2022
chart_Series(XOM, subset = "2022")

# XOM = ExxonMobil (oil and gas corporation)


# Q 3.1.13
# Make function with three arguments
# Default =  Google stock price of 2022
plotstock <- function(stock = "GOOGL",
                      year = "2022",
                      file.name = "Google_Stock_2022.png") {
  
  # Retrieve working directory (saving png should work on any computer)
  wd <- getwd()
  
  # Put .png in working directory
  png(file = paste0(wd, "/", file.name))
  
  # Save stock data to reuse later
  df <- getSymbols(stock,
                   src = "yahoo",
                   
                   # Necessary argument for assigning data to new data frame
                   auto.assign = FALSE)    
  
  # Final plot
  print(chart_Series(df,
              subset = year,
              name = paste("Stock Prices for", stock)))
  dev.off()
}

# Check if function works as it should
plotstock()


# Q 3.1.14
library(lintr)

# Lint current script to find poor coding style 
lint("/Users/akizada/Documents/UvA/1.3/Week 3/Assignment3.1_Arui.R")

# In Nr. 4, I added space before and after every equal sign, specifically 
# focusing on the arguments of the ggplot functions/layers.

# In Nr. 10 the link was much longer than 80 characters, so I broke it up
# and used paste0() to correctly read in the data set from the URL. Also, 
# I changed the layout slightly (open brackets in one line, then add the code 
# inside the brackets in the next line, close brackets in another line).


# Q 3.1.15
# Original code with extra print statements
a <- "dog"
b = "cats"
v = function(x, y){
  xx <- strsplit(x, "", )[[1]]
  print(xx)
  yy <- strsplit(y, "", )[[1]]
  print(yy)
  lxx = length(xx)
  lyy = length(yy)
  v <- lxx == lyy
  print(v)
  if(v) return(T)
}
v(a, b)

# Purpose: The function checks whether two character strings have the 
# same number of letters and returns TRUE or FALSE.

# Why is it bad style?
# 1) Alternating use of <- and = to assign variables (should only use <- !)
# 2) Variable names are very unclear and unspecific (e.g., x, y, v)
# 3) The name of the function is reused inside the function for something else
# 4) The use of "T" could lead to confusion, one should always spell out 
#     "TRUE" completely


# Q 3.1.16
t(matrix(1:9, nrow = 3)) * c(1:3)


# Q 3.1.17
# The shortcut is: Control + Shift + A


# Q 3.1.18
library(devtools)
library(memer)

meme_get("FirstWorldProbs") %>%
  meme_text_bottom("copied code from Google \n doesn't work on first try")

meme_get("TwoButtonsAnxiety") %>%
  meme_text_bottom("<-  or  =")


