library(ggplot2)

ggplot(iris, aes(x = Sepal.Length
                 , y = Sepal.Width
                 , color = Species)) +
  geom_point() +
  geom_smooth() 


library(ggplot2)
library(uberplot)
ggplot(iris, aes(x = Sepal.Length
                 , y = Sepal.Width
                 , color = Species)) +
  geom_point() +
  geom_smooth() +
  theme_uber_dl() +
  scale_color_manual(
    values = uber_primary_palette)


library(dplyr)
iris_summary <-
  iris %>%
  mutate(root_width = sqrt(Petal.Width)) %>%
  select(Species, root_width) %>%
  group_by(Species) %>%
  summarise(mean_root_width = mean(root_width)
            , median_root_width = median(root_width))


iris$root_width <- sqrt(iris$Petal.Width)
iris_subset <- iris[, c('Species', 'root_width')]
iris_mean <- aggregate(root_width ~ Species, data = iris, FUN = mean)
iris_median <- aggregate(root_width ~ Species, data = iris, FUN = median)
iris_summary <- data.frame(
  Species = iris_mean$Species
  , mean_root_width = iris_mean$root_width
  , median_root_width = iris_median$root_width)


### Write functional, vectorized R code
if_divisible_by <- function(x, n, result)
  ifelse(x %% n == 0, result, '')


fizzbuzz <- function(x){
  fizz <- if_divisible_by(x, 3, 'Fizz')
  buzz <- if_divisible_by(x, 5, 'Buzz')
  ifelse(fizz == '' & buzz == ''
         , x
         , paste0(fizz, buzz))
}

fizzbuzz(1:100)
