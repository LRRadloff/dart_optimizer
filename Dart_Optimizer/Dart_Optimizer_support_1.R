library(tidyverse)

polar_coords <- function(xy_df) {
  x <- xy_df[[1]]
  y <- xy_df[[2]]
  r <- (x^2 + y^2)^(1/2)
  phi <- if_else(
    condition = y >= 0,
    true = acos(x/r),
    false = 2 * pi - acos(x/r)
  )
  return(tibble(r, phi))
}

cart_coords <- function(rphi_df) {
  r <- rphi_df[[1]]
  phi <- rphi_df[[2]]
  x <- r * cos(phi)
  y <- r * sin(phi)
  return(tibble(x, y))
}

numbers <- c(6, 13, 4, 18, 1, 20, 5, 12, 9, 14, 11, 8, 16, 7, 19, 3, 17, 2, 15, 10)
rings <- c(6.35, 16, 99, 107, 162, 170)
rings <- rings / max(rings)

# function, that determines result of dart throws
# input: df with 2 cols that represent cartesian coordinates of points where dart hit the board
# board = unit circle
dart_result <- function(shots_df) {
  pc <- polar_coords(shots_df)
  
  pc$phi <- pc$phi/(2*pi) + 1/40
  pc$phi <- pc$phi - floor(pc$phi)
  
  number <- numbers[ceiling(20 * pc$phi)]
  return(
    case_when(
      pc$r < rings[1] ~ 50,
      pc$r < rings[2] ~ 25,
      pc$r < rings[3] ~ number,
      pc$r < rings[4] ~ 3* number,
      pc$r < rings[5] ~ number,
      pc$r < rings[6] ~ 2 * number,
      TRUE ~ 0
    )
  )
}  
