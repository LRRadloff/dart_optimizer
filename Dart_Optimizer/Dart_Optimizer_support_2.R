library(ellipse)
library(ggforce)
library(gridExtra)

source("Dart_Optimizer_support_1.R")

# radii and center(s) of circles of dart board
circle_df = tibble(x = 0, y = 0, r = rings)
circle_df

# all end points of boarder lines within dart board
border_phis <- 2 * pi / 40 + 0:19 / 20 * 2*pi
inner_border_points <- cart_coords(tibble(r = rings[2], phi = border_phis))
outer_border_points <- cart_coords(tibble(r = 1, phi = border_phis)) %>%
  rename(xend = x, yend = y)
border_df <- bind_cols(inner_border_points, outer_border_points) 

#board numbers df
number_phis <- 0:19 / 20 * 2 * pi
board_numbers_df <- cart_coords(tibble(r = 1.05, phi = number_phis)) %>%
  mutate(numbers = numbers)

# dart board creation
dart_board_plt <- ggplot()+ geom_circle(data = circle_df, mapping = aes(x0 = x, y0 = y, r = r)) +
  geom_segment(data = border_df, mapping = aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_void() +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1,1)) +
  theme(aspect.ratio=1) + 
  geom_text(data = board_numbers_df, mapping = aes(x = x, y = y, label = numbers))
