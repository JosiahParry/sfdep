
# Colocation Quotient -----------------------------------------------------
library(sf)
library(sfdep)
library(tidyverse)

crimes <- read_csv("/Users/josiahparry/Downloads/tmp7_f32p54.csv",
                   guess_max = 9999) |>
  rename_all(tolower) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)


top_crimes <- crimes |>
  filter(offense_description %in% c("THREATS TO DO BODILY HARM",
                                    "PROPERTY - LOST/ MISSING",
                                    "ASSAULT - SIMPLE",
                                    "INVESTIGATE PERSON",
                                    "VANDALISM"))

#
# top_crimes |>
#   as_tibble()
#   # including friday
#   mutate(is_weekend = day_of_week  %in% c("Friday", "Saturday", "Sunday")) |>
#   group_by(offense_description, is_weekend) |>
#   summarise(n = n()) |>
#   mutate(`%` = scales::percent(n / sum(n)))
#


set.seed(0)
x <- top_crimes |>
  bind_cols(
    data.frame(st_coordinates(top_crimes))
  ) |>
  filter(abs(X) > 0, abs(Y) > 0) |>
  distinct(geometry, .keep_all = TRUE) |>
  mutate(is_weekend = day_of_week  %in% c("Friday", "Saturday", "Sunday")) |>
  select(offense_description, is_weekend) |>
  group_by(offense_description) |>
  slice_sample(n = 1000) |>
  ungroup() |>
  mutate(nb = st_dist_band(geometry),
         # grab only 5
         nb = lapply(nb, \(.x) na.omit(.x[1:5])))

# lapply(unclass(x$nb), `[`, 1:5)

# variables ---------------------------------------------------------------

A <- as.factor(x$offense_description)
B <- as.factor(x$is_weekend)
nb <- x$nb
bij <- find_xj(B, nb)
n_prime_b <- length(B)
n_a = table(A)
a_vals <- names(n_a)
b_vals <- as.character(unique(B))
