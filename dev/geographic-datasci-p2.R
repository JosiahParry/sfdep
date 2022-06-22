library(sf)
library(sfdep)
library(dplyr)
library(ggplot2)

mx <- read_sf("/Users/josiahparry/Downloads/book-master/data/mexico/mexicojoin.shp")

mx |>
  select(NAME, PCGDP1940) |>
  slice(1:5)


mx |>
  ggplot(aes(PCGDP1940)) +
  geom_histogram(bins = 5) +
  geom_rug()

xvar <- mx$PCGDP1940
summary(mx$PCGDP1940)

classInt::classIntervals(mx$PCGDP1940, 5, "equal")
classInt::classIntervals(mx$PCGDP1940, 5, "quantile")
classInt::classIntervals(mx$PCGDP1940, 5, "sd")
classInt::classIntervals(mx$PCGDP1940, 5, "maximum")
# skipping box plot
classInt::classIntervals(mx$PCGDP1940, 5, "headtails")
# No Jenks Caspall
classInt::classIntervals(mx$PCGDP1940, 5, "fisher")
# No max-p

styles <- c("equal", "quantile", "sd", "maximum", "headtails", "fisher")

mx_df <- as_tibble(mx)

acdm <- purrr::map_dfr(styles, ~ {
  mx_df |>
    mutate(class = classInt::classify_intervals(PCGDP1940, 5, .x)) |>
    group_by(class) |>
    summarise(ADCM = sum(abs(PCGDP1940 - median(PCGDP1940)))) |>
    summarise(ADCM = sum(ADCM), style = .x)
})

acdm |>
  ggplot(aes(ADCM, style)) +
  geom_col()

# SD in R classifies differently than mapclassify hence the idfference in output


# Heatmap
mx_classes <- purrr::map_dfr(styles, ~ {
  mx_df |>
    mutate(class = classInt::classify_intervals(PCGDP1940, 5, .x, factor = FALSE),
           style = .x)
})

mx_classes |>
  arrange(PCGDP1940) |>
  mutate(name = forcats::fct_reorder(NAME, PCGDP1940)) |>
  ggplot(aes(style, name, fill = class)) +
  geom_tile() +
  scale_fill_viridis_c(option = "cividis")

mx_classes |>
  count(style, class) |>
  tidyr::pivot_wider(names_from = "style",
                     values_from = "n",
                     values_fill = 0)



# Mapping -----------------------------------------------------------------

mx_choro_df <- mx |>
  transmute(
    classes = classInt::classify_intervals(
      var = PCGDP1940, n = 5,
      style = "quantile",
      factor = TRUE
      ),
    # make factor pretty for printing
    class_labels = forcats::fct_reorder(
      names(classes), as.integer(classes)
      )
    )

mx_choro_df |>
  # plot
  ggplot(aes(fill = class_labels)) +
  geom_sf(color = "black", lwd = 0.2) +
  scale_fill_viridis_d(option = "D")


mx_choro_df |>
  # plot
  ggplot(aes(fill = class_labels)) +
  geom_sf(color = "black", lwd = 0.2) +
  scale_fill_brewer()




mx |>
  transmute(rank_1940 = min_rank(PCGDP1940),
            rank_2000 = min_rank(PCGDP2000),
            rnk_change = rank_2000 - rank_1940,
            change = cut(rnk_change, c(-Inf, -5, 0, 5, 20))) |>
  ggplot(aes(fill = change)) +
  geom_sf(color = "black", lwd = 0.2) +
  scale_fill_brewer(type = "div")


mx |>
  ggplot(aes(fill = HANSON98)) +
  geom_sf(lwd = 0.05, color = "white") +
  scale_fill_viridis_b()



mx |>
  mutate(hanson98 = as.character(HANSON98)) |>
  ggplot(aes(fill = hanson98)) +
  geom_sf(lwd = 0.1, color = "black") +
  scale_fill_brewer(type = "qual")


mx |>
  mutate(
    classes = cut(
      x = PCGDP2000,
      breaks = c(min(PCGDP2000), 10000, 12500, 15000, max(PCGDP2000)),
      include.lowest = TRUE
      ),
    classes = forcats::fct_rev(classes)
    ) |>
  ggplot(aes(fill = classes)) +
  geom_sf(lwd = 0.05, color = "white") +
  scale_fill_viridis_d()


mx |>
  mutate(
    classes = cut(
      x = PCGDP2000,
      breaks = c(-Inf, 10000, 12500, 15000, Inf),
      include.lowest = TRUE
    ),
    classes = forcats::fct_rev(classes)
  ) |>
  ggplot(aes(fill = classes)) +
  geom_sf(lwd = 0.05, color = "white") +
  scale_fill_viridis_d()


ts_df <- mx |>
  as_tibble() |>
  select(POLY_ID, starts_with("PCGDP")) |>
  tidyr::pivot_longer(cols = -POLY_ID,
                      names_to = "year",
                      values_to = "gdp"
                      ) |>
  mutate(year = as.integer(stringr::str_remove(year, "PCGDP")))

geo_df <- select(mx, POLY_ID)

library(classInt)

target_years <- c(1940, 1960, 1980, 2000)

tidy_gdp <- left_join(ts_df, geo_df, by = "POLY_ID") |>
  filter(year %in% target_years) |>
  mutate(classes = classify_intervals(gdp, n = 5)) |>
  ungroup()


all_gdp_plot <- purrr::map(target_years, ~ {
  tidy_gdp |>
    filter(year == .x) |>
    st_as_sf() |>
    ggplot(aes(fill = classes)) +
    geom_sf(color = "black", lwd = 0.1) +
    scale_fill_viridis_d() +
    labs(title = glue::glue("GDP {.x}")) +
    theme_void()
})

library(patchwork)

(all_gdp_plot[[1]] + all_gdp_plot[[2]]) / (all_gdp_plot[[3]] + all_gdp_plot[[4]])
