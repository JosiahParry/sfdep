library(sf)
library(sfdep)
library(dplyr)
library(ggplot2)
library(classInt)
library(patchwork)

lads <- read_sf("/Users/josiahparry/Downloads/book-master/data/brexit/local_authority_districts.geojson")

ref <- readr::read_csv("/Users/josiahparry/Downloads/book-master/data/brexit/brexit_vote.csv")


db <- select(lads, objectid, lad16nm, lad16cd) |>
  left_join(
    select(ref, Pct_Leave, lad16cd = Area_Code)
  ) |>
  rename_with(tolower) |>
  na.omit() |>
  st_transform(crs = 3857)

db |>
  mutate(`% Leave` = classify_intervals(pct_leave, 5)) |>
  ggplot(aes(fill = `% Leave`)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_viridis_d()

# Spatial Lag
# There are a couple of steps to this in R as we want to be very explicit
# we first create our neighbors, then our weights, then calculate the lag
df_nb <- db |>
  mutate(nb = st_knn(geometry, 8),
         wt = st_weights(nb),
         pct_leave_lag = st_lag(pct_leave, nb, wt))

df_nb |>
  filter(lad16cd %in% c('E08000012', 'S12000019')) |>
  select(contains("pct_leave"))


gg_1 <- df_nb |>
  mutate(`% Leave` = classify_intervals(pct_leave, 5)) |>
  ggplot(aes(fill = `% Leave`)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_viridis_d()

gg_2 <- df_nb |>
  mutate(`% Leave Lag` = classify_intervals(pct_leave_lag, 5)) |>
  ggplot(aes(fill = `% Leave Lag`)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_viridis_d()


gg_1 + gg_2


df_leave <- df_nb |>
  mutate(leave = ifelse(pct_leave > 50, TRUE, FALSE),
         wt = st_weights(nb, "B"))

ggplot(df_leave, aes(fill = leave)) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_brewer(type = "qual", palette = 8)

jc <- global_jc_perm(df_leave$leave, df_leave$nb, df_leave$wt)

global_jc_test(df_leave$leave,
               df_leave$nb,
               df_leave$wt,
               sampling = "free")


jcs <- df_leave |>
  as_tibble() |>
  summarise(res = tally_jc(leave, nb, wt)) |>
  tidyr::unnest(res)


jcs |>
  filter(joins != "Jtot") |>
  summarise(total_joins = sum(joincount))

# something is different and its likely either neighbors or omitting the
# missing values

moran_plot <- df_leave |>
  # center but not scale
  mutate(across(starts_with("pct_leave"), scale, scale = FALSE)) |>
  ggplot(aes(pct_leave, pct_leave_lag)) +
  geom_point() +
  geom_vline(
    aes(xintercept = mean(pct_leave)),
    color = "red", lty = 2
    ) +
  geom_hline(
    aes(yintercept = mean(pct_leave_lag)),
    color = "red", lty = 2
    ) +
  theme_light()

moran_plot

moran_res <- df_leave |>
  as_tibble() |>
  mutate(wt = st_weights(nb)) |>
  summarise(moran = list(global_moran_perm(pct_leave, nb, wt))) |>
  tidyr::unnest_wider(moran)

moran_res


moran_density <- moran_res |>
  select(res) |>
  tidyr::unnest(res) |>
  ggplot(aes(res)) +
  geom_density(fill = "black", alpha = 0.3) +
  geom_vline(xintercept = moran_res$statistic,
             color = "red", lty = 5) +
  theme_light()

moran_density | moran_plot


df_leave |>
  as_tibble() |>
  summarise(
    c = list(global_c_perm(
      pct_leave, nb, wt, alternative = "greater"))
    ) |>
  tidyr::unnest_wider(c)


# db_osgb = db.to_crs(epsg=27700)
# pts = db_osgb.centroid
# xys = pandas.DataFrame({'X': pts.x, 'Y': pts.y})
# min_thr = weights.util.min_threshold_distance(xys)
# min_thr

# line by line like pyhthon
db_osgb <- st_transform(db, crs = 27700)
pts <- st_centroid(st_geometry(db_osgb))
min_thr <- critical_threshold(pts)
min_thr

# all together as my preference with sfdep
db_osgb |>
  mutate(
    nb = st_dist_band(geometry),
    wt = st_weights(nb, style = "B")
  ) |>
  as_tibble() |>
  summarise(g = global_g_test(pct_leave, nb, wt)) |>
  pull(g)

df_leave |>
  as_tibble() |>
  mutate(wt = st_weights(nb, "B")) |>
  summarise(
    res = list(global_g_test(pct_leave, nb, wt, "two.sided"))
    ) |>
  pull(res)
