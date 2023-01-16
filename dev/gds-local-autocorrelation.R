library(sf)
library(sfdep)
library(dplyr)
library(ggplot2)
library(classInt)
library(patchwork)

lads <- st_read("/Users/josiahparry/Downloads/book-master/data/brexit/local_authority_districts.geojson")

ref <- readr::read_csv("/Users/josiahparry/Downloads/book-master/data/brexit/brexit_vote.csv")


db <- select(lads, objectid, lad16nm, lad16cd) |>
  left_join(
    select(ref, Pct_Leave, lad16cd = Area_Code)
  ) |>
  rename_with(tolower) |>
  na.omit() |>
  st_transform(crs = 3857)

db |>
  # center but not scale
  mutate(
    nb = st_knn(geometry, 8),
    wt = st_weights(nb),
    pct_leave_lag = st_lag(pct_leave, nb, wt),
    across(starts_with("pct_leave"), scale, scale = FALSE)
    ) |>
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
  annotate("text", x = 20, y = 5, label = "HH", color = "red", size = 12) +
  annotate("text", x = 12, y = -11, label = "HL", color = "red", size = 12) +
  annotate("text", x = -20, y = 8, label = "LH", color = "red", size = 12) +
  annotate("text", x = -25, y = -11, label = "LL", color = "red", size = 12) +
  theme_light()

db_lisa <- db |>
  # center but not scale
  mutate(
    nb = st_knn(geometry, 8),
    wt = st_weights(nb),
    moran = local_moran(pct_leave, nb, wt)
  ) |>
  tidyr::unnest(moran)

db_lisa |>
  ggplot(aes(ii)) +
  geom_density() +
  geom_rug() +
  theme_light()



local_stats <- db_lisa |>
  mutate(class = classify_intervals(ii, 5)) |>
  ggplot(aes(fill = class)) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_viridis_d(option = "plasma") +
  theme_void() +
  labs(title = "Local Statistics")


spq <- db_lisa |>
  ggplot(aes(fill = mean)) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_brewer(type = "div", palette = 5, direction = -1) +
  theme_void() +
  labs(title = "Scatterplot Quadrant")


significance <- db_lisa |>
  mutate(significance = ifelse(p_folded_sim < 0.05,
                               "Significant", "Non-Significant")) |>
  ggplot(aes(fill = significance)) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_brewer(type = "qual", palette = 5, direction = -1) +
  theme_void() +
  labs(title = "Significance")



moran_clust_map <- db_lisa |>
  mutate(significance = ifelse(p_folded_sim < 0.05,
                               "Significant", "Non-Significant"),
         moran_cluster = ifelse(
           significance == "Significant",
           mean, NA
           ),
         moran_cluster = factor(
           moran_cluster, 1:4, labels = levels(mean)
            )
         ) |>
  ggplot(aes(fill = moran_cluster)) +
  geom_sf(color = "black", lwd = 0.15) +
 scale_fill_brewer(type = "div", palette = 5, direction = -1) +
  theme_void() +
  labs(title = "Moran Cluster Map")

(local_stats + spq) / (significance + moran_clust_map)




as.integer(db_lisa$mean)

table(db_lisa$mean)


sum(db_lisa$p_folded_sim <= 0.05) * 100 / nrow(db_lisa)

# we don't need to do these data engineering steps


# Local Gs ----------------------------------------------------------------
# Important to note that st_knn is using point on surface and not centroid
# this could be creating some differences
db_g <- db |>
  mutate(nb = st_knn(geometry, 8),
         nb_self = include_self(nb),
         wt = st_weights(nb),
         wt_self = st_weights(nb_self),
         g = local_g_perm(pct_leave, nb, wt),
         gstar = local_gstar_perm(pct_leave, nb_self, wt_self))


gg_g <- db_g |>
  tidyr::unnest(g) |>
  mutate(sig = ifelse(p_folded_sim < 0.05, TRUE, FALSE),
         clust = case_when(
           sig & gi < 0 ~ "LL",
           sig & gi > 0 ~ "HH"
         )) |>
  ggplot(aes(fill = clust)) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_brewer(type = "qual", palette = 4) +
  theme_void()


gg_gstar <- db_g |>
  tidyr::unnest(gstar) |>
  mutate(sig = ifelse(p_folded_sim < 0.05, TRUE, FALSE),
         clust = case_when(
           sig & gi_star < 0 ~ "LL",
           sig & gi_star > 0 ~ "HH"
         )) |>
  ggplot(aes(fill = clust)) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_brewer(type = "qual", palette = 4) +
  theme_void()

gg_g + gg_gstar


# From raster -------------------------------------------------------------




tif_fp <- "/Users/josiahparry/Downloads/book-master/data/ghsl/ghsl_sao_paulo.tif"

pop <- stars::read_stars(tif_fp)

pop_sf <- sf::st_as_sf(pop)

pop_lisa <- pop_sf |>
  rename(x = 1) |>
  mutate(nb = st_contiguity(geometry),
         wt = st_weights(nb),
         lisa = local_moran(x, nb, wt, nsim = 149))


gg_pop <- pop_lisa |>
  tidyr::unnest(lisa) |>
  mutate(
    moran_cluster = ifelse(
      p_folded_sim <= 0.01,
      mean, NA
    ),
    moran_cluster = factor(
      moran_cluster, 1:4, labels = levels(mean)
    )
  ) |>
  ggplot(aes(fill = moran_cluster)) +
  geom_sf(color = "black", lwd = 0) +
  scale_fill_brewer(type = "div", palette = 5, direction = -1) +
  theme_void()

