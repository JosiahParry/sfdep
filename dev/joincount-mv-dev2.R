# Python Code
# from esda.join_counts_local_mv import Join_Counts_Local_MV
# guerry_ds['infq5'] = 0
# guerry_ds['donq5'] = 0
# guerry_ds['suic5'] = 0
# guerry_ds.loc[(guerry_ds['Infants'] > 23574), 'infq5'] = 1
# guerry_ds.loc[(guerry_ds['Donatns'] > 10973), 'donq5'] = 1
# guerry_ds.loc[(guerry_ds['Suicids'] > 55564), 'suic5'] = 1
# w = libpysal.weights.Queen.from_dataframe(guerry_ds)
# LJC_MV = Join_Counts_Local_MV(connectivity=w).fit([guerry_ds['infq5'], guerry_ds['donq5'], guerry_ds['suic5']])
# LJC_MV.LJC
# LJC_MV.p_sim




inf <- as.integer(guerry$infants > 23574)
dons <- as.integer(guerry$donations > 10973)
suc <- as.integer(guerry$suicides > 5564)

dots <- list(inf, dons, suc)

nb <- st_contiguity(guerry)
wt <- st_weights(nb, style = "B")

all_nbs <- lapply(dots, find_xj, nb)
# -------------------------------------------------------------------------

dotnames <- paste0("var", 1:length(dots))
dots <- stats::setNames(dots, dotnames)

lhs_prod <- Reduce(`*`, dots)

df <- list2DF(stats::setNames(all_nbs, dotnames))


all_nb_ones <- apply(df, 1, \(x) prod(unlist(x)))

jc <- mapply(\(x, wt) sum(x * wt), all_nb_ones, wt)

index <- which(all_nb_ones == 1)

res <- lhs_prod
res[index] <- jc[index]
res




# -------------------------------------------------------------------------

xx <- c(dots, list(wt)) %>%
  list2DF() %>%
  apply(1,\(x) prod(unlist(x)))

mapply(\(x, wt) sum(x * wt), xx, wt)
