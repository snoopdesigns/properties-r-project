source("utils/encoding.r")

source_utf8("utils/model_utils.r")

df_complexes <- load_dataframe("data/complexes.csv", factors = TRUE)
df_apartments <- load_dataframe("data/apartments.csv", factors = TRUE)
df_apartments <- merge(x = df_complexes, y = df_apartments, by = "complex_id", all = TRUE)

res <- MODELUTILS_run_model(df_apartments)
print("FUUU")