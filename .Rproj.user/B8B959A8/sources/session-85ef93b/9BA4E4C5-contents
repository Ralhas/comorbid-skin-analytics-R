# ── R/02_clean.R ─────────────────────────────────────────────
source("config.R")
library(dplyr)
library(stringr)
library(tidyr)

raw <- readRDS(file.path(DIR_RAW, "obf_raw.rds"))
message("Ham veri yüklendi: ", nrow(raw), " satır")

cleaned <- raw |>
  mutate(across(where(is.character), ~na_if(trimws(.), ""))) |>
  select(-any_of(c("nutrition", "nutriments"))) |>
  mutate(has_ingredients = !is.na(ingredients_text))

n_missing <- sum(!cleaned$has_ingredients)
message(sprintf("ingredients_text eksik: %d urün (%.1f%%) — analizden cikariliyor",
                n_missing, n_missing / nrow(cleaned) * 100))

df <- cleaned |> filter(has_ingredients)
message("Analize dahil edilecek urün: ", nrow(df))

parse_inci <- function(text) {
  result <- text |>
    str_to_upper() |>
    str_remove_all("\\([^)]*\\)") |>
    str_remove_all("\\[[^]]*\\]") |>
    str_remove_all("(?i)(may contain|peut contenir|[+]/-|±).*$")
  result <- unlist(str_split(result, "[,;]"))
  result <- str_squish(result)
  result <- result[nchar(result) > 2]
  result
}

test_parse <- parse_inci(df$ingredients_text[1])
cat("── INCI parse testi (ilk urün) ──\n")
cat(paste(head(test_parse, 10), collapse = "\n"), "\n")

df_long <- df |>
  mutate(product_id = row_number()) |>
  select(product_id, product_name, brands, source_category,
         countries_tags, labels_tags, ingredients_text) |>
  mutate(inci_list = lapply(ingredients_text, parse_inci)) |>
  unnest(inci_list) |>
  rename(ingredient = inci_list) |>
  group_by(product_id) |>
  mutate(position = row_number(), n_ingredients = n()) |>
  ungroup()

message("\nLong format olusturuldu:")
message("  Satir (urün x icerik): ", nrow(df_long))
message("  Unique urün: ",           n_distinct(df_long$product_id))
message("  Unique icerik: ",         n_distinct(df_long$ingredient))

df_long <- df_long |>
  mutate(
    country_clean = countries_tags |>
      str_extract("(?<=en:)[a-z-]+") |>
      str_to_title(),
    region = case_when(
      str_detect(tolower(countries_tags),
        "france|germany|spain|italy|united-kingdom|netherlands|belgium|sweden|poland") ~ "EU",
      str_detect(tolower(countries_tags), "united-states|canada") ~ "US",
      str_detect(tolower(countries_tags), "turkey|türkiye") ~ "TR",
      !is.na(countries_tags) ~ "OTHER",
      TRUE ~ "UNKNOWN"
    )
  )

cat("\n── Bölge dagilimi ──\n")
df_long |> distinct(product_id, region) |> count(region, sort = TRUE) |> print()

saveRDS(df_long, file.path(DIR_PROCESSED, "df_long.rds"))
saveRDS(df,      file.path(DIR_PROCESSED, "df_products.rds"))

message("\n✓ df_long.rds ve df_products.rds kaydedildi.")
message("Hazir: 03_risk_score.R")
