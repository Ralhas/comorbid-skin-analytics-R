# ── R/01_fetch.R ─────────────────────────────────────────────
# Open Beauty Facts API veri çekme
# Kaynak: https://world.openbeautyfacts.org
# Kategoriler: moisturisers, sunscreens, cleansers, toners

source("config.R")
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

fetch_obf_category <- function(category, page_size = 1000, pages = 3) {
  all_products <- list()
  for (page in seq_len(pages)) {
    message(sprintf("  -> %s | sayfa %d/%d", category, page, pages))
    resp <- request("https://world.openbeautyfacts.org/cgi/search.pl") |>
      req_url_query(
        action         = "process",
        tagtype_0      = "categories",
        tag_contains_0 = "contains",
        tag_0          = category,
        fields         = paste(c(
          "product_name", "brands", "categories_tags",
          "ingredients_text", "ingredients_tags",
          "countries_tags", "labels_tags"
        ), collapse = ","),
        json      = 1,
        page_size = page_size,
        page      = page
      ) |>
      req_timeout(30) |>
      req_retry(max_tries = 3) |>
      req_perform()
    data <- resp |> resp_body_json(simplifyVector = TRUE)
    if (length(data$products) == 0) break
    all_products[[page]] <- data$products
    Sys.sleep(0.5)
  }
  bind_rows(all_products) |> mutate(source_category = category)
}

categories <- c("moisturisers", "sunscreens", "cleansers", "toners")

message("Open Beauty Facts veri cekimi basliyor...")
raw_products <- map_dfr(categories, function(cat) {
  tryCatch(
    fetch_obf_category(cat),
    error = function(e) {
      message(sprintf("  Hata %s: %s", cat, e$message))
      NULL
    }
  )
})

message(sprintf("Toplam %d urun cekildi.", nrow(raw_products)))

# Flat CSV icin nested kolonlari duzlestir
flat_products <- raw_products |>
  select(-any_of(c("nutrition", "nutriments"))) |>
  mutate(across(where(is.list),
    ~sapply(., function(x) paste(unlist(x), collapse = "; "))))

saveRDS(raw_products,   file.path(DIR_RAW, "obf_raw.rds"))
write.csv(flat_products, file.path(DIR_RAW, "obf_raw.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

message("Kaydedildi: data/raw/obf_raw.rds ve obf_raw.csv")
message("Hazir: 02_clean.R")
