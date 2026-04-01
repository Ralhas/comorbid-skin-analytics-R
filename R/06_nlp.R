# ── R/06_nlp.R ───────────────────────────────────────────────
source("config.R")
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(ggplot2)

df_scored <- readRDS(file.path(DIR_PROCESSED, "df_scored.rds"))

# ── 1. Ürün ismi + marka üzerinden claim extraction ───────────
# "sensitive", "gentle", "hypoallergenic" gibi ifadeler
# gerçek içerik profiliyle örtüşüyor mu?

claim_keywords <- c(
  "sensitive", "sensible", "sensitiv",
  "gentle", "doux", "sanft",
  "hypoallergenic", "hypoallergenique",
  "soothing", "apaisant", "beruhigend",
  "fragrance.free", "sans.parfum", "parfumfrei",
  "alcohol.free", "sans.alcool", "alkoholfrei",
  "natural", "naturel", "natürlich",
  "dermatologist", "dermatologue", "dermatologisch",
  "calming", "calmant"
)

# Ürün isimlerinden claim tespiti
df_products <- readRDS(file.path(DIR_PROCESSED, "df_products.rds"))

df_claims <- df_products |>
  mutate(
    product_id   = row_number(),
    name_lower   = tolower(paste(product_name, brands, labels_tags, sep = " ")),
    claims_sensitive     = str_detect(name_lower, "sensitiv|sensible|sensitive"),
    claims_gentle        = str_detect(name_lower, "gentle|doux|sanft|doux"),
    claims_hypoallergenic= str_detect(name_lower, "hypoallerg"),
    claims_fragrance_free= str_detect(name_lower, "fragrance.free|sans.parfum|parfumfrei|unscented"),
    claims_alcohol_free  = str_detect(name_lower, "alcohol.free|sans.alcool|alkoholfrei"),
    claims_dermatologist = str_detect(name_lower, "dermatolog"),
    claims_natural       = str_detect(name_lower, "natural|naturel|natür"),
    claims_soothing      = str_detect(name_lower, "sooth|apaisant|beruhig|calm"),
    any_claim = claims_sensitive | claims_gentle | claims_hypoallergenic |
                claims_fragrance_free | claims_alcohol_free |
                claims_dermatologist | claims_natural | claims_soothing
  )

cat("── Claim dağılımı ──
")
df_claims |>
  summarise(across(starts_with("claims_"), sum)) |>
  tidyr::pivot_longer(everything(), names_to = "claim", values_to = "n") |>
  mutate(claim = str_remove(claim, "claims_"),
         pct   = round(n / nrow(df_claims) * 100, 1)) |>
  arrange(desc(n)) |>
  print()

# ── 2. Claim vs gerçek içerik tutarsızlığı ────────────────────
# "fragrance free" diyor ama parfum/fragrance içeriyor mu?

claim_conflicts <- df_scored |>
  left_join(df_claims |> select(product_id, starts_with("claims_"), any_claim),
            by = "product_id") |>
  group_by(product_id) |>
  summarise(
    has_fragrance     = any(ingredient %in% c("FRAGRANCE", "PARFUM", "PARFUM.")),
    has_alcohol       = any(ingredient %in% c("ALCOHOL DENAT", "ALCOHOL DENAT.", "ALCOHOL", "SD ALCOHOL")),
    has_fragrance_allergen = any(ingredient %in% c(
      "LIMONENE", "LINALOOL", "CITRONELLOL", "GERANIOL",
      "EUGENOL", "COUMARIN", "BENZYL SALICYLATE", "HEXYL CINNAMAL",
      "ALPHA-ISOMETHYL IONONE", "BUTYLPHENYL METHYLPROPIONAL"
    )),
    claims_fragrance_free = first(claims_fragrance_free),
    claims_alcohol_free   = first(claims_alcohol_free),
    claims_sensitive      = first(claims_sensitive),
    claims_dermatologist  = first(claims_dermatologist),
    .groups = "drop"
  ) |>
  mutate(
    # Gerçek çelişkiler
    conflict_fragrance_free = claims_fragrance_free & has_fragrance,
    conflict_alcohol_free   = claims_alcohol_free   & has_alcohol,
    # "Sensitive" veya "dermatologist tested" ama yüksek koku alerjen yükü
    conflict_sensitive_frag = (claims_sensitive | claims_dermatologist) & has_fragrance_allergen
  )

cat("
── Claim tutarsızlıkları ──
")
cat("Fragrance-free deyip parfum iceren: ",
    sum(claim_conflicts$conflict_fragrance_free, na.rm = TRUE), "
")
cat("Alcohol-free deyip alkol iceren: ",
    sum(claim_conflicts$conflict_alcohol_free, na.rm = TRUE), "
")
cat("Sensitive/derm-tested deyip koku alerjeni iceren: ",
    sum(claim_conflicts$conflict_sensitive_frag, na.rm = TRUE), "
")

# ── 3. En sık birlikte görülen içerik çiftleri ────────────────
# Aynı üründe hangi riskli içerikler birlikte geliyor?
risky_pairs <- df_scored |>
  filter(risk_score > 0) |>
  select(product_id, ingredient) |>
  inner_join(
    df_scored |> filter(risk_score > 0) |> select(product_id, ingredient),
    by = "product_id",
    suffix = c("_1", "_2")
  ) |>
  filter(ingredient_1 < ingredient_2) |>
  count(ingredient_1, ingredient_2, sort = TRUE) |>
  head(20)

cat("
── En sık riskli icerik ciftleri ──
")
print(risky_pairs)

# ── 4. Koku alerjen yük skoru ─────────────────────────────────
eu_allergens <- c(
  "LIMONENE", "LINALOOL", "CITRONELLOL", "GERANIOL",
  "EUGENOL", "COUMARIN", "BENZYL SALICYLATE", "HEXYL CINNAMAL",
  "ALPHA-ISOMETHYL IONONE", "BUTYLPHENYL METHYLPROPIONAL",
  "BENZYL ALCOHOL", "FRAGRANCE", "PARFUM", "PARFUM."
)

allergen_score <- df_scored |>
  group_by(product_id, product_name, brands, source_category) |>
  summarise(
    allergen_count = sum(ingredient %in% eu_allergens),
    allergen_load  = sum(risk_score[ingredient %in% eu_allergens], na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(allergen_load))

cat("
── Koku alerjen yükü en yüksek 10 ürün ──
")
print(head(allergen_score, 10))

cat("
── Kategori bazlı ortalama koku alerjen yükü ──
")
allergen_score |>
  group_by(source_category) |>
  summarise(
    avg_allergen_load  = round(mean(allergen_load), 2),
    avg_allergen_count = round(mean(allergen_count), 2),
    .groups = "drop"
  ) |>
  arrange(desc(avg_allergen_load)) |>
  print()

# ── 5. Kaydet ────────────────────────────────────────────────
saveRDS(df_claims,       file.path(DIR_PROCESSED, "df_claims.rds"))
saveRDS(claim_conflicts, file.path(DIR_PROCESSED, "claim_conflicts.rds"))
saveRDS(allergen_score,  file.path(DIR_PROCESSED, "allergen_score.rds"))

message("
NLP analizi tamamlandi.")
message("Hazir: 06_ggplot2_viz.R")
