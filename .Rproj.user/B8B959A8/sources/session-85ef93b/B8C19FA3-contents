# ── R/03_risk_score.R ─────────────────────────────────────────
source("config.R")
library(dplyr)
library(stringr)

df_long <- readRDS(file.path(DIR_PROCESSED, "df_long.rds"))

# ── 1. Komorbid profil bazlı içerik veritabanı ────────────────
# Kaynak: CosIng, EWG Skin Deep, dermatoloji literatürü
# Her içerik için:
#   - risk_level  : banned / restricted / flagged / safe
#   - risk_score  : 0 (safe) → 4 (banned)
#   - mechanism   : etki mekanizması
#   - conditions  : hangi cilt durumlarında sorunlu
#   - synergy     : hangi durumlarla birlikte faydalı

# Mevcut genisletilmis DB yukle, yoksa yeni olustur
if (file.exists(file.path(DIR_PROCESSED, "ingredient_db_full.rds"))) {
  ingredient_db <- readRDS(file.path(DIR_PROCESSED, "ingredient_db_full.rds"))
  message("Mevcut DB yuklendi: ", nrow(ingredient_db), " icerik")
} else {
  message("DB bulunamadi, yeni olusturuluyor...")
}

ingredient_db_base <- tribble(
  ~ingredient,                          ~risk_level,    ~risk_score, ~mechanism,                        ~conditions,
  # ── Yasaklı (AB) ──────────────────────────────────────────────
  "MERCURY",                            "banned",       4,           "Heavy metal",                     "all",
  "LEAD ACETATE",                       "banned",       4,           "Heavy metal",                     "all",
  "HYDROQUINONE",                       "banned",       4,           "Depigmenting agent",              "all",
  "TRETINOIN",                          "banned_otc",   4,           "Retinoid",                        "acne;melasma",
  "FORMALDEHYDE",                       "banned",       4,           "Preservative/allergen",           "all",
  "TRICLOSAN",                          "banned",       4,           "Antimicrobial/endocrine",         "all",
  
  # ── Kısıtlı (AB) ──────────────────────────────────────────────
  "RESORCINOL",                         "restricted",   3,           "Depigmenting/keratolytic",        "melasma;acne",
  "KOJIC ACID",                         "restricted",   3,           "Tyrosinase inhibitor",            "melasma",
  "SALICYLIC ACID",                     "restricted",   3,           "BHA/keratolytic",                 "rosacea;atopic_dermatitis",
  "BENZOYL PEROXIDE",                   "restricted",   3,           "Antimicrobial/oxidant",           "rosacea;atopic_dermatitis",
  "ALPHA ARBUTIN",                      "restricted",   3,           "Tyrosinase inhibitor",            "rosacea",
  "LACTIC ACID",                        "restricted",   3,           "AHA/exfoliant",                   "rosacea;atopic_dermatitis",
  "GLYCOLIC ACID",                      "restricted",   3,           "AHA/exfoliant",                   "rosacea;atopic_dermatitis",
  "CITRIC ACID",                        "restricted",   2,           "AHA/pH adjuster",                 "rosacea",
  "RETINOL",                            "restricted",   3,           "Vitamin A derivative",            "rosacea;atopic_dermatitis",
  "RETINYL PALMITATE",                  "restricted",   2,           "Vitamin A ester",                 "rosacea",
  
  # ── Flagged (literatür / EWG) ──────────────────────────────────
  "NIACINAMIDE",                        "flagged",      1,           "B3 vitamin / flushing risk",      "rosacea",
  "FRAGRANCE",                          "flagged",      2,           "Allergen mix",                    "rosacea;atopic_dermatitis",
  "PARFUM",                             "flagged",      2,           "Allergen mix",                    "rosacea;atopic_dermatitis",
  "ALCOHOL DENAT",                      "flagged",      2,           "Barrier disruptor",               "rosacea;atopic_dermatitis",
  "SD ALCOHOL",                         "flagged",      2,           "Barrier disruptor",               "rosacea;atopic_dermatitis",
  "MENTHOL",                            "flagged",      2,           "Vasodilator",                     "rosacea",
  "CAMPHOR",                            "flagged",      2,           "Irritant/vasodilator",            "rosacea;atopic_dermatitis",
  "WITCH HAZEL",                        "flagged",      2,           "Astringent/irritant",             "rosacea;atopic_dermatitis",
  "PEPPERMINT",                         "flagged",      2,           "Menthol source",                  "rosacea",
  "EUCALYPTUS",                         "flagged",      2,           "Irritant",                        "rosacea;atopic_dermatitis",
  "SODIUM LAURYL SULFATE",              "flagged",      2,           "Surfactant/barrier disruptor",    "atopic_dermatitis;rosacea",
  "SODIUM LAURETH SULFATE",             "flagged",      2,           "Surfactant/barrier disruptor",    "atopic_dermatitis",
  "METHYLPARABEN",                      "flagged",      2,           "Paraben/endocrine concern",       "all",
  "PROPYLPARABEN",                      "flagged",      2,           "Paraben/endocrine concern",       "all",
  "BUTYLPARABEN",                       "flagged",      2,           "Paraben/endocrine concern",       "all",
  "ISOBUTYLPARABEN",                    "flagged",      2,           "Paraben/endocrine concern",       "all",
  "OXYBENZONE",                         "flagged",      2,           "UV filter/endocrine concern",     "melasma;rosacea",
  "OCTINOXATE",                         "flagged",      2,           "UV filter/endocrine concern",     "melasma",
  "METHYLISOTHIAZOLINONE",              "flagged",      2,           "Preservative/sensitizer",         "atopic_dermatitis;rosacea",
  "METHYLCHLOROISOTHIAZOLINONE",        "flagged",      2,           "Preservative/sensitizer",         "atopic_dermatitis;rosacea",
  "TRIETHANOLAMINE",                    "flagged",      1,           "pH adjuster/sensitizer",          "atopic_dermatitis",
  "COCAMIDOPROPYL BETAINE",             "flagged",      1,           "Surfactant/sensitizer",           "atopic_dermatitis",
  "PROPYLENE GLYCOL",                   "flagged",      1,           "Humectant/penetration enhancer",  "atopic_dermatitis",
  
  # ── Sinerjik / faydalı ────────────────────────────────────────
  "CERAMIDE NP",                        "safe",         0,           "Barrier repair",                  "atopic_dermatitis;rosacea",
  "CERAMIDE AP",                        "safe",         0,           "Barrier repair",                  "atopic_dermatitis;rosacea",
  "CERAMIDE EOP",                       "safe",         0,           "Barrier repair",                  "atopic_dermatitis;rosacea",
  "CHOLESTEROL",                        "safe",         0,           "Barrier repair",                  "atopic_dermatitis",
  "PANTHENOL",                          "safe",         0,           "Soothing/barrier",                "atopic_dermatitis;rosacea",
  "ALLANTOIN",                          "safe",         0,           "Soothing",                        "rosacea;atopic_dermatitis",
  "CENTELLA ASIATICA",                  "safe",         0,           "Anti-inflammatory",               "rosacea;acne",
  "AZELAIC ACID",                       "safe",         0,           "Dual: acne+melasma+rosacea",      "acne;melasma;rosacea",
  "TRANEXAMIC ACID",                    "safe",         0,           "Depigmenting/gentle",             "melasma;rosacea",
  "ZINC OXIDE",                         "safe",         0,           "UV filter/anti-inflammatory",     "rosacea;acne;atopic_dermatitis",
  "TITANIUM DIOXIDE",                   "safe",         0,           "Physical UV filter",              "rosacea;melasma",
  "HYALURONIC ACID",                    "safe",         0,           "Humectant",                       "atopic_dermatitis;rosacea",
  "SODIUM HYALURONATE",                 "safe",         0,           "Humectant",                       "atopic_dermatitis;rosacea",
  "GLYCERIN",                           "safe",         0,           "Humectant",                       "atopic_dermatitis;rosacea",
  "GLYCERINE",                          "safe",         0,           "Humectant",                       "atopic_dermatitis;rosacea",
  "SQUALANE",                           "safe",         0,           "Emollient/barrier",               "atopic_dermatitis;rosacea",
  "NIACINAMIDE",                        "safe",         0,           "Depigmenting/barrier (low dose)", "acne;melasma;atopic_dermatitis"
)

# ── 2. Duplike INCI temizle ve mevcut DB ile birlestir (niacinamide hem flagged hem safe) ─
# Komorbid bağlamda ikisi de gerekli — profil bazında değerlendireceğiz
# Burada genel DB'de flagged versiyonu öncelikli tut
ingredient_db <- ingredient_db |>
  group_by(ingredient) |>
  slice_max(risk_score, n = 1, with_ties = FALSE) |>
  ungroup()

# ── 3. df_long ile join ───────────────────────────────────────
df_scored <- df_long |>
  left_join(ingredient_db, by = "ingredient") |>
  mutate(
    risk_level = replace_na(risk_level, "unknown"),
    risk_score = replace_na(risk_score, 0)
  )

# ── 4. Ürün bazlı risk özeti ──────────────────────────────────
product_risk <- df_scored |>
  group_by(product_id, product_name, brands, source_category, region) |>
  summarise(
    n_ingredients    = n(),
    n_flagged        = sum(risk_level == "flagged"),
    n_restricted     = sum(risk_level == "restricted"),
    n_banned         = sum(risk_level %in% c("banned", "banned_otc")),
    n_unknown        = sum(risk_level == "unknown"),
    total_risk_score = sum(risk_score, na.rm = TRUE),
    avg_risk_score   = round(mean(risk_score, na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  mutate(
    risk_tier = case_when(
      n_banned > 0             ~ "HIGH",
      n_restricted > 0         ~ "MEDIUM",
      n_flagged > 1            ~ "LOW",
      TRUE                     ~ "MINIMAL"
    )
  )

cat("── Risk tier dağılımı ──\n")
product_risk |> count(risk_tier, sort = TRUE) |> print()

cat("\n── Kategori × risk tier ──\n")
product_risk |>
  count(source_category, risk_tier) |>
  tidyr::pivot_wider(names_from = risk_tier, values_from = n, values_fill = 0) |>
  print()

# ── 5. En sık görülen riskli içerikler ───────────────────────
cat("\n── En sık flagged/restricted/banned içerikler ──\n")
df_scored |>
  filter(risk_level %in% c("flagged", "restricted", "banned", "banned_otc")) |>
  count(ingredient, risk_level, sort = TRUE) |>
  head(20) |>
  print()

# ── 6. Kaydet ────────────────────────────────────────────────
saveRDS(df_scored,    file.path(DIR_PROCESSED, "df_scored.rds"))
saveRDS(product_risk, file.path(DIR_PROCESSED, "product_risk.rds"))

message("\n✓ df_scored.rds ve product_risk.rds kaydedildi.")
message("Hazır: 04_comorbid_analysis.R")
