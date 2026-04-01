# ── R/07_ggplot2_viz.R ───────────────────────────────────────
source("config.R")
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(patchwork)
library(RColorBrewer)
library(ggupset)

df_scored        <- readRDS(file.path(DIR_PROCESSED, "df_scored.rds"))
comorbid_conflicts <- readRDS(file.path(DIR_PROCESSED, "comorbid_conflicts.rds"))
product_risk     <- readRDS(file.path(DIR_PROCESSED, "product_risk.rds"))
allergen_score   <- readRDS(file.path(DIR_PROCESSED, "allergen_score.rds"))
claim_conflicts  <- readRDS(file.path(DIR_PROCESSED, "claim_conflicts.rds"))
product_clusters <- readRDS(file.path(DIR_PROCESSED, "product_clusters.rds"))
ingredient_db    <- readRDS(file.path(DIR_PROCESSED, "ingredient_db_full.rds"))

# ── Tema ──────────────────────────────────────────────────────
theme_cosmo <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    panel.grid.minor = element_blank(),
    strip.text    = element_text(face = "bold")
  )

COLORS <- c(
  banned     = "#C0392B",
  restricted = "#E67E22",
  flagged    = "#F1C40F",
  safe       = "#27AE60",
  unknown    = "#BDC3C7"
)

# ── 1. Risk level dağılımı — lollipop chart ───────────────────
p1 <- df_scored |>
  filter(risk_level != "unknown") |>
  count(ingredient, risk_level, sort = TRUE) |>
  group_by(risk_level) |>
  slice_max(n, n = 8) |>
  ungroup() |>
  mutate(ingredient = reorder(ingredient, n)) |>
  ggplot(aes(x = n, y = ingredient, color = risk_level)) +
  geom_segment(aes(x = 0, xend = n, y = ingredient, yend = ingredient),
               linewidth = 0.8, alpha = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(values = COLORS) +
  facet_wrap(~risk_level, scales = "free_y", ncol = 2) +
  labs(
    title    = "En sık görülen riskli içerikler",
    subtitle = "Risk kategorisine göre, ürün sayısı",
    x = "Ürün sayısı", y = NULL, color = NULL
  ) +
  theme_cosmo +
  theme(legend.position = "none")

ggsave(file.path(DIR_FIGURES, "p1_lollipop_risk.png"), p1,
       width = 12, height = 8, dpi = 150)

# ── 2. Komorbid profil çakışma heatmap ────────────────────────
conflict_matrix <- df_scored |>
  filter(risk_score > 0) |>
  distinct(product_id, ingredient) |>
  left_join(
    ingredient_db |>
      mutate(condition_list = str_split(conditions, ";")) |>
      tidyr::unnest(condition_list) |>
      rename(condition = condition_list) |>
      filter(condition != "all") |>
      distinct(ingredient, condition),
    by = "ingredient"
  ) |>
  filter(!is.na(condition)) |>
  count(ingredient, condition) |>
  filter(n >= 5) |>
  pivot_wider(names_from = condition, values_from = n, values_fill = 0)

heatmap_data <- conflict_matrix |>
  pivot_longer(-ingredient, names_to = "condition", values_to = "n") |>
  filter(n > 0) |>
  group_by(ingredient) |>
  filter(sum(n) >= 10) |>
  ungroup() |>
  mutate(ingredient = str_trunc(ingredient, 28))

p2 <- ggplot(heatmap_data, aes(x = condition, y = ingredient, fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#FFF3E0", high = "#E65100") +
  labs(
    title    = "İçerik × Cilt durumu risk matrisi",
    subtitle = "Kaç üründe bu kombinasyon var",
    x = NULL, y = NULL, fill = "Ürün
sayısı"
  ) +
  theme_cosmo +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(DIR_FIGURES, "p2_heatmap_conditions.png"), p2,
       width = 10, height = 9, dpi = 150)

# ── 3. Komorbid profil karşılaştırma — bar chart ──────────────
p3 <- comorbid_conflicts |>
  group_by(comorbid_profile) |>
  summarise(
    avg_conflict = mean(conflict_score),
    n_products   = n(),
    .groups = "drop"
  ) |>
  mutate(comorbid_profile = reorder(comorbid_profile, avg_conflict)) |>
  ggplot(aes(x = avg_conflict, y = comorbid_profile, fill = comorbid_profile)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("n=%d | ort=%.2f", n_products, avg_conflict)),
            hjust = -0.1, size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Komorbid profil bazlı ortalama çakışma skoru",
    subtitle = "Yüksek skor = daha fazla problemli içerik kombinasyonu",
    x = "Ortalama çakışma skoru", y = NULL
  ) +
  theme_cosmo

ggsave(file.path(DIR_FIGURES, "p3_comorbid_conflict.png"), p3,
       width = 10, height = 5, dpi = 150)

# ── 4. Koku alerjen yükü — kategori bazlı violin ─────────────
p4 <- allergen_score |>
  filter(allergen_count > 0) |>
  ggplot(aes(x = source_category, y = allergen_load, fill = source_category)) +
  geom_violin(alpha = 0.7, show.legend = FALSE) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.5, color = "grey30") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title    = "Koku alerjen yük dağılımı — kategori bazlı",
    subtitle = "Sadece en az 1 koku alerjeni içeren ürünler",
    x = NULL, y = "Koku alerjen skoru"
  ) +
  theme_cosmo

ggsave(file.path(DIR_FIGURES, "p4_violin_allergen.png"), p4,
       width = 8, height = 6, dpi = 150)

# ── 5. Claim tutarsızlığı ─────────────────────────────────────
p5_data <- claim_conflicts |>
  left_join(df_scored |> distinct(product_id, source_category), by = "product_id") |>
  summarise(
    `Sensitive/derm claim + koku alerjeni` = sum(conflict_sensitive_frag, na.rm = TRUE),
    `Alcohol-free claim + alkol`           = sum(conflict_alcohol_free, na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "conflict_type", values_to = "n")

p5 <- ggplot(p5_data, aes(x = n, y = conflict_type, fill = conflict_type)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.3, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("#E74C3C", "#E67E22")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Ürün iddiası ile gerçek içerik arasındaki çelişkiler",
    subtitle = "Label claim tutarsızlığı analizi",
    x = "Ürün sayısı", y = NULL
  ) +
  theme_cosmo

ggsave(file.path(DIR_FIGURES, "p5_claim_conflicts.png"), p5,
       width = 9, height = 4, dpi = 150)

# ── 6. Küme profili radar benzeri bar ────────────────────────
cluster_profiles <- readRDS(file.path(DIR_PROCESSED, "cluster_profiles.rds"))

p6 <- cluster_profiles |>
  select(cluster, pct_flagged, pct_restricted, pct_safe) |>
  mutate(cluster = paste("Küme", cluster)) |>
  pivot_longer(-cluster, names_to = "risk_type", values_to = "pct") |>
  mutate(risk_type = str_remove(risk_type, "pct_") |> str_to_title()) |>
  ggplot(aes(x = risk_type, y = pct, fill = risk_type)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c(
    Flagged    = "#F1C40F",
    Restricted = "#E67E22",
    Safe       = "#27AE60"
  )) +
  facet_wrap(~cluster, ncol = 4) +
  labs(
    title    = "Küme bazlı risk profili dağılımı",
    subtitle = "İçerik risk kategorilerinin yüzdesi",
    x = NULL, y = "Yüzde (%)"
  ) +
  theme_cosmo

ggsave(file.path(DIR_FIGURES, "p6_cluster_risk_profile.png"), p6,
       width = 12, height = 5, dpi = 150)

message("
✓ 6 görsel kaydedildi:")
message("  figures/ klasörüne bak")
message("Hazır: 08_shiny_app.R")
