# ── R/05_clustering.R ───────────────────────────────────────
source("config.R")
library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(ggplot2)

df_scored <- readRDS(file.path(DIR_PROCESSED, "df_scored.rds"))

# ── 1. Tüm tanımlı içeriklerle binary matris ─────────────────
key_ingredients <- df_scored |>
  filter(risk_level != "unknown") |>
  count(ingredient, sort = TRUE) |>
  filter(n >= 5) |>
  pull(ingredient)

message("Kümeleme için içerik sayısı: ", length(key_ingredients))

product_matrix <- df_scored |>
  filter(ingredient %in% key_ingredients) |>
  mutate(present = 1) |>
  distinct(product_id, ingredient, present) |>
  pivot_wider(
    names_from  = ingredient,
    values_from = present,
    values_fill = 0
  )

mat <- product_matrix |>
  select(-product_id) |>
  as.matrix()

rownames(mat) <- product_matrix$product_id
message("Matris: ", nrow(mat), " urun x ", ncol(mat), " icerik")

# ── 2. Optimal k ──────────────────────────────────────────────
set.seed(42)
p_elbow <- fviz_nbclust(mat, kmeans, method = "wss", k.max = 8) +
  labs(title = "Optimal Kume Sayisi - Elbow Method") +
  theme_minimal()

ggsave(file.path(DIR_FIGURES, "elbow_plot.png"), p_elbow,
       width = 8, height = 5, dpi = 150)
message("elbow_plot.png kaydedildi")

# ── 3. K-means ────────────────────────────────────────────────
k <- 4
set.seed(42)
km <- kmeans(mat, centers = k, nstart = 25, iter.max = 100)
product_matrix$cluster <- km$cluster

message("
Kume buyuklükleri:")
print(table(km$cluster))

# ── 4. Küme profilleri ────────────────────────────────────────
cluster_profiles <- df_scored |>
  left_join(product_matrix |> select(product_id, cluster), by = "product_id") |>
  filter(!is.na(cluster)) |>
  group_by(cluster) |>
  summarise(
    n_products     = n_distinct(product_id),
    avg_risk       = round(mean(risk_score, na.rm = TRUE), 3),
    pct_flagged    = round(mean(risk_level == "flagged") * 100, 1),
    pct_restricted = round(mean(risk_level == "restricted") * 100, 1),
    pct_safe       = round(mean(risk_level == "safe") * 100, 1),
    top3_risky     = paste(
      names(sort(table(ingredient[risk_score > 0]), decreasing = TRUE)[1:3]),
      collapse = "; "
    ),
    .groups = "drop"
  )

cat("
── Küme profilleri ──
")
print(cluster_profiles)

cat("
── Küme x Kategori ──
")
df_scored |>
  left_join(product_matrix |> select(product_id, cluster), by = "product_id") |>
  filter(!is.na(cluster)) |>
  distinct(product_id, source_category, cluster) |>
  count(cluster, source_category) |>
  pivot_wider(names_from = source_category, values_from = n, values_fill = 0) |>
  print()

# ── 5. PCA görselleştirme ─────────────────────────────────────
pca_result <- prcomp(mat, scale. = TRUE)
pca_df <- as.data.frame(pca_result$x[, 1:2])
pca_df$cluster <- factor(km$cluster)
pca_df$product_id <- as.integer(rownames(pca_df))

pca_df <- pca_df |>
  left_join(df_scored |> distinct(product_id, source_category), by = "product_id")

p_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster, shape = source_category)) +
  geom_point(size = 2.5, alpha = 0.8) +
  labs(
    title    = "Urun Kümeleri - PCA",
    subtitle = "Tüm tanımlı icerik profiline gore",
    color    = "Küme", shape = "Kategori"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(DIR_FIGURES, "pca_clusters.png"), p_pca,
       width = 9, height = 6, dpi = 150)
message("pca_clusters.png kaydedildi")

# ── 6. Hiyerarşik kümeleme ────────────────────────────────────
dist_mat      <- dist(mat, method = "binary")
hclust_result <- hclust(dist_mat, method = "ward.D2")

png(file.path(DIR_FIGURES, "dendrogram.png"), width = 1200, height = 600, res = 120)
plot(hclust_result, labels = FALSE,
     main = "Hiyerarsik Kümeleme - Icerik Profili", xlab = "", sub = "")
rect.hclust(hclust_result, k = 4,
            border = c("#E63946", "#457B9D", "#2A9D8F", "#E9C46A"))
dev.off()
message("dendrogram.png kaydedildi")

# ── 7. Kaydet ─────────────────────────────────────────────────
product_clusters <- product_matrix |> select(product_id, cluster)
saveRDS(product_clusters, file.path(DIR_PROCESSED, "product_clusters.rds"))
saveRDS(cluster_profiles, file.path(DIR_PROCESSED, "cluster_profiles.rds"))

message("
Kümeleme tamamlandi.")
message("Hazir: 06_nlp.R")
