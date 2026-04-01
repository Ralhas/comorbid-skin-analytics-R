# Comorbid Skin Conditions & Cosmetic Ingredient Conflicts
## An R-Based INCI Analysis

![R](https://img.shields.io/badge/R-4.5-276DC3?style=flat&logo=r)
![License](https://img.shields.io/badge/license-MIT-green)
![Data](https://img.shields.io/badge/data-Open%20Beauty%20Facts-orange)

## Overview

Most skincare research addresses single skin conditions in isolation. But what happens when a patient has **both rosacea and melasma**? Or **acne and atopic dermatitis**? The ingredient that treats one condition may actively worsen the other.

This project takes a data-driven approach to identify **conflicting and synergistic INCI ingredients** across four clinically relevant comorbid skin condition profiles, using open cosmetic product data and dermatology literature.

---

## Research Question

> *For patients with comorbid skin conditions, which cosmetic ingredients create conflicts — and which are safe or beneficial for both conditions simultaneously?*

---

## Comorbid Profiles Analyzed

| Profile | Conflict Focus |
|---|---|
| Rosacea + Melasma | Niacinamide dose paradox, UV filter selection |
| Acne + Melasma | Retinoid overlap, tyrosinase inhibitors |
| Acne + Rosacea | Highest conflict score (2.68 avg) |
| Atopic Dermatitis + Acne | Barrier vs. sebum paradox (highest score: 2.71) |

---

## Data Sources

| Source | Type | Products/Ingredients |
|---|---|---|
| [Open Beauty Facts](https://world.openbeautyfacts.org) | API | 262 products (sunscreens, cleansers) |
| [INCIdecoder](https://incidecoder.com) | Scraped | 397 serums |
| EU CosIng Annex II/III | Literature | Banned/restricted reference |
| EWG Skin Deep | Literature | Flagged ingredients |
| Dermatology literature | Manual | Comorbid-specific risk DB |

**Total: 659 products, 3,025 unique ingredients, 601 ingredients classified**

---

## Methodology

### Pipeline
```
01_fetch.R              → API data collection (Open Beauty Facts)
02_clean.R              → INCI parsing, long format conversion
03_risk_score.R         → Literature-based risk scoring (0-4 scale)
04_comorbid_analysis.R  → Conflict/synergy analysis per profile
05_clustering.R         → K-means + hierarchical clustering, PCA
06_nlp.R                → Claim conflict detection, allergen load
07_ggplot2_viz.R        → Publication-quality visualizations
app/app.R               → Bilingual Shiny dashboard (TR/EN)
```

### Risk Classification

| Level | Score | Definition |
|---|---|---|
| Banned | 4 | Prohibited by EU regulations (Annex II) |
| Restricted | 3 | Concentration limits apply (Annex III) |
| Flagged | 2 | Not banned but problematic for specific conditions |
| Safe | 0 | Well-tolerated for the selected profile |
| Beneficial | 0 | Actively therapeutic for the condition |

### Ingredient Database
601 ingredients classified across 5 risk levels, covering:
- EU Annex II banned substances
- EU Annex III restricted substances
- Fragrance allergens (26 EU-listed)
- Endocrine disruptors (EDC)
- Condition-specific contraindications

---

## Key Findings

### 1. Fragrance is the universal risk
PARFUM/FRAGRANCE appears in **146 products** and is flagged across all 4 comorbid profiles. Among products claiming "dermatologist tested" or "sensitive", **11 contain fragrance allergens** — a direct label conflict.

### 2. Atopic Dermatitis + Acne is the most complex profile
Highest average conflict score (2.71), with **16 shared problematic ingredients** including alcohol, parabens, AHAs, retinoids, and surfactants.

### 3. Sunscreens carry the highest allergen load
Average fragrance allergen score: **3.44** (vs. 1.97 for cleansers, 1.01 for serums). Chemical UV filters (Butyl Methoxydibenzoylmethane + Octocrylene) co-occur in 41 products.

### 4. Zinc Oxide is the only universally safe UV filter
Safe and beneficial across all 4 comorbid profiles. Titanium Dioxide is safe for 3/4 profiles.

### 5. Niacinamide: the paradox ingredient
Beneficial for melasma and acne, but flagged for rosacea (flushing risk >4%). Appears in **142 products** — often without dose information.

---

## Clustering Results

4 clusters identified by ingredient profile:

| Cluster | n | Avg Risk | Dominant Category | Profile |
|---|---|---|---|---|
| 1 | 201 | 0.044 | Serums | Low risk, clean formulas |
| 2 | 144 | 0.092 | Mixed | Moderate risk |
| 3 | 100 | 0.357 | Sunscreens | Highest risk, fragrance-heavy |
| 4 | 190 | 0.099 | Mixed | Moderate risk |

---

## Shiny Dashboard

Interactive bilingual (TR/EN) dashboard with:
- **Skin Profile tab**: Select comorbid conditions → get avoid/safe/beneficial ingredient lists with frequency data
- **INCI Analysis tab**: Paste any product ingredient list → instant color-coded analysis
- **Ingredient Search**: Query by Turkish name, English name, or INCI
- **Product Database**: Filter 659 products by risk tier and category

---

## Limitations & Methodological Notes

1. **Ingredient DB coverage**: 601/3,025 unique ingredients classified (18.5%). Rare botanicals and biotechnology-derived ingredients are underrepresented. Pattern-based classification applied for common molecular families.

2. **No programmatic access to CosIng**: EU CosIng database requires JavaScript rendering; bulk download unavailable. Risk classifications derived from published Annex II/III lists and literature.

3. **Serum data source**: Serum category sourced from INCIdecoder (scraping) due to limited OBF coverage. Excluded from statistical analysis; available in Shiny app only.

4. **No dose information**: INCI lists do not include concentrations. Risk flags are presence-based, not dose-based (e.g., niacinamide flagged regardless of concentration).

5. **This is not medical advice**: For diagnosis and treatment, consult a dermatologist.

---

## Repository Structure
```
comorbid-skin-analytics-R/
├── R/
│   ├── 01_fetch.R
│   ├── 02_clean.R
│   ├── 03_risk_score.R
│   ├── 04_comorbid_analysis.R
│   ├── 05_clustering.R
│   ├── 06_nlp.R
│   └── 07_ggplot2_viz.R
├── app/
│   └── app.R
├── data/
│   ├── raw/
│   └── processed/
├── figures/
├── config.R
├── renv.lock
└── README.md
```

---

## How to Reproduce
```r
# 1. Clone and open project
# 2. Restore environment
renv::restore()

# 3. Run pipeline
source("R/01_fetch.R")          # ~5 min, API calls
source("R/02_clean.R")
source("R/03_risk_score.R")
source("R/04_comorbid_analysis.R")
source("R/05_clustering.R")
source("R/06_nlp.R")
source("R/07_ggplot2_viz.R")

# 4. Launch Shiny app
shiny::runApp("app/app.R")
```

---

## Author

**Reyhan Alhas**  
Data Scientist  
[GitHub](https://github.com/Ralhas) 

*Built with R 4.5 | Open data | Dermatology literature*

---

## License

MIT License — free to use with attribution.
