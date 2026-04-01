# ── app/app.R ────────────────────────────────────────────────
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)

# ── Veri ─────────────────────────────────────────────────────
df_scored        <- readRDS("../data/processed/df_scored.rds")
ingredient_db    <- readRDS("../data/processed/ingredient_db_full.rds")
product_risk     <- readRDS("../data/processed/product_risk.rds")
condition_map    <- readRDS("../data/processed/condition_map.rds")
condition_parent <- readRDS("../data/processed/condition_parent_map.rds")
inci_aliases     <- readRDS("../data/processed/inci_aliases.rds")
total_products   <- n_distinct(df_scored$product_id)

# ── Renkler ───────────────────────────────────────────────────
COL <- list(
  primary    = "#9C7BB5",
  accent     = "#C9B8E8",
  background = "#FAF5FF",
  danger     = "#F48A8A",
  warning    = "#FFE082",
  safe       = "#81C784",
  unknown    = "#B0BEC5",
  beneficial = "#CE93D8",
  text_dark  = "#4A235A",
  card_bg    = "#FFFFFF"
)

# ── Dil ───────────────────────────────────────────────────────
LABELS <- list(
  TR = list(
    title         = "Cilt İçerik Analizörü",
    tab_profile   = "Cilt Profili",
    tab_analyze   = "İçerik Analizi",
    tab_search    = "İçerik Sorgula",
    tab_products  = "Ürün Veritabanı",
    tab_about     = "Hakkında",
    select_cond   = "Cilt durumlarınızı seçin",
    paste_inci    = "INCI içerik listesini yapıştırın",
    paste_hint    = "Aqua, Glycerin, Niacinamide, Parfum...",
    btn_analyze   = "Analiz Et",
    btn_search    = "Sorgula",
    search_hint   = "niasinamid / NIACINAMIDE / meyan kökü...",
    lbl_avoid     = "Kaçınılacak İçerikler",
    lbl_safe      = "Güvenli İçerikler",
    lbl_unknown   = "Tanımlanamayan İçerikler",
    lbl_total     = "Toplam İçerik",
    lbl_problem   = "Problemli",
    lbl_good      = "Güvenli",
    lbl_unknown2  = "Bilinmiyor",
    lbl_score     = "Risk Skoru",
    lbl_mechanism = "Mekanizma",
    lbl_pct_prob  = "Bu ürün profiliniz için",
    lbl_problemli = "PROBLEMLİ",
    lbl_guvenli   = "GÜVENLİ",
    lbl_bilinmiyor= "BİLİNMİYOR",
    about_text    = "Kozmetik ürünlerin INCI içerik listelerini analiz ederek komorbid cilt durumları için risk ve sinerji profili çıkarır.",
    disclaimer    = "Bu uygulama tıbbi tavsiye niteliği taşımaz."
  ),
  EN = list(
    title         = "Skin Ingredient Analyzer",
    tab_profile   = "Skin Profile",
    tab_analyze   = "Ingredient Analysis",
    tab_search    = "Search Ingredient",
    tab_products  = "Product Database",
    tab_about     = "About",
    select_cond   = "Select your skin conditions",
    paste_inci    = "Paste INCI ingredient list",
    paste_hint    = "Aqua, Glycerin, Niacinamide, Parfum...",
    btn_analyze   = "Analyze",
    btn_search    = "Search",
    search_hint   = "niacinamide / NIACINAMIDE / licorice root...",
    lbl_avoid     = "Ingredients to Avoid",
    lbl_safe      = "Safe Ingredients",
    lbl_unknown   = "Unrecognized Ingredients",
    lbl_total     = "Total Ingredients",
    lbl_problem   = "Problematic",
    lbl_good      = "Safe",
    lbl_unknown2  = "Unknown",
    lbl_score     = "Risk Score",
    lbl_mechanism = "Mechanism",
    lbl_pct_prob  = "For your profile, this product is",
    lbl_problemli = "PROBLEMATIC",
    lbl_guvenli   = "SAFE",
    lbl_bilinmiyor= "UNKNOWN",
    about_text    = "Analyzes cosmetic product INCI ingredient lists to generate risk and synergy profiles for comorbid skin conditions.",
    disclaimer    = "This application does not constitute medical advice."
  )
)

# ── Yardımcı ─────────────────────────────────────────────────
resolve_conditions <- function(selected_en) {
  direct     <- selected_en[selected_en %in% c("acne","rosacea","melasma","atopic_dermatitis")]
  via_parent <- condition_parent |>
    filter(condition_en %in% selected_en) |>
    pull(maps_to_condition)
  unique(c(direct, via_parent))
}

resolve_alias <- function(query) {
  q   <- str_to_lower(str_squish(query))
  hit <- inci_aliases |> filter(str_to_lower(alias) == q) |> pull(inci_name)
  if (length(hit) > 0) hit[1] else str_to_upper(q)
}

parse_inci_input <- function(text) {
  result <- text |>
    str_to_upper() |>
    str_remove_all("\\([^)]*\\)") |>
    str_remove_all("\\[[^]]*\\]") |>
    str_remove_all("(?i)(may contain|peut contenir|[+]/-|±).*$")
  result <- unlist(str_split(result, "[,;\n]"))
  result <- str_squish(result)
  result[nchar(result) > 2]
}

get_avoid <- function(conds) {
  ingredient_db |>
    mutate(cond_list = str_split(conditions, ";")) |>
    unnest(cond_list) |>
    rename(condition = cond_list) |>
    filter(condition %in% conds, risk_score > 0) |>
    group_by(ingredient, risk_level, risk_score, mechanism) |>
    summarise(matched = paste(unique(condition), collapse = " + "), .groups = "drop") |>
    arrange(desc(risk_score))
}

get_safe <- function(conds) {
  ingredient_db |>
    mutate(cond_list = str_split(conditions, ";")) |>
    unnest(cond_list) |>
    rename(condition = cond_list) |>
    filter(condition %in% conds, risk_score == 0) |>
    group_by(ingredient, mechanism) |>
    summarise(matched = paste(unique(condition), collapse = " + "), .groups = "drop")
}

classify_ingredient <- function(ingredient, avoid_ings, safe_ings) {
  if (ingredient %in% avoid_ings$ingredient) {
    score <- avoid_ings$risk_score[avoid_ings$ingredient == ingredient][1]
    level <- avoid_ings$risk_level[avoid_ings$ingredient == ingredient][1]
    if (level %in% c("banned","banned_otc") || score >= 4) return("banned")
    if (level == "restricted" || score >= 3) return("restricted")
    return("flagged")
  }
  if (ingredient %in% safe_ings$ingredient) return("safe")
  return("unknown")
}

color_for_class <- function(cls) {
  switch(cls,
    banned     = "#F48A8A",
    restricted = "#FFAB91",
    flagged    = "#FFE082",
    safe       = "#81C784",
    unknown    = "#B0BEC5"
  )
}

badge_html <- function(ingredient, cls, mechanism = "") {
  col <- color_for_class(cls)
  tip <- if (nchar(mechanism) > 0) paste0(" title=\"", mechanism, "\"") else ""
  sprintf(
    '<span%s style="background:%s;padding:4px 10px;border-radius:12px;
    font-size:12px;font-weight:600;color:#333;display:inline-block;
    margin:2px;cursor:default">%s</span>',
    tip, col, ingredient
  )
}

group_labels <- c(
  rosacea_group      = "1. Rosacea & Vascular",
  pigmentation_group = "2. Pigmentation",
  barrier_group      = "3. Barrier & Sensitivity",
  acne_group         = "4. Acne",
  sebum_group        = "5. Sebum & Pores",
  aging_group        = "6. Aging",
  other              = "7. Other"
)

group_labels_tr <- c(
  rosacea_group      = "1. Rozasea & Vasküler",
  pigmentation_group = "2. Pigmentasyon",
  barrier_group      = "3. Bariyer & Hassasiyet",
  acne_group         = "4. Akne",
  sebum_group        = "5. Sebum & Gözenek",
  aging_group        = "6. Yaşlanma",
  other              = "7. Diğer"
)

cond_choices_en <- split(
  setNames(condition_map$condition_en, condition_map$condition_tr),
  sapply(condition_map$parent_group, function(g) group_labels[[g]])
)

cond_choices_tr <- split(
  setNames(condition_map$condition_en, condition_map$condition_tr),
  sapply(condition_map$parent_group, function(g) group_labels_tr[[g]])
)

# ── CSS ──────────────────────────────────────────────────────
app_css <- sprintf('
  body { background-color: %s; font-family: "Helvetica Neue", sans-serif; }
  .navbar { background-color: %s !important; border-color: %s !important; }
  .navbar-brand, .navbar-nav > li > a {
    color: white !important; font-weight: 600; }
  .navbar-nav > li > a:hover { background-color: %s !important; }
  .navbar-nav > .active > a {
    background-color: %s !important; color: white !important; }
  .well {
    background-color: white;
    border: 1px solid %s;
    border-radius: 12px;
    box-shadow: 0 2px 8px rgba(156,123,181,0.12);
  }
  .btn-primary { background-color: %s; border-color: %s; font-weight: 600; }
  .btn-primary:hover { background-color: %s; border-color: %s; }
  .summary-card {
    border-radius: 12px; padding: 16px; text-align: center;
    box-shadow: 0 2px 8px rgba(0,0,0,0.08); margin: 4px;
  }
  .summary-card h2 { margin: 4px 0; font-size: 2em; font-weight: 700; }
  .summary-card p  { margin: 0; font-size: 12px; font-weight: 600; letter-spacing: 1px; }
  .progress-bar-custom {
    height: 28px; border-radius: 14px; overflow: hidden;
    display: flex; margin: 8px 0;
  }
  .section-title {
    color: %s; font-weight: 700; font-size: 16px;
    border-bottom: 2px solid %s; padding-bottom: 6px; margin-bottom: 12px;
  }
  .lang-btn { position: fixed; top: 12px; right: 16px; z-index: 9999; }
  .ingredient-grid { line-height: 2.2; }
  table.dataTable thead th { background-color: %s; color: white; }
',
  COL$background,
  COL$primary, COL$primary,
  COL$accent,
  COL$text_dark,
  COL$accent,
  COL$primary, COL$primary,
  COL$text_dark, COL$text_dark,
  COL$text_dark, COL$accent,
  COL$primary
)

# ── UI ───────────────────────────────────────────────────────
ui <- tagList(
  tags$head(tags$style(HTML(app_css))),
  div(class = "lang-btn",
    actionButton("toggle_lang", "TR / EN",
      style = sprintf("background:%s;color:white;border:none;border-radius:8px;
                       font-weight:600;padding:6px 14px;", COL$primary))
  ),
  uiOutput("main_ui")
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {

  lang <- reactiveVal("EN")

  # Enter ile analiz
  observeEvent(input$ing_query, {
    if (!is.null(input$ing_query) && 
        endsWith(input$ing_query, "\n")) {
      shinyjs::click("btn_ing")
    }
  })

  observeEvent(input$toggle_lang, {
    lang(if (lang() == "EN") "TR" else "EN")
  })

  L <- reactive({ LABELS[[lang()]] })
  cond_choices <- reactive({
    if (lang() == "TR") cond_choices_tr else cond_choices_en
  })

  output$main_ui <- renderUI({
    l <- L()
    navbarPage(
      title = l$title,
      id    = "navbar",

      # ── Tab 1: Skin Profile ──────────────────────────────
      tabPanel(l$tab_profile,
        br(),
        fluidRow(
          column(12, wellPanel(
            div(class = "section-title", l$select_cond),
            selectInput("profile_conds", label = NULL,
              choices  = cond_choices(),
              multiple = TRUE,
              selected = NULL,
              width    = "100%"),
            actionButton("btn_profile", l$btn_analyze,
                         class = "btn-primary btn-lg")
          ))
        ),
        fluidRow(
          column(6, wellPanel(
            div(class = "section-title", style = "color:#F48A8A",
                l$lbl_avoid),
            DTOutput("tbl_profile_avoid")
          )),
          column(6, wellPanel(
            div(class = "section-title", style = "color:#43A047",
                l$lbl_safe),
            DTOutput("tbl_profile_safe")
          ))
        ),
        fluidRow(
          column(12, wellPanel(
            div(class = "section-title",
                "Frequency in product database"),
            DTOutput("tbl_freq")
          ))
        )
      ),

      # ── Tab 2: Ingredient Analysis ───────────────────────
      tabPanel(l$tab_analyze,
        br(),
        fluidRow(
          column(5, wellPanel(
            div(class = "section-title", l$paste_inci),
            textAreaInput("inci_text", label = NULL,
              placeholder = l$paste_hint,
              rows = 10, width = "100%"),
            div(class = "section-title", l$select_cond),
            selectInput("inci_conds", label = NULL,
              choices  = cond_choices(),
              multiple = TRUE,
              selected = NULL,
              width    = "100%"),
            actionButton("btn_inci", l$btn_analyze,
                         class = "btn-primary btn-lg")
          )),
          column(7,
            uiOutput("inci_summary"),
            br(),
            uiOutput("inci_badges"),
            br(),
            uiOutput("inci_unknown_box")
          )
        ),
        br(),
        fluidRow(
          column(6, wellPanel(
            div(class = "section-title", style="color:#F48A8A",
                l$lbl_avoid),
            DTOutput("tbl_inci_avoid")
          )),
          column(6, wellPanel(
            div(class = "section-title", style="color:#43A047",
                l$lbl_safe),
            DTOutput("tbl_inci_safe")
          ))
        )
      ),

      # ── Tab 3: Search ────────────────────────────────────
      tabPanel(l$tab_search,
        br(),
        fluidRow(
          column(8,
            textInput("ing_query", label = NULL,
              placeholder = l$search_hint, width = "100%")),
          column(4, br(),
            actionButton("btn_ing", l$btn_search,
                         class = "btn-primary btn-lg"))
        ),
        fluidRow(
          column(12, wellPanel(
            DTOutput("tbl_ing_detail"),
            br(),
            DTOutput("tbl_ing_products")
          ))
        )
      ),

      # ── Tab 4: Products ──────────────────────────────────
      tabPanel(l$tab_products,
        br(),
        fluidRow(
          column(3, selectInput("prod_cat", "Category:",
            choices  = c("All","sunscreens","cleansers","serums"),
            selected = "All")),
          column(3, selectInput("prod_tier", "Risk tier:",
            choices  = c("All","HIGH","MEDIUM","LOW","MINIMAL"),
            selected = "All")),
          column(6, textInput("prod_q", "Search product / brand:",
            placeholder = "Nivea, La Roche-Posay..."))
        ),
        DTOutput("tbl_products")
      ),

      # ── Tab 5: About ─────────────────────────────────────
      tabPanel(l$tab_about,
        br(),
        fluidRow(column(8, offset = 2, wellPanel(
          h3(l$title),
          p(l$about_text),
          h4("Data sources:"),
          tags$ul(
            tags$li("Open Beauty Facts"),
            tags$li("EU CosIng database"),
            tags$li("EWG Skin Deep"),
            tags$li("Dermatology literature")
          ),
          hr(),
          p(em(l$disclaimer))
        )))
      )
    )
  })

  # ── Profile tab ─────────────────────────────────────────
  profile_res <- eventReactive(input$btn_profile, {
    req(input$profile_conds)
    conds <- resolve_conditions(input$profile_conds)
    list(avoid = get_avoid(conds), safe = get_safe(conds), conds = conds)
  })

  output$tbl_profile_avoid <- renderDT({
    req(profile_res())
    profile_res()$avoid |>
      select(INCI = ingredient, Risk = risk_level,
             Score = risk_score, Mechanism = mechanism) |>
      datatable(options = list(pageLength = 10, dom = "tp"),
                rownames = FALSE) |>
      formatStyle("Risk", backgroundColor = styleEqual(
        c("banned","banned_otc","restricted","flagged"),
        c("#F48A8A","#F48A8A","#FFAB91","#FFE082")))
  })

  output$tbl_profile_beneficial <- renderDT({
    req(profile_res())
    ben <- get_beneficial(profile_res()$conds)
    if (nrow(ben) == 0) {
      datatable(data.frame(Result = "No beneficial ingredients found"),
                rownames = FALSE)
    } else {
      ben |>
        select(INCI = ingredient, Mechanism = mechanism,
               "For conditions" = matched) |>
        datatable(options = list(pageLength = 10, dom = "tp"),
                  rownames = FALSE)
    }
  })

  output$tbl_profile_safe <- renderDT({
    req(profile_res())
    profile_res()$safe |>
      select(INCI = ingredient, Mechanism = mechanism) |>
      datatable(options = list(pageLength = 10, dom = "tp"),
                rownames = FALSE)
  })

  output$tbl_freq <- renderDT({
    req(profile_res())
    avoid_ings <- profile_res()$avoid$ingredient
    safe_ings  <- profile_res()$safe$ingredient
    all_ings   <- c(avoid_ings, safe_ings)

    df_scored |>
      filter(ingredient %in% all_ings) |>
      group_by(ingredient) |>
      summarise(n_products = n_distinct(product_id), .groups = "drop") |>
      left_join(
        bind_rows(
          profile_res()$avoid |>
            select(ingredient, risk_level, risk_score, mechanism),
          profile_res()$safe |>
            mutate(risk_level = "safe", risk_score = 0) |>
            select(ingredient, risk_level, risk_score, mechanism)
        ), by = "ingredient") |>
      mutate(
        pct      = round(n_products / total_products * 100, 1),
        Frequency = paste0(n_products, " products (", pct, "%)")
      ) |>
      arrange(desc(risk_score), desc(n_products)) |>
      select(INCI = ingredient, Risk = risk_level,
             Score = risk_score, Frequency, Mechanism = mechanism) |>
      datatable(options = list(pageLength = 15, scrollX = TRUE, dom = "tp"),
                rownames = FALSE) |>
      formatStyle("Risk", backgroundColor = styleEqual(
        c("banned","banned_otc","restricted","flagged","safe"),
        c("#F48A8A","#F48A8A","#FFAB91","#FFE082","#81C784"))) |>
      formatStyle("Score", fontWeight = "bold",
        color = styleInterval(c(0,1,3), c("#43A047","#F9A825","#E65100","#C62828")))
  })

  # ── INCI Analysis tab ───────────────────────────────────
  inci_res <- eventReactive(input$btn_inci, {
    req(input$inci_text, input$inci_conds)
    ingredients <- parse_inci_input(input$inci_text)
    conds       <- resolve_conditions(input$inci_conds)
    avoid_all   <- get_avoid(conds)
    safe_all    <- get_safe(conds)

    classified <- lapply(ingredients, function(ing) {
      cls  <- classify_ingredient(ing, avoid_all, safe_all)
      mech <- ""
      if (cls %in% c("banned","restricted","flagged")) {
        idx  <- which(avoid_all$ingredient == ing)
        if (length(idx) > 0) mech <- avoid_all$mechanism[idx[1]]
      } else if (cls == "safe") {
        idx  <- which(safe_all$ingredient == ing)
        if (length(idx) > 0) mech <- safe_all$mechanism[idx[1]]
      }
      list(ingredient = ing, class = cls, mechanism = mech)
    })

    n_total      <- length(ingredients)
    n_banned     <- sum(sapply(classified, function(x) x$class == "banned"))
    n_restricted <- sum(sapply(classified, function(x) x$class == "restricted"))
    n_flagged    <- sum(sapply(classified, function(x) x$class == "flagged"))
    n_safe       <- sum(sapply(classified, function(x) x$class == "safe"))
    n_unknown    <- sum(sapply(classified, function(x) x$class == "unknown"))
    n_problem    <- n_banned + n_restricted + n_flagged

    pct_problem  <- round(n_problem  / n_total * 100)
    pct_safe     <- round(n_safe     / n_total * 100)
    pct_unknown  <- round(n_unknown  / n_total * 100)

    unknown_ings <- sapply(
      Filter(function(x) x$class == "unknown", classified),
      function(x) x$ingredient)

    list(
      classified   = classified,
      n_total      = n_total,
      n_problem    = n_problem,
      n_safe       = n_safe,
      n_unknown    = n_unknown,
      pct_problem  = pct_problem,
      pct_safe     = pct_safe,
      pct_unknown  = pct_unknown,
      unknown_ings = unknown_ings,
      avoid_all    = avoid_all,
      safe_all     = safe_all
    )
  })

  output$inci_summary <- renderUI({
    req(inci_res())
    r <- inci_res()
    l <- L()

    tagList(
      fluidRow(
        column(3, div(class = "summary-card",
          style = sprintf("background:%s", COL$unknown),
          tags$h2(r$n_total), tags$p(l$lbl_total))),
        column(3, div(class = "summary-card",
          style = sprintf("background:%s", COL$danger),
          tags$h2(paste0(r$pct_problem, "%")),
          tags$p(l$lbl_problemli))),
        column(3, div(class = "summary-card",
          style = sprintf("background:%s", COL$safe),
          tags$h2(paste0(r$pct_safe, "%")),
          tags$p(l$lbl_guvenli))),
        column(3, div(class = "summary-card",
          style = sprintf("background:%s", COL$unknown),
          tags$h2(paste0(r$pct_unknown, "%")),
          tags$p(l$lbl_bilinmiyor)))
      ),
      br(),
      div(class = "progress-bar-custom",
        div(style = sprintf(
          "width:%s%%;background:%s;display:flex;align-items:center;
           justify-content:center;color:white;font-weight:700;font-size:13px",
          r$pct_problem, COL$danger),
          if (r$pct_problem > 8) paste0(r$pct_problem, "%") else ""),
        div(style = sprintf(
          "width:%s%%;background:%s;display:flex;align-items:center;
           justify-content:center;color:white;font-weight:700;font-size:13px",
          r$pct_safe, COL$safe),
          if (r$pct_safe > 8) paste0(r$pct_safe, "%") else ""),
        div(style = sprintf(
          "width:%s%%;background:%s;display:flex;align-items:center;
           justify-content:center;color:#555;font-weight:700;font-size:13px",
          r$pct_unknown, COL$unknown),
          if (r$pct_unknown > 8) paste0(r$pct_unknown, "%") else "")
      )
    )
  })

  output$inci_badges <- renderUI({
    req(inci_res())
    r <- inci_res()
    badges <- sapply(r$classified, function(x) {
      badge_html(x$ingredient, x$class, x$mechanism)
    })
    div(class = "ingredient-grid well",
      div(class = "section-title", "Ingredient Profile"),
      HTML(paste(badges, collapse = " "))
    )
  })

  output$inci_unknown_box <- renderUI({
    req(inci_res())
    r <- inci_res()
    if (length(r$unknown_ings) == 0) return(NULL)
    div(class = "well",
      div(class = "section-title", style = "color:#9E9E9E",
          sprintf("Unrecognized ingredients (%d)", length(r$unknown_ings))),
      p(style = "color:#9E9E9E; font-size:12px",
        "These ingredients are not in our database. They are shown in grey above."),
      p(paste(r$unknown_ings, collapse = ", "))
    )
  })

  output$tbl_inci_avoid <- renderDT({
    req(inci_res())
    r <- inci_res()
    found <- r$avoid_all |>
      filter(ingredient %in% sapply(
        Filter(function(x) x$class != "safe" & x$class != "unknown",
               r$classified), function(x) x$ingredient))
    if (nrow(found) == 0) {
      datatable(data.frame(Result = "No problematic ingredients found"),
                rownames = FALSE)
    } else {
      found |>
        select(INCI = ingredient, Risk = risk_level,
               Score = risk_score, Mechanism = mechanism) |>
        datatable(options = list(pageLength = 10, dom = "tp"),
                  rownames = FALSE) |>
        formatStyle("Risk", backgroundColor = styleEqual(
          c("banned","banned_otc","restricted","flagged"),
          c("#F48A8A","#F48A8A","#FFAB91","#FFE082")))
    }
  })

  output$tbl_inci_safe <- renderDT({
    req(inci_res())
    r <- inci_res()
    found <- r$safe_all |>
      filter(ingredient %in% sapply(
        Filter(function(x) x$class == "safe", r$classified),
        function(x) x$ingredient))
    if (nrow(found) == 0) {
      datatable(data.frame(Result = "No recognized safe ingredients"),
                rownames = FALSE)
    } else {
      found |>
        select(INCI = ingredient, Mechanism = mechanism) |>
        datatable(options = list(pageLength = 10, dom = "tp"),
                  rownames = FALSE)
    }
  })

  # ── Search tab ──────────────────────────────────────────
  ing_res <- eventReactive(input$btn_ing, {
    req(input$ing_query)
    inci_name <- resolve_alias(input$ing_query)
    detail    <- ingredient_db |>
      filter(str_detect(toupper(ingredient),
                        toupper(str_squish(inci_name))))
    products  <- df_scored |>
      filter(str_detect(toupper(ingredient),
                        toupper(str_squish(inci_name)))) |>
      distinct(product_id, product_name, brands, source_category) |>
      head(50)
    list(detail = detail, products = products)
  })

  output$tbl_ing_detail <- renderDT({
    req(ing_res())
    if (nrow(ing_res()$detail) == 0) {
      datatable(data.frame(Result = "Not found"), rownames = FALSE)
    } else {
      ing_res()$detail |>
        datatable(options = list(pageLength = 5, scrollX = TRUE),
                  rownames = FALSE) |>
        formatStyle("risk_level", backgroundColor = styleEqual(
          c("banned","banned_otc","restricted","flagged","safe"),
          c("#F48A8A","#F48A8A","#FFAB91","#FFE082","#81C784")))
    }
  })

  output$tbl_ing_products <- renderDT({
    req(ing_res())
    ing_res()$products |>
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })

  # ── Products tab ────────────────────────────────────────
  output$tbl_products <- renderDT({
    data <- product_risk
    if (input$prod_cat != "All")
      data <- data |> filter(source_category == input$prod_cat)
    if (input$prod_tier != "All")
      data <- data |> filter(risk_tier == input$prod_tier)
    if (nchar(trimws(input$prod_q)) > 0) {
      q    <- tolower(trimws(input$prod_q))
      data <- data |>
        filter(str_detect(tolower(paste(product_name, brands)), q))
    }
    data |>
      arrange(desc(total_risk_score)) |>
      select(Product = product_name, Brand = brands,
             Category = source_category,
             N_Ingredients = n_ingredients,
             Flagged = n_flagged, Restricted = n_restricted,
             Banned = n_banned, Total_Risk = total_risk_score,
             Tier = risk_tier) |>
      datatable(options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE) |>
      formatStyle("Tier", backgroundColor = styleEqual(
        c("HIGH","MEDIUM","LOW","MINIMAL"),
        c("#F48A8A","#FFAB91","#FFE082","#81C784")))
  })
}

shinyApp(ui, server)
