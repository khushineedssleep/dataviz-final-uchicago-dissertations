# ============================================================
# server.R
# A Century of Knowledge: Doctoral Production at UChicago
# MACS 40700 — Khushi Desai
# ============================================================

library(shiny)
library(tidyverse)
library(plotly)

# ── Load data ─────────────────────────────────────────────────
final <- read.csv("uchicago_dissertations_analysis.csv", stringsAsFactors = FALSE)

# ── Brand palette ─────────────────────────────────────────────
MAROON     <- "#800000"
GOLD       <- "#C16622"
DARK_GRAY  <- "#767676"
LIGHT_GRAY <- "#D6D6CE"

DIVISION_COLORS <- c(
  "Social Sciences"     = "#C16622",
  "Physical Sciences"   = "#350E20",
  "Biological Sciences" = "#6B2737",
  "Humanities"          = "#800000",
  "Divinity School"     = "#767676",
  "Professional Schools"= "#ADB5BD"
)

BASE_LAYOUT <- list(
  plot_bgcolor  = "white",
  paper_bgcolor = "white",
  font          = list(family = "'Gotham SSm', 'Gotham', 'Helvetica Neue', Helvetica, sans-serif",
                       size = 12),
  margin        = list(t = 30, b = 50, l = 60, r = 40)
)

# ── Pre-compute data ──────────────────────────────────────────

# Graph 1
g1_data <- final %>%
  filter(!is.na(Division_short), !is.na(Decade)) %>%
  count(Decade, Division_short)

# Graph 2 — all departments (real + imputed), canonical division via lookup
dept_home_division <- final %>%
  filter(
    !is.na(Department_norm) & Department_norm != "",
    !is.na(Division_short)
  ) %>%
  count(Department_clean, Division_short) %>%
  group_by(Department_clean) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Department_clean, home_division = Division_short) %>%
  mutate(home_division = case_when(
    Department_clean == "Computer Science"        ~ "Physical Sciences",
    Department_clean == "Molecular Engineering"   ~ "Physical Sciences",
    Department_clean == "Business"                ~ "Social Sciences",
    Department_clean == "Public Policy Studies"   ~ "Social Sciences",
    Department_clean == "Social Service Administration" ~ "Social Sciences",
    Department_clean == "Statistics"              ~ "Physical Sciences",
    TRUE ~ home_division
  ))

g2_data <- final %>%
  filter(!is.na(Department_clean), !is.na(Decade)) %>%
  left_join(dept_home_division, by = "Department_clean") %>%
  filter(!is.na(home_division), home_division != "Professional Schools")

divisions  <- sort(unique(na.omit(g2_data$home_division)))
all_decades <- c("1893–1902","1903–1912","1913–1922","1923–1932",
                 "1933–1942","1943–1952","1953–1962","1963–1972",
                 "1973–1982","1983–1992","1993–2002","2003–2012","2013–2022")

# Graph 3
g3_data <- final %>%
  filter(!is.na(Division_short), !is.na(Decade)) %>%
  count(Decade, Division_short)

# Graph 4 — top 10 keywords per department per decade
# Official UChicago departments — only those with 8+ decades of keyword data
official_dept_map <- c(
  "Anthropology"                        = "Anthropology",
  "Astronomy and Astrophysics"          = "Astronomy and Astrophysics",
  "Biochemistry and Molecular Biology"  = "Biochemistry and Molecular Biology",
  "Chemistry"                           = "Chemistry",
  "Classics"                            = "Classics",
  "Divinity"                            = "Divinity School",
  "Ecology and Evolution"               = "Ecology and Evolution",
  "Economics"                           = "Economics",
  "Education"                           = "Education",
  "English Language and Literature"     = "English Language and Literature",
  "Geophysical Sciences"                = "Geophysical Sciences",
  "History"                             = "History",
  "Linguistics"                         = "Linguistics",
  "Mathematics"                         = "Mathematics",
  "Microbiology"                        = "Microbiology",
  "Music"                               = "Music",
  "Organismal Biology and Anatomy"      = "Organismal Biology and Anatomy",
  "Philosophy"                          = "Philosophy",
  "Physics"                             = "Physics",
  "Political Science"                   = "Political Science",
  "Psychology"                          = "Psychology",
  "Social Service Administration"       = "Social Service Administration",
  "Sociology"                           = "Sociology"
)

# Parse Subject.Terms into long format, filter to official depts
kw_raw <- final %>%
  filter(
    !is.na(Subject.Terms), Subject.Terms != "",
    !is.na(Department_clean), !is.na(Decade),
    Department_clean %in% names(official_dept_map)
  ) %>%
  mutate(dept_label = official_dept_map[Department_clean]) %>%
  mutate(Subject.Terms = str_remove_all(Subject.Terms, "\\[|\\]|\'")) %>%
  mutate(term = strsplit(Subject.Terms, ",\\s*")) %>%
  unnest(term) %>%
  mutate(term = str_trim(str_to_lower(term))) %>%
  filter(nchar(term) > 2)

# Only keep departments with 8+ decades of keyword data
dept_decade_coverage <- kw_raw %>%
  count(dept_label, Decade) %>%
  count(dept_label, name = "n_decades")

kw_top_depts <- dept_decade_coverage %>%
  filter(n_decades >= 8) %>%
  arrange(desc(n_decades)) %>%
  pull(dept_label)

# Top 10 keywords per dept per decade, excluding dept name words
dept_decade_kws <- map_dfr(kw_top_depts, function(dept) {
  dept_words <- unlist(str_split(str_to_lower(dept), "\\s+"))
  dept_words <- dept_words[nchar(dept_words) > 3]
  exclude    <- c(str_to_lower(dept), dept_words)
  
  kw_raw %>%
    filter(dept_label == dept) %>%
    filter(!term %in% exclude) %>%
    count(Decade, term) %>%
    group_by(Decade) %>%
    slice_max(n, n = 10, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Department_clean = dept)
})

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Populate keyword dept dropdown ───────────────────────
  observe({
    updateSelectInput(session, "kw_dept_select",
                      choices  = sort(kw_top_depts),
                      selected = if ("History" %in% kw_top_depts) "History" else kw_top_depts[1]
    )
  })
  
  # ── Figure 1: Stacked bar ──────────────────────────────────
  output$fig1 <- renderPlotly({
    plot_ly(
      g1_data,
      x = ~Decade, y = ~n,
      color = ~Division_short,
      colors = DIVISION_COLORS,
      type = "bar",
      hovertemplate = "<b>%{fullData.name}</b><br>%{x}<br>%{y} dissertations<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis   = list(title = "Decade", tickangle = -35, gridcolor = LIGHT_GRAY,
                       tickfont = list(size = 11)),
        yaxis   = list(title = "Dissertations", gridcolor = LIGHT_GRAY),
        legend  = list(title = list(text = "<b>Division</b>"),
                       bgcolor = "white", bordercolor = LIGHT_GRAY),
        plot_bgcolor  = BASE_LAYOUT$plot_bgcolor,
        paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
        font          = BASE_LAYOUT$font,
        margin        = BASE_LAYOUT$margin
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Figure 2: Top 10 depts, selected decade + division ───────
  output$fig2 <- renderPlotly({
    
    div_sel    <- input$division_select
    # Map upper bound year to decade label
    decade_sel <- all_decades[match(input$decade_slider,
                                    c(1902,1912,1922,1932,1942,1952,1962,1972,1982,1992,2002,2012,2022))]
    
    # Top 10 departments for selected division + decade
    dept_data <- g2_data %>%
      filter(home_division == div_sel, Decade == decade_sel) %>%
      count(Department_clean) %>%
      slice_max(n, n = 10, with_ties = FALSE) %>%
      arrange(n) %>%
      mutate(Department_clean = fct_inorder(Department_clean))
    
    validate(need(nrow(dept_data) > 0, "No data for this selection."))
    
    # Maroon color scale light -> dark by count
    maroon_pal <- colorRampPalette(c("#f0b8b8", MAROON))(nrow(dept_data))
    
    plot_ly(
      dept_data,
      x = ~n,
      y = ~Department_clean,
      type = "bar",
      orientation = "h",
      marker = list(color = maroon_pal),
      hovertemplate = "<b>%{y}</b><br>%{x} dissertations<extra></extra>",
      showlegend = FALSE
    ) %>%
      layout(
        title  = list(
          text = paste0("<b>Top 10 Departments · ", div_sel, " · ", decade_sel, "</b>"),
          font = list(color = MAROON, size = 14), x = 0
        ),
        xaxis  = list(title = "Dissertations", gridcolor = LIGHT_GRAY, zeroline = FALSE),
        yaxis  = list(title = "", automargin = TRUE),
        plot_bgcolor  = BASE_LAYOUT$plot_bgcolor,
        paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
        font          = BASE_LAYOUT$font,
        margin = list(l = 240, r = 40, t = 50, b = 50)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Figure 3: Line chart ───────────────────────────────────
  output$fig3 <- renderPlotly({
    plot_ly(
      g3_data,
      x = ~Decade, y = ~n,
      color = ~Division_short,
      colors = DIVISION_COLORS,
      type = "scatter", mode = "lines+markers",
      hovertemplate = "<b>%{fullData.name}</b><br>%{x}: %{y} dissertations<extra></extra>"
    ) %>%
      layout(
        xaxis  = list(title = "Decade", tickangle = -35, gridcolor = LIGHT_GRAY),
        yaxis  = list(title = "Dissertations per Decade", gridcolor = LIGHT_GRAY),
        legend = list(title = list(text = "<b>Division</b>"),
                      bgcolor = "white", bordercolor = LIGHT_GRAY),
        plot_bgcolor  = BASE_LAYOUT$plot_bgcolor,
        paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
        font          = BASE_LAYOUT$font,
        margin        = BASE_LAYOUT$margin
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Figure 4: Top 10 keywords per dept per decade ──────────
  output$fig4 <- renderPlotly({
    
    dept_sel <- input$kw_dept_select
    
    plot_data <- dept_decade_kws %>%
      filter(Department_clean == dept_sel)
    
    validate(need(nrow(plot_data) > 0, "No keyword data for this department."))
    
    # Get all unique keywords across all decades for this dept
    all_kws <- plot_data %>%
      group_by(term) %>%
      summarise(total = sum(n)) %>%
      slice_max(total, n = 10, with_ties = FALSE) %>%
      arrange(desc(total)) %>%
      pull(term)
    
    plot_data <- plot_data %>% filter(term %in% all_kws)
    
    kw_colors <- colorRampPalette(c(MAROON, GOLD, "#4a90a4", "#2d6a4f",
                                    "#e76f51", "#9b2226", "#606c38", "#7b2d8b",
                                    "#6a0572", "#264653"))(length(all_kws))
    names(kw_colors) <- all_kws
    
    fig4 <- plot_ly()
    
    for (i in seq_along(all_kws)) {
      kw_sel <- all_kws[i]
      kw_d   <- plot_data %>% filter(term == kw_sel) %>% arrange(Decade)
      if (nrow(kw_d) < 1) next
      
      fig4 <- fig4 %>% add_trace(
        x    = kw_d$Decade,
        y    = kw_d$n,
        type = "scatter", mode = "lines+markers",
        name = kw_sel,
        line   = list(color = kw_colors[kw_sel], width = 2.5, shape = "spline"),
        marker = list(color = kw_colors[kw_sel], size = 7),
        hovertemplate = paste0("<b>", kw_sel, "</b><br>%{x}<br>%{y} occurrences<extra></extra>")
      )
    }
    
    fig4 %>%
      layout(
        title  = list(
          text = paste0("<b>Keyword Trends · ", dept_sel, " · 1893–2022</b>"),
          font = list(color = MAROON, size = 14), x = 0
        ),
        xaxis  = list(title = "Decade", tickangle = -35, gridcolor = LIGHT_GRAY),
        yaxis  = list(title = "Frequency / Occurrence", gridcolor = LIGHT_GRAY),
        legend = list(title = list(text = "<b>Keyword</b>"),
                      bgcolor = "white", bordercolor = LIGHT_GRAY),
        plot_bgcolor  = BASE_LAYOUT$plot_bgcolor,
        paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
        font          = BASE_LAYOUT$font,
        margin = list(l = 60, r = 180, t = 50, b = 80)
      ) %>%
      config(displayModeBar = FALSE)
  })
}