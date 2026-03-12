# ============================================================
# server.R
# A Century of Knowledge: Doctoral Production at UChicago
# MACS 40700 — Khushi Desai
# ============================================================

library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(scales)

# Force dplyr::filter to win over plotly::filter globally
filter <- dplyr::filter

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
  "Biological Sciences" = "#8B4513",
  "Humanities"          = "#800000",
  "Divinity School"     = "#A0785A",
  "Professional Schools"= "#D6D6CE"
)

BASE_LAYOUT <- list(
  plot_bgcolor  = "white",
  paper_bgcolor = "white",
  font          = list(family = "'Gotham SSm', 'Gotham', 'Helvetica Neue', Helvetica, sans-serif",
                       size = 12),
  margin        = list(t = 30, b = 50, l = 60, r = 40)
)

# ── Pre-compute data ──────────────────────────────────────────

g1_data <- final %>%
  filter(!is.na(Division_short), !is.na(Decade)) %>%
  count(Decade, Division_short)

dept_home_division <- final %>%
  filter(!is.na(Department_norm) & Department_norm != "", !is.na(Division_short)) %>%
  count(Department_clean, Division_short) %>%
  group_by(Department_clean) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Department_clean, home_division = Division_short) %>%
  mutate(home_division = case_when(
    Department_clean == "Computer Science"             ~ "Physical Sciences",
    Department_clean == "Molecular Engineering"        ~ "Physical Sciences",
    Department_clean == "Business"                     ~ "Social Sciences",
    Department_clean == "Public Policy Studies"        ~ "Social Sciences",
    Department_clean == "Social Service Administration"~ "Social Sciences",
    Department_clean == "Statistics"                   ~ "Physical Sciences",
    TRUE ~ home_division
  ))

g2_data <- final %>%
  filter(!is.na(Department_clean), !is.na(Decade)) %>%
  left_join(dept_home_division, by = "Department_clean") %>%
  filter(!is.na(home_division), home_division != "Professional Schools")

all_decades <- c("1893\u20131902","1903\u20131912","1913\u20131922","1923\u20131932",
                 "1933\u20131942","1943\u20131952","1953\u20131962","1963\u20131972",
                 "1973\u20131982","1983\u20131992","1993\u20132002","2003\u20132012","2013\u20132022")

g3_data <- final %>%
  filter(!is.na(Division_short), !is.na(Decade)) %>%
  count(Decade, Division_short)

official_dept_map <- c(
  "Anthropology"                       = "Anthropology",
  "Astronomy and Astrophysics"         = "Astronomy and Astrophysics",
  "Biochemistry and Molecular Biology" = "Biochemistry and Molecular Biology",
  "Chemistry"                          = "Chemistry",
  "Classics"                           = "Classics",
  "Divinity"                           = "Divinity School",
  "Ecology and Evolution"              = "Ecology and Evolution",
  "Economics"                          = "Economics",
  "Education"                          = "Education",
  "English Language and Literature"    = "English Language and Literature",
  "Geophysical Sciences"               = "Geophysical Sciences",
  "History"                            = "History",
  "Linguistics"                        = "Linguistics",
  "Mathematics"                        = "Mathematics",
  "Microbiology"                       = "Microbiology",
  "Music"                              = "Music",
  "Organismal Biology and Anatomy"     = "Organismal Biology and Anatomy",
  "Philosophy"                         = "Philosophy",
  "Physics"                            = "Physics",
  "Political Science"                  = "Political Science",
  "Psychology"                         = "Psychology",
  "Social Service Administration"      = "Social Service Administration",
  "Sociology"                          = "Sociology"
)

kw_raw <- final %>%
  filter(
    !is.na(Subject.Terms), Subject.Terms != "",
    !is.na(Department_clean), !is.na(Decade),
    Department_clean %in% names(official_dept_map)
  ) %>%
  mutate(dept_label = official_dept_map[Department_clean]) %>%
  mutate(Subject.Terms = str_remove_all(Subject.Terms, "\\[|\\]|'")) %>%
  mutate(term = strsplit(Subject.Terms, ",\\s*")) %>%
  unnest(term) %>%
  mutate(term = str_trim(str_to_lower(term))) %>%
  filter(nchar(term) > 2)

dept_decade_coverage <- kw_raw %>%
  count(dept_label, Decade) %>%
  count(dept_label, name = "n_decades")

kw_top_depts <- dept_decade_coverage %>%
  filter(n_decades >= 8) %>%
  arrange(desc(n_decades)) %>%
  pull(dept_label)

dept_decade_kws <- map_dfr(kw_top_depts, function(dept) {
  dept_words <- unlist(str_split(str_to_lower(dept), "\\s+"))
  dept_words <- dept_words[nchar(dept_words) > 3]
  exclude    <- c(str_to_lower(dept), dept_words)
  kw_raw %>%
    filter(dept_label == dept, !term %in% exclude) %>%
    count(Decade, term) %>%
    group_by(Decade) %>%
    slice_max(n, n = 10, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Department_clean = dept)
})

locs     <- read.csv("uchicago_locations.csv", stringsAsFactors = FALSE)
map_data <- locs %>%
  left_join(
    final %>% filter(!is.na(Department_clean)) %>% count(Department_clean, name = "total"),
    by = c("Department" = "Department_clean")
  ) %>%
  mutate(total = replace_na(total, 0))

advisor_parsed <- final %>%
  filter(!is.na(Advisors), Advisors != "") %>%
  mutate(advisor = str_remove(Advisors, "^\\['")) %>%
  mutate(advisor = str_remove(advisor, "'\\]$")) %>%
  mutate(advisor = strsplit(advisor, "',\\s*'")) %>%
  unnest(advisor) %>%
  mutate(advisor = str_trim(advisor)) %>%
  filter(advisor != "", nchar(advisor) > 2) %>%
  mutate(
    last  = str_extract(advisor, "^[^,]+"),
    first = str_extract(advisor, "(?<=,\\s)\\S{1,2}")
  ) %>%
  mutate(advisor_norm = ifelse(!is.na(first), paste0(last, ", ", first), advisor)) %>%
  group_by(advisor_norm) %>%
  mutate(advisor_display = advisor[which.max(nchar(advisor))]) %>%
  ungroup() %>%
  mutate(advisor = ifelse(
    str_detect(advisor_display, "^[^,]+,\\s*.+"),
    str_replace(advisor_display, "^([^,]+),\\s*(.+)$", "\\2 \\1"),
    advisor_display
  )) %>%
  select(-last, -first, -advisor_norm, -advisor_display) %>%
  left_join(dept_home_division, by = "Department_clean")

top25_advisors <- advisor_parsed %>%
  count(advisor, name = "n") %>%
  slice_max(n, n = 25, with_ties = FALSE) %>%
  arrange(n) %>%
  mutate(advisor = factor(advisor, levels = advisor))

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "kw_dept_select",
                      choices  = sort(kw_top_depts),
                      selected = if ("History" %in% kw_top_depts) "History" else kw_top_depts[1])
  })
  
  # ── Figure 1 ─────────────────────────────────────────────────
  output$fig1 <- renderPlotly({
    plot_ly(g1_data, x = ~Decade, y = ~n,
            color = ~Division_short, colors = DIVISION_COLORS, type = "bar",
            hovertemplate = "<b>%{fullData.name}</b><br>%{x}<br>%{y} dissertations<extra></extra>") %>%
      plotly::layout(
        barmode = "stack",
        xaxis   = list(title = "Decade", tickangle = -35, gridcolor = LIGHT_GRAY, tickfont = list(size = 11)),
        yaxis   = list(title = "Dissertations", gridcolor = LIGHT_GRAY),
        legend  = list(title = list(text = "<b>Division</b>"), bgcolor = "white", bordercolor = LIGHT_GRAY),
        plot_bgcolor  = BASE_LAYOUT$plot_bgcolor,
        paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
        font          = BASE_LAYOUT$font,
        margin        = BASE_LAYOUT$margin
      ) %>% config(displayModeBar = FALSE)
  })
  
  # ── Figure 2 ─────────────────────────────────────────────────
  output$fig2 <- renderPlotly({
    req(input$dept_division, input$decade_slider)
    div_sel <- as.character(input$dept_division)
    decade_sel <- all_decades[match(as.numeric(input$decade_slider),
                                    c(1902,1912,1922,1932,1942,1952,1962,1972,1982,1992,2002,2012,2022))]
    req(!is.na(decade_sel))
    
    if (div_sel == "All Divisions") {
      dept_data <- g2_data[g2_data$Decade == decade_sel, ]
      dept_data <- dept_data %>%
        count(Department_clean) %>%
        slice_max(n, n = 10, with_ties = FALSE) %>%
        arrange(n) %>%
        mutate(Department_clean = factor(Department_clean, levels = Department_clean))
      title_txt <- paste0("<b>Top 10 Departments (All Divisions) \u00b7 ", decade_sel, "</b>")
    } else {
      dept_data <- g2_data[g2_data$home_division == div_sel & g2_data$Decade == decade_sel, ]
      dept_data <- dept_data %>%
        count(Department_clean) %>%
        slice_max(n, n = 10, with_ties = FALSE) %>%
        arrange(n) %>%
        mutate(Department_clean = factor(Department_clean, levels = Department_clean))
      title_txt <- paste0("<b>Top 10 Departments \u00b7 ", div_sel, " \u00b7 ", decade_sel, "</b>")
    }
    
    validate(need(nrow(dept_data) > 0, "No data for this selection."))
    bar_pal <- colorRampPalette(c("#f0d0d0", MAROON))(nrow(dept_data))
    
    plot_ly(dept_data, x = ~n, y = ~Department_clean,
            type = "bar", orientation = "h",
            marker = list(color = bar_pal),
            hovertemplate = "<b>%{y}</b><br>%{x} dissertations<extra></extra>",
            showlegend = FALSE) %>%
      plotly::layout(
        title  = list(text = title_txt, font = list(color = MAROON, size = 14), x = 0),
        xaxis  = list(title = "Dissertations", gridcolor = LIGHT_GRAY, zeroline = FALSE),
        yaxis  = list(title = "", automargin = TRUE),
        plot_bgcolor  = BASE_LAYOUT$plot_bgcolor,
        paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
        font          = BASE_LAYOUT$font,
        margin = list(l = 240, r = 40, t = 50, b = 50)
      ) %>% config(displayModeBar = FALSE)
  })
  
  # ── Figure 3 ─────────────────────────────────────────────────
  output$fig3 <- renderPlotly({
    plot_ly(g3_data, x = ~Decade, y = ~n,
            color = ~Division_short, colors = DIVISION_COLORS,
            type = "scatter", mode = "lines+markers",
            hovertemplate = "<b>%{fullData.name}</b><br>%{x}: %{y} dissertations<extra></extra>") %>%
      plotly::layout(
        xaxis  = list(title = "Decade", tickangle = -35, gridcolor = LIGHT_GRAY),
        yaxis  = list(title = "Dissertations per Decade", gridcolor = LIGHT_GRAY),
        legend = list(title = list(text = "<b>Division</b>"), bgcolor = "white", bordercolor = LIGHT_GRAY),
        plot_bgcolor  = BASE_LAYOUT$plot_bgcolor,
        paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
        font          = BASE_LAYOUT$font,
        margin        = BASE_LAYOUT$margin
      ) %>% config(displayModeBar = FALSE)
  })
  
  # ── Figure 4 ─────────────────────────────────────────────────
  output$fig4 <- renderPlotly({
    dept_sel  <- input$kw_dept_select
    plot_data <- dept_decade_kws %>% filter(Department_clean == dept_sel)
    validate(need(nrow(plot_data) > 0, "No keyword data for this department."))
    
    all_kws <- plot_data %>%
      group_by(term) %>% summarise(total = sum(n)) %>%
      slice_max(total, n = 10, with_ties = FALSE) %>%
      arrange(desc(total)) %>% pull(term)
    
    plot_data  <- plot_data %>% filter(term %in% all_kws)
    kw_colors  <- colorRampPalette(c(MAROON, GOLD, "#4a90a4", "#2d6a4f",
                                     "#e76f51", "#9b2226", "#606c38", "#7b2d8b",
                                     "#6a0572", "#264653"))(length(all_kws))
    names(kw_colors) <- all_kws
    
    fig4 <- plot_ly()
    for (i in seq_along(all_kws)) {
      kw_sel <- all_kws[i]
      kw_d   <- plot_data %>% filter(term == kw_sel) %>% arrange(Decade)
      if (nrow(kw_d) < 1) next
      fig4 <- fig4 %>% add_trace(
        x = kw_d$Decade, y = kw_d$n,
        type = "scatter", mode = "lines+markers", name = kw_sel,
        line   = list(color = kw_colors[kw_sel], width = 2.5, shape = "spline"),
        marker = list(color = kw_colors[kw_sel], size = 7),
        hovertemplate = paste0("<b>", kw_sel, "</b><br>%{x}<br>%{y} occurrences<extra></extra>")
      )
    }
    fig4 %>%
      plotly::layout(
        title  = list(text = paste0("<b>Keyword Trends \u00b7 ", dept_sel, " \u00b7 1893\u20132022</b>"),
                      font = list(color = MAROON, size = 14), x = 0),
        xaxis  = list(title = "Decade", tickangle = -35, gridcolor = LIGHT_GRAY),
        yaxis  = list(title = "Frequency / Occurrence", gridcolor = LIGHT_GRAY),
        legend = list(title = list(text = "<b>Keyword</b>"), bgcolor = "white", bordercolor = LIGHT_GRAY),
        plot_bgcolor  = BASE_LAYOUT$plot_bgcolor,
        paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
        font          = BASE_LAYOUT$font,
        margin = list(l = 60, r = 180, t = 50, b = 80)
      ) %>% config(displayModeBar = FALSE)
  })
  
  # ── Campus Map ────────────────────────────────────────────────
  output$fig_map <- renderLeaflet({
    pal       <- colorNumeric(c("#f5e6e6","#c97a7a","#800000","#350E20"), domain = map_data$total)
    radius_fn <- function(n) sqrt(n / max(map_data$total, 1)) * 20 + 4
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -87.5986, lat = 41.7886, zoom = 15) %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~radius_fn(total), color = "#350E20", weight = 1,
        fillColor = ~pal(total), fillOpacity = 0.85,
        popup = ~paste0(
          "<div style='font-family:Georgia,serif;padding:8px;min-width:180px'>",
          "<b style='color:#800000;font-size:14px'>", Building, "</b><br/>",
          "<span style='color:#767676;font-size:12px'>", Department, "</span><br/><br/>",
          "<b style='font-size:13px'>", comma(total), "</b>",
          "<span style='color:#767676'> dissertations</span></div>"
        )
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = ~total,
                title = "Dissertations", labFormat = labelFormat(big.mark = ","))
  })
  
  # ── Figure 5 ─────────────────────────────────────────────────
  output$fig5 <- renderPlotly({
    plot_ly(top25_advisors, x = ~n, y = ~advisor,
            type = "bar", orientation = "h",
            marker = list(color = colorRampPalette(c("#f0d0d0", MAROON))(25),
                          line  = list(color = "white", width = 0.5)),
            hovertemplate = "<b>%{y}</b><br>%{x} dissertations supervised<extra></extra>") %>%
      plotly::layout(
        title  = list(text = "<b>Top 25 Most Prolific Advisors \u00b7 University of Chicago</b>",
                      font = list(color = MAROON, size = 14), x = 0),
        xaxis  = list(title = "Dissertations Supervised", gridcolor = LIGHT_GRAY, zeroline = FALSE),
        yaxis  = list(title = "", tickfont = list(size = 11), automargin = TRUE),
        plot_bgcolor  = BASE_LAYOUT$plot_bgcolor,
        paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
        font          = BASE_LAYOUT$font,
        margin = list(l = 180, r = 40, t = 50, b = 60)
      ) %>% config(displayModeBar = FALSE)
  })
  
  # ── Figure 6: Sankey ─────────────────────────────────────────
  output$fig6 <- renderPlotly({
    req(input$advisor_division)
    div_filter <- as.character(input$advisor_division)
    
    DIV_COLORS <- c(
      "Social Sciences"     = "#C16622",
      "Physical Sciences"   = "#350E20",
      "Biological Sciences" = "#8B4513",
      "Humanities"          = "#800000",
      "Divinity School"     = "#A0785A"
    )
    
    if (div_filter == "All Divisions") {
      
      sankey_data <- advisor_parsed %>%
        filter(!is.na(home_division), home_division %in% names(DIV_COLORS)) %>%
        count(advisor, home_division, name = "n") %>%
        group_by(home_division) %>%
        slice_max(n, n = 10, with_ties = FALSE) %>%
        ungroup()
      
      div_totals <- advisor_parsed %>%
        filter(!is.na(home_division), home_division %in% names(DIV_COLORS)) %>%
        count(home_division, name = "div_total")
      
      div_shares <- sankey_data %>%
        group_by(home_division) %>%
        summarise(top10_n = sum(n), .groups = "drop") %>%
        left_join(div_totals, by = "home_division") %>%
        mutate(pct   = round(100 * top10_n / div_total, 1),
               label = paste0(home_division, "\n", pct, "% by top 10"))
      
      div_labels    <- setNames(div_shares$label, div_shares$home_division)
      advisor_nodes <- unique(sankey_data$advisor)
      all_nodes     <- c(unname(div_labels), advisor_nodes)
      node_keys     <- c(names(div_labels), advisor_nodes)
      node_idx      <- setNames(seq_along(node_keys) - 1L, node_keys)
      node_colors   <- c(
        unname(DIV_COLORS[names(div_labels)]),
        sapply(advisor_nodes, function(a) {
          div <- sankey_data$home_division[sankey_data$advisor == a][1]
          unname(DIV_COLORS[div])
        })
      )
      link_colors <- sapply(sankey_data$home_division, function(d) {
        col <- col2rgb(DIV_COLORS[d])
        paste0("rgba(", col[1], ",", col[2], ",", col[3], ",0.25)")
      })
      
      plot_ly(type = "sankey", orientation = "h",
              node = list(label = all_nodes, color = node_colors, pad = 14, thickness = 22,
                          hovertemplate = "<b>%{label}</b><extra></extra>"),
              link = list(source = node_idx[sankey_data$home_division],
                          target = node_idx[sankey_data$advisor],
                          value  = sankey_data$n, color = link_colors,
                          hovertemplate = "<b>%{source.label}</b> \u2192 <b>%{target.label}</b><br>%{value} dissertations<extra></extra>")
      ) %>%
        plotly::layout(
          title = list(text = paste0("<b>Advising Flow \u00b7 All Divisions</b><br>",
                                     "<sup style='color:#767676'>Top 10 advisors per division \u00b7 % shows share of division total</sup>"),
                       font = list(color = MAROON, size = 14), x = 0),
          font = BASE_LAYOUT$font, paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
          margin = list(l = 40, r = 40, t = 80, b = 40)
        ) %>% config(displayModeBar = FALSE)
      
    } else {
      
      div_color   <- unname(DIV_COLORS[div_filter])
      sankey_data <- advisor_parsed %>%
        filter(!is.na(home_division), home_division == div_filter) %>%
        count(advisor, name = "n") %>%
        slice_max(n, n = 10, with_ties = FALSE) %>%
        arrange(desc(n))
      
      validate(need(nrow(sankey_data) > 0, "No advisor data for this division."))
      
      div_total <- nrow(advisor_parsed[!is.na(advisor_parsed$home_division) &
                                         advisor_parsed$home_division == div_filter, ])
      top10_pct <- round(100 * sum(sankey_data$n) / div_total, 1)
      
      all_nodes   <- c(paste0(div_filter, "\n", top10_pct, "% by top 10"), sankey_data$advisor)
      node_keys   <- c(div_filter, sankey_data$advisor)
      node_idx    <- setNames(seq_along(node_keys) - 1L, node_keys)
      node_colors <- rep(div_color, length(all_nodes))
      col         <- col2rgb(div_color)
      link_col    <- paste0("rgba(", col[1], ",", col[2], ",", col[3], ",0.22)")
      
      plot_ly(type = "sankey", orientation = "h",
              node = list(label = all_nodes, color = node_colors, pad = 15, thickness = 25,
                          hovertemplate = "<b>%{label}</b><extra></extra>"),
              link = list(source = rep(node_idx[div_filter], nrow(sankey_data)),
                          target = node_idx[sankey_data$advisor],
                          value  = sankey_data$n, color = link_col,
                          hovertemplate = "<b>%{target.label}</b><br>%{value} dissertations<extra></extra>")
      ) %>%
        plotly::layout(
          title = list(text = paste0("<b>Advising Flow \u00b7 ", div_filter, "</b><br>",
                                     "<sup style='color:#767676'>Top 10 advisors account for <b>",
                                     top10_pct, "%</b> of all supervised dissertations</sup>"),
                       font = list(color = MAROON, size = 14), x = 0),
          font = BASE_LAYOUT$font, paper_bgcolor = BASE_LAYOUT$paper_bgcolor,
          margin = list(l = 40, r = 40, t = 80, b = 40)
        ) %>% config(displayModeBar = FALSE)
    }
  })
  
}