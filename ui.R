# ============================================================
# ui.R — A Century of Knowledge
# MACS 40700 — Khushi Desai
# ============================================================

library(shiny)
library(plotly)
library(leaflet)

ui <- navbarPage(
  title = div(
    span("A Century of Knowledge",
         style = "font-family: 'Libre Franklin', Helvetica, Arial, sans-serif;
                  font-size: 1.1rem; font-weight: 400; letter-spacing: 0.02em;")
  ),
  id = "nav",
  collapsible = TRUE,
  header = tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=EB+Garamond:ital,wght@0,400;0,600;1,400&family=Libre+Franklin:wght@300;400;600;700&display=swap"
    ),
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    tags$style(HTML("
      *, *::before, *::after { box-sizing: border-box; }
      :root {
        --maroon:      #800000;
        --maroon-dark: #5a0000;
        --gold:        #C16622;
        --stone:       #350E20;
        --gray:        #767676;
        --light-gray:  #D6D6CE;
        --off-white:   #F9F6F0;
        --white:       #ffffff;
        --text:        #1a1a1a;
        --text-muted:  #555555;
      }
      html, body {
        font-family: 'Libre Franklin', Helvetica, Arial, sans-serif;
        color: var(--text);
        background: var(--white);
        font-size: 16px;
        line-height: 1.6;
      }
      .navbar {
        background-color: var(--white) !important;
        border-bottom: 2px solid var(--maroon) !important;
        min-height: 54px !important;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      }
      .navbar-brand { color: var(--maroon) !important; padding-top: 14px !important; }
      .navbar-nav > li > a {
        color: var(--gray) !important;
        font-size: 0.75rem !important;
        font-weight: 600 !important;
        letter-spacing: 0.07em !important;
        text-transform: uppercase !important;
        padding: 18px 16px !important;
        border-bottom: 3px solid transparent;
        transition: color 0.2s, border-color 0.2s;
      }
      .navbar-nav > li > a:hover { color: var(--maroon) !important; background: transparent !important; }
      .navbar-nav > li.active > a,
      .navbar-nav > li.active > a:hover {
        color: var(--maroon) !important;
        background: transparent !important;
        border-bottom: 3px solid var(--maroon) !important;
      }
      .hero {
        background: var(--maroon);
        color: var(--white);
        padding: 4rem 3rem 3.5rem;
        position: relative;
        overflow: hidden;
      }
      .hero::before {
        content: '';
        position: absolute;
        inset: 0;
        background:
          repeating-linear-gradient(0deg, transparent, transparent 39px, rgba(255,255,255,0.035) 39px, rgba(255,255,255,0.035) 40px),
          repeating-linear-gradient(90deg, transparent, transparent 39px, rgba(255,255,255,0.035) 39px, rgba(255,255,255,0.035) 40px);
        pointer-events: none;
      }
      .hero-inner { max-width: 1100px; margin: 0 auto; position: relative; }
      .hero-eyebrow {
        font-size: 0.7rem; font-weight: 700; letter-spacing: 0.14em;
        text-transform: uppercase; color: rgba(255,255,255,0.55); margin-bottom: 0.9rem;
      }
      .hero h1 {
        font-family: 'Libre Franklin', Georgia, serif;
        font-size: clamp(2.4rem, 5vw, 4rem);
        font-weight: 400; line-height: 1.12; margin: 0 0 0.4rem;
        animation: fadeUp 0.6s ease both;
      }
      .hero h1 em { font-style: italic; color: rgba(255,255,255,0.8); }
      .hero-sub {
        font-size: 1rem; font-weight: 300; color: rgba(255,255,255,0.7);
        max-width: 620px; margin-bottom: 3rem; animation: fadeUp 0.6s 0.15s ease both;
      }
      .stats-row {
        display: flex; flex-wrap: wrap; gap: 0;
        border-top: 1px solid rgba(255,255,255,0.18);
        padding-top: 1.8rem; animation: fadeUp 0.6s 0.3s ease both;
      }
      .stat-item {
        flex: 1; min-width: 130px; padding-right: 2rem; margin-right: 2rem;
        border-right: 1px solid rgba(255,255,255,0.13);
      }
      .stat-item:last-child { border-right: none; margin-right: 0; padding-right: 0; }
      .stat-number {
        display: block; font-family: 'Libre Franklin', Georgia, serif;
        font-size: 2.8rem; font-weight: 400; line-height: 1; color: var(--white);
      }
      .stat-label {
        display: block; font-size: 0.68rem; font-weight: 700;
        letter-spacing: 0.1em; text-transform: uppercase;
        color: rgba(255,255,255,0.5); margin-top: 0.3rem;
      }
      .rule-bar {
        height: 4px;
        background: linear-gradient(90deg, var(--maroon), var(--gold), var(--maroon));
      }
      .intro-section {
        max-width: 820px; margin: 0 auto; padding: 3rem 2.5rem 2rem;
        border-bottom: 1px solid var(--light-gray);
      }
      .intro-section p {
        font-family: 'Libre Franklin', Georgia, serif;
        font-size: 1.18rem; line-height: 1.8; color: var(--text); margin-bottom: 1rem;
      }
      
      .panel-wrap { max-width: 1200px; margin: 0 auto; padding: 2rem 2.5rem 4rem; }
      
      .panel-header { padding-bottom: 1rem; border-bottom: 1px solid var(--light-gray); margin-bottom: 1.5rem; }
      .panel-number { font-size: 0.68rem; font-weight: 700; letter-spacing: 0.12em; text-transform: uppercase; color: var(--gold); margin-bottom: 0.3rem; }
      .panel-header h2 { font-family: 'Libre Franklin', Georgia, serif; font-size: 1.75rem; font-weight: 400; color: var(--maroon); margin: 0 0 0.5rem; }
      .panel-header p { font-size: 0.92rem; color: var(--text-muted); max-width: 700px; line-height: 1.6; margin: 0; }
      .plot-wrap { border: 1px solid var(--light-gray); border-radius: 2px; padding: 0.75rem; background: var(--white); }
      .site-footer {
        background: var(--maroon); color: rgba(255,255,255,0.6);
        text-align: center; padding: 2rem; font-size: 0.78rem;
        letter-spacing: 0.03em; line-height: 1.8; margin-top: 2rem;
      }
            .fun-facts {
        max-width: 1200px;
        margin: 0 auto 3rem;
        padding: 0 2.5rem;
      }
      .fun-facts-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
        gap: 1.25rem;
      }
      .fun-fact-card {
        border: 1px solid var(--light-gray);
        background: #fcfbf8;
        padding: 1.25rem 1.25rem 1rem;
        border-left: 5px solid var(--gold);
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
      }
      .fun-fact-title {
        font-size: 0.75rem;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: var(--gold);
        margin-bottom: 0.4rem;
      }
      .fun-fact-card p {
        font-size: 0.95rem;
        line-height: 1.7;
        margin: 0;
        color: var(--text);
      }
      .tab-content { padding: 0 !important; }
      .tab-pane    { padding: 0 !important; }
      @keyframes fadeUp {
        from { opacity: 0; transform: translateY(14px); }
        to   { opacity: 1; transform: translateY(0); }
      }
    "))
  ),
  
  # ── TAB 1: Overview ──────────────────────────────────────────
  tabPanel(
    title = "Overview", value = "tab_overview",
    div(class = "hero",
        div(class = "hero-inner",
            div(style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1.5rem;",
                tags$img(src = "UChicago_Phoenix_Classic_1Color_Maroon_RGB.png",
                         style = "height: 80px; filter: brightness(0) invert(1); opacity: 0.85;"),
                tags$img(src = "University Logotype_Maroon_RGB.png",
                         style = "height: 70px; filter: brightness(0) invert(1); opacity: 0.75;")
            ),
            p(class = "hero-eyebrow", "Doctoral Dissertations · 1893–2025"),
            h1("A Century of", tags$em("Knowledge")),
            p(class = "hero-sub",
              "Tracing 130 years of doctoral production through dissertations,
               departments, and the advisors who shaped them."),
            div(class = "stats-row",
                div(class = "stat-item",
                    span(class = "stat-number", id = "stat_total", "0"),
                    span(class = "stat-label", "Dissertations")),
                div(class = "stat-item",
                    span(class = "stat-number", id = "stat_years", "0"),
                    span(class = "stat-label", "Years of History")),
                div(class = "stat-item",
                    span(class = "stat-number", id = "stat_depts", "0"),
                    span(class = "stat-label", "Departments")),
                div(class = "stat-item",
                    span(class = "stat-number", id = "stat_advisors", "0"),
                    span(class = "stat-label", "Advisors")),
                div(class = "stat-item",
                    span(class = "stat-number", "6"),
                    span(class = "stat-label", "Divisions"))
            )
        )
    ),
    tags$script(HTML("
      function animateCount(id, target, duration) {
        var el = document.getElementById(id);
        if (!el) return;
        var start = performance.now();
        function step(now) {
          var t = Math.min((now - start) / duration, 1);
          var ease = 1 - Math.pow(1 - t, 3);
          el.textContent = Math.round(ease * target).toLocaleString();
          if (t < 1) requestAnimationFrame(step);
        }
        requestAnimationFrame(step);
      }
      setTimeout(function() {
        animateCount('stat_total',    32878, 1800);
        animateCount('stat_years',    132,   1400);
        animateCount('stat_depts',    68,    1200);
        animateCount('stat_advisors', 4257,  1800);
      }, 300);
    ")),
    div(class = "rule-bar"),
    div(class = "intro-section",
        p("The University of Chicago has conferred doctoral degrees since 1893 — one of the oldest
          continuous doctoral programs in the United States. Over 130 years, its graduate enterprise
          expanded from a handful of Humanities and Social Science departments to a sprawling
          ecosystem spanning Divinity, Law, Business, and the natural sciences."),
        p("This project draws on a unified dataset assembled from the HathiTrust Digital Library
          and ProQuest Dissertations & Theses. Four interactive visualizations trace the
          institutional evolution of knowledge production across 130 years.")
    ),
    
    div(class = "fun-facts",
        div(class = "panel-header",
            p(class = "panel-number", "Highlights"),
            h2("Fun Facts from the Archive"),
            p("A few notable stories drawn from the university’s dissertation and advising history.")
        ),
        div(class = "fun-facts-grid",
            div(class = "fun-fact-card",
                div(class = "fun-fact-title", "Most Prolific Advisor"),
                p(
                  tags$b("Professor Jean Comaroff"),
                  " holds the record as UChicago’s most prolific dissertation advisor, having supervised ",
                  tags$b("71 PhDs"),
                  ". She joined the university as an Assistant Professor in 1978 and spent 34 years at UChicago, chairing the Anthropology Department from 1996 to 1999 and winning the Quantrell Award for undergraduate teaching before moving to Harvard in 2012."
                )
            ),
            div(class = "fun-fact-card",
                div(class = "fun-fact-title", "Famous Dissertation"),
                p(
                  tags$b("Carl Sagan"),
                  " earned three degrees from the University of Chicago — a B.S. and M.S. in Physics, and a PhD in Astronomy & Astrophysics in 1960. His dissertation, ",
                  tags$em("\"Physical Studies of the Planets\""),
                  ", was supervised by Gerard Kuiper at Yerkes Observatory. UChicago later named its ",
                  tags$b("Sagan Teaching Awards"),
                  " in his honor. He went on to write ",
                  tags$em("Cosmos"),
                  ", watched by over 500 million people worldwide."
                )
            )
        )
    ),
    
    div(class = "panel-wrap",
        div(class = "panel-header",
            p(class = "panel-number", "Visualization 01"),
            h2("Dissertations by Division Over Time"),
            p("Each bar represents one decade; colors encode the six academic divisions.
              Hover for exact counts. The dramatic postwar expansion of the Physical and
              Biological Sciences reshaped the university's intellectual center of gravity.")
        ),
        div(class = "plot-wrap",
            plotlyOutput("fig1", height = "520px")
        )
    ),
    div(class = "site-footer",
        p("Khushi Desai \u00b7 MACS 40700: Data Visualization \u00b7 University of Chicago \u00b7 March 2026"),
        p("Data: HathiTrust Digital Library & ProQuest Dissertations & Theses.")
    )
  ),
  
  # ── TAB 2: Departments ───────────────────────────────────────
  tabPanel(
    title = "Departments", value = "tab_dept",
    div(class = "panel-wrap",
        div(class = "panel-header",
            p(class = "panel-number", "Visualization 02"),
            h2("Top 10 Departments by Decade"),
            p("Select a division and a decade to see the top 10 departments by dissertation output.")
        ),
        fluidRow(
          column(4,
                 selectInput("dept_division", "Filter by Division",
                             choices  = c("All Divisions", "Biological Sciences", "Divinity School",
                                          "Humanities", "Physical Sciences", "Social Sciences"),
                             selected = "All Divisions")
          ),
          column(8,
                 sliderInput("decade_slider", "Decade",
                             min = 1902, max = 2022, value = 2022, step = 10,
                             ticks = TRUE, sep = "",
                             animate = animationOptions(interval = 800, loop = FALSE))
          )
        ),
        div(class = "plot-wrap", style = "margin-bottom: 3rem;",
            plotlyOutput("fig2", height = "480px")
        ),
        div(class = "panel-header",
            p(class = "panel-number", "Visualization 03"),
            h2("Divisional Growth Trajectories"),
            p("Absolute dissertation counts per division per decade across 130 years.")
        ),
        div(class = "plot-wrap",
            plotlyOutput("fig3", height = "480px"),
            tags$div(
              style = "margin-top:12px; font-size:0.85rem; color:#555; line-height:1.5;",
              tags$b("Note:"), 
              " Institutional structures have evolved over the past 130 years. ",
              "Several professional schools appear later in the series because they were established or reorganized under different names, for example ",
              tags$b("Chicago Booth School of Business"), " (formerly the Graduate School of Business) and ",
              tags$b("Harris School of Public Policy"), 
              ". Similarly, fields such as ",
              tags$b("Computer Science"), 
              " emerged much later as independent academic departments. These institutional changes help explain the later growth of the ",
              tags$em("Professional Schools"), 
              " and parts of the ",
              tags$em("Physical Sciences"), 
              " divisions in the figure."
            )
        )
    )
  ),
  
  # ── TAB 3: Campus Map ────────────────────────────────────────
  tabPanel(
    title = "Campus", value = "tab_map",
    div(class = "panel-wrap",
        div(class = "panel-header",
            p(class = "panel-number", "Visualization 04"),
            h2("Doctoral Output Across Campus"),
            p("Each circle marks a department building on the University of Chicago campus.
              Size and color encode total dissertation output. Click any circle for department details.")
        ),
        div(class = "plot-wrap",
            leafletOutput("fig_map", height = "600px")
        )
    )
  ),
  
  # ── TAB 4: Advisors ──────────────────────────────────────────
  tabPanel(
    title = "Advisors", value = "tab_advisors",
    div(class = "panel-wrap",
        div(class = "panel-header",
            p(class = "panel-number", "Visualization 05"),
            h2("Most Prolific Advisors"),
            p("The top 25 advisors by total dissertations supervised across all departments and years.")
        ),
        div(class = "plot-wrap", style = "margin-bottom: 3rem;",
            plotlyOutput("fig5", height = "520px")
        ),
        div(class = "panel-header",
            p(class = "panel-number", "Visualization 06"),
            h2("Advising Flow by Division"),
            p("A Sankey diagram showing how dissertation advising flows from division to top advisors. Width of each flow encodes the number of dissertations supervised.")
        ),
        fluidRow(
          column(4,
                 selectInput("advisor_division", "Filter by Division",
                             choices  = c("All Divisions", "Biological Sciences", "Divinity School",
                                          "Humanities", "Physical Sciences", "Social Sciences"),
                             selected = "All Divisions")
          )
        ),
        div(class = "plot-wrap",
            plotlyOutput("fig6", height = "600px"),
            tags$div(
              style = "margin-top:12px; font-size:0.85rem; color:#555; line-height:1.5;",
              tags$b("Note:"), " Advisors may supervise dissertations across multiple divisions. ",
              "This Sankey diagram counts dissertations ", tags$em("within the selected division only"),
              ", while the bar chart above shows each advisor’s ",
              tags$em("total dissertations supervised across the entire university"), ". ",
              tags$b("For example:"), " Jean Comaroff supervised ",
              tags$b("71 dissertations overall"),
              ", distributed across Social Sciences (50), Humanities (9), Biological Sciences (6), and the Divinity School (6)."
            )
        )
    )
 )
)