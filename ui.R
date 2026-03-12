# ============================================================
# ui.R
# A Century of Knowledge: Doctoral Production at UChicago
# MACS 40700 — Khushi Desai
# ============================================================

library(shiny)
library(plotly)

ui <- navbarPage(
  title = div(
    span("A Century of Knowledge", style = "font-family: 'EB Garamond', Georgia, serif;
          font-size: 1.1rem; font-weight: 400; letter-spacing: 0.02em;")
  ),
  id = "nav",
  collapsible = TRUE,
  header = tags$head(
    
    # Google Fonts
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=EB+Garamond:ital,wght@0,400;0,600;1,400&family=Libre+Franklin:wght@300;400;600;700&display=swap"
    ),
    # Font Awesome
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    
    tags$style(HTML("

      /* ── Reset & base ─────────────────────────── */
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

      /* ── Navbar ───────────────────────────────── */
      .navbar {
        background-color: var(--white) !important;
        border-bottom: 2px solid var(--maroon) !important;
        min-height: 54px !important;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      }

      .navbar-brand {
        color: var(--maroon) !important;
        padding-top: 14px !important;
      }

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

      .navbar-nav > li > a:hover {
        color: var(--maroon) !important;
        background: transparent !important;
      }

      .navbar-nav > li.active > a,
      .navbar-nav > li.active > a:hover {
        color: var(--maroon) !important;
        background: transparent !important;
        border-bottom: 3px solid var(--maroon) !important;
      }

      /* ── Hero ─────────────────────────────────── */
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
          repeating-linear-gradient(
            0deg, transparent, transparent 39px,
            rgba(255,255,255,0.035) 39px, rgba(255,255,255,0.035) 40px
          ),
          repeating-linear-gradient(
            90deg, transparent, transparent 39px,
            rgba(255,255,255,0.035) 39px, rgba(255,255,255,0.035) 40px
          );
        pointer-events: none;
      }

      .hero-inner { max-width: 1100px; margin: 0 auto; position: relative; }

      .hero-eyebrow {
        font-size: 0.7rem;
        font-weight: 700;
        letter-spacing: 0.14em;
        text-transform: uppercase;
        color: rgba(255,255,255,0.55);
        margin-bottom: 0.9rem;
      }

      .hero h1 {
        font-family: 'EB Garamond', Georgia, serif;
        font-size: clamp(2.4rem, 5vw, 4rem);
        font-weight: 400;
        line-height: 1.12;
        margin: 0 0 0.4rem;
        animation: fadeUp 0.6s ease both;
      }

      .hero h1 em {
        font-style: italic;
        color: rgba(255,255,255,0.8);
      }

      .hero-sub {
        font-size: 1rem;
        font-weight: 300;
        color: rgba(255,255,255,0.7);
        max-width: 620px;
        margin-bottom: 3rem;
        animation: fadeUp 0.6s 0.15s ease both;
      }

      /* ── Stats row ────────────────────────────── */
      .stats-row {
        display: flex;
        flex-wrap: wrap;
        gap: 0;
        border-top: 1px solid rgba(255,255,255,0.18);
        padding-top: 1.8rem;
        animation: fadeUp 0.6s 0.3s ease both;
      }

      .stat-item {
        flex: 1;
        min-width: 130px;
        padding-right: 2rem;
        margin-right: 2rem;
        border-right: 1px solid rgba(255,255,255,0.13);
      }

      .stat-item:last-child { border-right: none; margin-right: 0; padding-right: 0; }

      .stat-number {
        display: block;
        font-family: 'EB Garamond', Georgia, serif;
        font-size: 2.8rem;
        font-weight: 400;
        line-height: 1;
        color: var(--white);
      }

      .stat-label {
        display: block;
        font-size: 0.68rem;
        font-weight: 700;
        letter-spacing: 0.1em;
        text-transform: uppercase;
        color: rgba(255,255,255,0.5);
        margin-top: 0.3rem;
      }

      /* ── Gold rule ────────────────────────────── */
      .rule-bar {
        height: 4px;
        background: linear-gradient(90deg, var(--maroon), var(--gold), var(--maroon));
      }

      /* ── Intro section ────────────────────────── */
      .intro-section {
        max-width: 820px;
        margin: 0 auto;
        padding: 3rem 2.5rem 2rem;
        border-bottom: 1px solid var(--light-gray);
      }

      .intro-section p {
        font-family: 'EB Garamond', Georgia, serif;
        font-size: 1.18rem;
        line-height: 1.8;
        color: var(--text);
        margin-bottom: 1rem;
      }

      /* ── Panel layout ─────────────────────────── */
      .panel-wrap {
        max-width: 1200px;
        margin: 0 auto;
        padding: 2rem 2.5rem 4rem;
      }

      .panel-header {
        padding-bottom: 1rem;
        border-bottom: 1px solid var(--light-gray);
        margin-bottom: 1.5rem;
      }

      .panel-number {
        font-size: 0.68rem;
        font-weight: 700;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        color: var(--gold);
        margin-bottom: 0.3rem;
      }

      .panel-header h2 {
        font-family: 'EB Garamond', Georgia, serif;
        font-size: 1.75rem;
        font-weight: 400;
        color: var(--maroon);
        margin: 0 0 0.5rem;
      }

      .panel-header p {
        font-size: 0.92rem;
        color: var(--text-muted);
        max-width: 700px;
        line-height: 1.6;
        margin: 0;
      }

      .plot-wrap {
        border: 1px solid var(--light-gray);
        border-radius: 2px;
        padding: 0.75rem;
        background: var(--white);
      }

      /* ── Footer ───────────────────────────────── */
      .site-footer {
        background: var(--maroon);
        color: rgba(255,255,255,0.6);
        text-align: center;
        padding: 2rem;
        font-size: 0.78rem;
        letter-spacing: 0.03em;
        line-height: 1.8;
        margin-top: 2rem;
      }

      /* Remove Shiny default padding on tab content */
      .tab-content { padding: 0 !important; }
      .tab-pane    { padding: 0 !important; }

      /* ── Animation ────────────────────────────── */
      @keyframes fadeUp {
        from { opacity: 0; transform: translateY(14px); }
        to   { opacity: 1; transform: translateY(0);    }
      }

    "))
  ),
  
  # ── TAB 1: Overview ────────────────────────────────────────
  tabPanel(
    title = "Overview",
    value = "tab_overview",
    
    # Hero
    div(class = "hero",
        div(class = "hero-inner",
            p(class = "hero-eyebrow",
              "University of Chicago · MACS 40700 Data Visualization · March 2026"),
            h1("A Century of", br(), em("Knowledge")),
            p(class = "hero-sub",
              "Doctoral production at the University of Chicago, 1893–2025.
           Tracing 130 years of intellectual history through dissertations,
           departments, and advisors."),
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
    
    # Animated counters (JS)
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
        animateCount('stat_total',   32878, 1800);
        animateCount('stat_years',   132,   1400);
        animateCount('stat_depts',   68,    1200);
        animateCount('stat_advisors',4257,  1800);
      }, 300);
    ")),
    
    div(class = "rule-bar"),
    
    # Intro
    div(class = "intro-section",
        p("The University of Chicago has conferred doctoral degrees since 1893—one of the oldest
         continuous doctoral programs in the United States. Over 130 years, its graduate enterprise
         expanded from a handful of Humanities and Social Science departments to a sprawling
         ecosystem spanning Divinity, Law, Business, and the natural sciences."),
        p("This project draws on a unified dataset assembled from the HathiTrust Digital Library
         and ProQuest Dissertations & Theses, in collaboration with the Center for Digital
         Scholarship at the Regenstein Library. Four interactive visualizations trace the
         institutional evolution of knowledge production across 130 years.")
    ),
    
    # Graph 1
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
        p("Khushi Desai · MACS 40700: Data Visualization · University of Chicago · March 2026"),
        p("Data: HathiTrust Digital Library & ProQuest Dissertations & Theses.
         Assembled in collaboration with the Center for Digital Scholarship, Regenstein Library.")
    )
  ),
  
  # ── TAB 2: Departments (top 10 + trajectories) ────────────
  tabPanel(
    title = "Departments",
    value = "tab_dept",
    div(class = "panel-wrap",
        
        # ── Section A: Top 10 by decade ──
        div(class = "panel-header",
            p(class = "panel-number", "Visualization 02"),
            h2("Top 10 Departments by Decade"),
            p("Select a division and a decade to see the top 10 departments by dissertation
           output. The maroon color scale encodes relative output — darker = more
           dissertations.")
        ),
        fluidRow(
          column(4,
                 selectInput(
                   inputId  = "division_select",
                   label    = "Division",
                   choices  = c(
                     "Biological Sciences", "Divinity School", "Humanities",
                     "Physical Sciences", "Social Sciences"
                   ),
                   selected = "Social Sciences"
                 )
          ),
          column(8,
                 sliderInput(
                   inputId = "decade_slider",
                   label   = "Decade (upper bound)",
                   min     = 1902,
                   max     = 2022,
                   value   = 2022,
                   step    = 10,
                   ticks   = TRUE,
                   animate = animationOptions(interval = 800, loop = FALSE),
                   sep     = ""
                 )
          )
        ),
        div(class = "plot-wrap", style = "margin-bottom: 3rem;",
            plotlyOutput("fig2", height = "480px")
        ),
        
        # ── Section B: Trajectories ──
        div(class = "panel-header",
            p(class = "panel-number", "Visualization 03"),
            h2("Divisional Growth Trajectories"),
            p("Absolute dissertation counts per division per decade across 130 years.
           Click legend items to isolate individual divisions.")
        ),
        div(class = "plot-wrap",
            plotlyOutput("fig3", height = "480px")
        )
    )
  ),
  
  # ── TAB 3: Keywords ───────────────────────────────────────
  tabPanel(
    title = "Keywords",
    value = "tab_kw",
    div(class = "panel-wrap",
        div(class = "panel-header",
            p(class = "panel-number", "Visualization 03"),
            h2("Shifting Intellectual Vocabularies"),
            p("Select a department to trace how its top keywords shifted in frequency from
           1990 to 2025. Lines show a 3-year smoothed occurrence count drawn from
           ProQuest dissertation keywords.")
        ),
        fluidRow(
          column(4,
                 selectInput(
                   inputId  = "kw_dept_select",
                   label    = "Department",
                   choices  = NULL,
                   selected = NULL
                 )
          )
        ),
        div(class = "plot-wrap",
            plotlyOutput("fig4", height = "540px")
        )
    )
  )
)