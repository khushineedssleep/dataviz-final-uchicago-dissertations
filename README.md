# A Century of Knowledge: Doctoral Production at the University of Chicago, 1893–2025

**MACS 40700: Data Visualization** | Khushi Desai 

## Overview

This project visualizes 130 years of doctoral dissertation production at the University of Chicago across 6 academic divisions, 194 departments, and over 4,200 advisors. Using a dataset of 32,878 dissertations drawn from ProQuest and HathiTrust, the Shiny app explores how the university's intellectual output has shifted over time, which fields grew, which advisors shaped generations of scholars, and where knowledge was physically produced on campus.

**Research Question:** How has doctoral production at the University of Chicago shifted across academic divisions, departments, and advisors from 1893 to 2025?


## Live App

(https://khushisapp.shinyapps.io/dataviz-final-uchicago-dissertations/)

## Key Findings

- **Social Sciences dominates.** The Division of Social Sciences has produced the most doctoral graduates across the full 130-year period (11,372 dissertations), followed by Physical Sciences (6,760) and Biological Sciences (4,333).
- **Output has grown sharply since the 1960s.** The 2013–2022 decade is the most productive in university history with 4,221 dissertations, roughly 11x the output of the 1893–1902 decade.
- **A small number of advisors supervised a disproportionate share of dissertations.** The top 25 advisors — including William Schweiker (51), Andrew Abbott (45), Wendy Doniger (44), and James J. Heckman (54 when name variants are merged) — collectively account for a significant share of all supervised dissertations with known department affiliations.
- **Chemistry, History, and Economics are the most prolific departments** with over 1,700 dissertations each, reflecting the university's historic strength in these fields.
- **Keyword trends reveal intellectual shifts.** LLM-generated keywords from dissertation titles show fields like Political Science absorbing new subfields (international relations, comparative politics) and Biology fragmenting into specialized committees (Cancer Biology, Immunology, Computational Neuroscience) after the 1990s.

---

## App Structure

The app has four tabs:

| Tab | Content |
|-----|---------|
| **Overview** | Hero section with animated stats + stacked bar chart of dissertation output by division across all 13 decades |
| **Departments** | Top 10 departments per division per decade (dropdown + decade slider) and division trajectory line charts |
| **Campus** | Interactive Leaflet map of dissertation output by UChicago building, sized and colored by count |
| **Advisors** | Top 25 most prolific advisors (bar chart) + Sankey diagram showing advising flow by division |


## Data Sources

- **ProQuest Dissertations & Theses** — 32,013 records with metadata including advisor, department, degree type, subject terms, and keywords (post-1990)
- **HathiTrust Digital Library** — supplementary records for pre-1950 dissertations; limited department metadata
- **Anthropic Claude API (Haiku)** — used to generate normalized intellectual keywords from dissertation titles via batch processing (~32,874 titles processed)


## Data Limitations & Methodological Notes

**Missing department data:** 25,703 of the 32,013 ProQuest records were submitted without a department field — this is a ProQuest data quality issue, not a merging error. Department was imputed for ~18,775 additional records using ProQuest's `Subject.Terms` field (e.g., "Cultural anthropology" → Anthropology). The remaining unimputed records still appear in division-level visualizations.

**Professional Schools excluded from department-level analysis:** The "Professional Schools" division in ProQuest is a systematic miscategorization bucket. Inspection revealed it was dominated by Computer Science records (which properly belong to Physical Sciences), with almost no records for the actual professional schools (Harris School of Public Policy, Crown Family School of Social Work, Chicago Booth). After reassigning Computer Science to Physical Sciences, Professional Schools had effectively zero usable department-level data and was excluded from the departmental visualizations. Division-level counts for Professional Schools (121 dissertations) are still included in the Overview chart.

**Advisor name normalization:** Advisor names in ProQuest are inconsistently recorded across records (e.g., "Heckman, James J.", "Heckman, James", "Heckman, James H."). Names were normalized by extracting last name + first two characters of first name as a deduplication key, then using the longest recorded variant as the canonical display name. This increased Heckman's count from 41 (most common variant alone) to 54 (all variants merged). Some common last names (e.g., Yu) required this two-character approach to avoid merging distinct individuals.

**Keyword generation:** Paper.Keywords in ProQuest only has dense coverage from 1993 onward and was too sparse for longitudinal analysis. LLM-generated keywords from dissertation titles were produced using the Anthropic Batch API (claude-haiku-4-5) with a structured prompt requesting 3 normalized, semicolon-separated conceptual terms per title. 32,558 of 32,874 titles received keywords successfully.


## Running the App

### Requirements
```r
install.packages(c(
  "shiny",
  "tidyverse",
  "plotly",
  "leaflet",
  "scales",
  "zoo",
  "purrr"
))
```

### Data Setup

1. Place the following files in your app directory:
   - `uchicago_dissertations_keywords.csv` — the keywords CSV generated by the Python script (not included in repo due to size; see below to regenerate)
   - `uchicago_locations.csv` — included in repo
   - `www/UChicago_Phoenix_Classic_1Color_Maroon_RGB.png`
   - `www/University Logotype_Maroon_RGB.png`

2. Run `01_clean_data.R` to generate `uchicago_dissertations_analysis.csv`:
```r
source("01_clean_data.R")
```

3. Launch the app:
```r
shiny::runApp()
```

### Regenerating LLM Keywords (optional)

If you need to regenerate the keywords from scratch:
```bash
export ANTHROPIC_API_KEY="your-key-here"
pip install anthropic pandas tqdm
python generate_keywords_batch.py submit
#wait 15–30 minutes
python generate_keywords_batch.py collect
```

## File Structure
```
.
├── 01_clean_data.R                        # Data cleaning and normalization
├── ui.R                                   # Shiny UI
├── server.R                               # Shiny server logic
├── generate_keywords_batch.py             # Anthropic Batch API keyword generation
├── uchicago_dissertations_analysis.csv    # Cleaned dataset (output of 01_clean_data.R)
├── uchicago_locations.csv                 # Building lat/lon for campus map
└── www/
    ├── UChicago_Phoenix_Classic_1Color_Maroon_RGB.png
    └── University Logotype_Maroon_RGB.png
```

## Design Notes

The app uses the official University of Chicago brand identity throughout: maroon (`#800000`), gold (`#C16622`), dark gray (`#767676`), and cream (`#D6D6CE`). Typography uses EB Garamond (headlines) and Libre Franklin (body), the closest web-safe equivalents to the university's official Adobe Garamond Pro and Gotham typefaces.


*MACS 40700: Data Visualization*
Advised by: Jean Clipperton