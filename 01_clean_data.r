# ============================================================
# 01_clean_data.R
# A Century of Knowledge: Doctoral Production at UChicago
# MACS 40700 — Khushi Desai
#
# PURPOSE: Load, clean, and save an analysis-ready dataset.
# OUTPUT:  uchicago_dissertations_analysis.csv
# ============================================================

library(tidyverse)

# ── 1. Load raw data ─────────────────────────────────────────
# Use the keywords CSV as the base — it has all original columns
# plus LLM_Keywords already baked in from the Anthropic Batch API
cat("Loading data...", "\n")
final <- read.csv("uchicago_dissertations_keywords.csv", stringsAsFactors = FALSE)

# ── 2. Clean Division labels ──────────────────────────────────
final <- final %>%
  mutate(Division_short = case_when(
    str_detect(Division_final, "SOCIAL")       ~ "Social Sciences",
    str_detect(Division_final, "PHYSICAL")     ~ "Physical Sciences",
    str_detect(Division_final, "BIOLOGICAL")   ~ "Biological Sciences",
    str_detect(Division_final, "HUMANITIES")   ~ "Humanities",
    str_detect(Division_final, "DIVINITY")     ~ "Divinity School",
    str_detect(Division_final, "PROFESSIONAL") ~ "Professional Schools",
    TRUE ~ NA_character_
  ))

# ── 3. Impute Department from Subject.Terms where missing ─────
subject_to_dept <- c(
  "Chemistry"                    = "Chemistry",
  "Organic chemistry"            = "Chemistry",
  "Physical chemistry"           = "Chemistry",
  "Literature"                   = "English Language and Literature",
  "British and Irish literature"  = "English Language and Literature",
  "American literature"          = "English Language and Literature",
  "Mathematics"                  = "Mathematics",
  "Religion"                     = "Divinity",
  "Theology"                     = "Divinity",
  "Religious history"            = "Divinity",
  "Political science"            = "Political Science",
  "International relations"      = "Political Science",
  "History"                      = "History",
  "American history"             = "History",
  "European history"             = "History",
  "Education"                    = "Education",
  "Economics"                    = "Economics",
  "Finance"                      = "Economics",
  "Psychology"                   = "Psychology",
  "Physics"                      = "Physics",
  "Particle physics"             = "Physics",
  "Cultural anthropology"        = "Anthropology",
  "Sociology"                    = "Sociology",
  "Philosophy"                   = "Philosophy",
  "Biochemistry"                 = "Biochemistry and Molecular Biology",
  "Molecular biology"            = "Biochemistry and Molecular Biology",
  "Pharmacology"                 = "Biochemistry and Molecular Biology",
  "Botany"                       = "Ecology and Evolution",
  "Zoology"                      = "Ecology and Evolution",
  "Biology"                      = "Ecology and Evolution",
  "Anatomy & physiology"         = "Organismal Biology and Anatomy",
  "Astrophysics"                 = "Astronomy and Astrophysics",
  "Microbiology"                 = "Microbiology",
  "Linguistics"                  = "Linguistics",
  "Geography"                    = "Geophysical Sciences",
  "Geology"                      = "Geophysical Sciences",
  "Geophysics"                   = "Geophysical Sciences",
  "Social work"                  = "Social Service Administration",
  "Genetics"                     = "Genetics, Genomics, and Systems Biology",
  "Music"                        = "Music",
  "Classical studies"            = "Classics",
  "Condensation"                 = "Physics"
)

final <- final %>%
  mutate(first_term = str_extract(Subject.Terms, "(?<=\\[')([^']+)")) %>%
  mutate(Department_imputed = subject_to_dept[first_term])

# ── 4. Build Department_clean ─────────────────────────────────
joint_pattern <- paste(
  "\\s+and Linguistics$",
  "\\s+and History$",
  "\\s+and Anthropology$",
  "\\s+and Sociology$",
  "\\s+and Philosophy$",
  "\\s+and Divinity$",
  "\\s+and Classics$",
  "\\s+and Social Thought$",
  "\\s+and Cinema and Media Studies$",
  "\\s+and Theater and Performance Studies$",
  "\\s+and Comparative Literature$",
  "\\s+and Germanic Studies$",
  "\\s+and Art History$",
  "\\s+and English$",
  "\\s+and Business$",
  "\\s+and Psychology$",
  "\\s+and Economics$",
  "\\s+& Media Studies.*$",
  sep = "|"
)

final <- final %>%
  mutate(Department_clean = case_when(
    !is.na(Department_norm) & Department_norm != "" ~ Department_norm,
    !is.na(Department_imputed)                      ~ Department_imputed,
    TRUE                                            ~ NA_character_
  )) %>%
  mutate(Department_clean = str_remove(Department_clean,
                                       "Interdisciplinary Scientist Training Program:\\s*")) %>%
  mutate(Department_clean = str_remove(Department_clean, joint_pattern)) %>%
  mutate(Department_clean = str_to_title(str_trim(Department_clean))) %>%
  mutate(Department_clean = str_replace_all(Department_clean, " And ", " and ")) %>%
  mutate(Department_clean = case_when(
    Department_clean == "Sociology and Anthropology"                 ~ "Anthropology",
    Department_clean == "Business and Economics"                     ~ "Economics",
    Department_clean == "Economics and Business"                     ~ "Economics",
    Department_clean == "Geology and Paleontology"                   ~ "Geophysical Sciences",
    Department_clean == "Hygiene and Bacteriology"                   ~ "Microbiology",
    Department_clean == "Physiological Chemistry and Pharmacology"   ~ "Biochemistry and Molecular Biology",
    Department_clean == "Oriental Languages and Literatures"         ~ "Near Eastern Languages and Civilizations",
    Department_clean == "History of Art"                             ~ "Art History",
    Department_clean == "Mathematical Astronomy"                     ~ "Astronomy and Astrophysics",
    Department_clean == "Practical Astronomy and Astrophysics"       ~ "Astronomy and Astrophysics",
    Department_clean == "The School of Commerce and Administration"   ~ "Business",
    Department_clean == "Home Economics and Household Administration" ~ NA_character_,
    Department_clean == "B. Coleman Renick"                          ~ NA_character_,
    Department_clean == ""                                           ~ NA_character_,
    TRUE ~ Department_clean
  ))

# ── 5. Summary ────────────────────────────────────────────────
cat("\n── Dataset Summary ──────────────────────────────────", "\n")
cat("Total records:            ", nrow(final), "\n")
cat("With Division:            ", sum(!is.na(final$Division_short)), "\n")
cat("With Department (clean):  ", sum(!is.na(final$Department_clean)), "\n")
cat("With LLM keywords:        ", sum(!is.na(final$LLM_Keywords)), "\n")
cat("Year range:               ", min(final$Year, na.rm=TRUE), "-", max(final$Year, na.rm=TRUE), "\n")
cat("Unique departments:       ", n_distinct(final$Department_clean, na.rm=TRUE), "\n")
cat("Unique advisors:          ", n_distinct(final$Advisors[!is.na(final$Advisors) & final$Advisors != ""]), "\n")

# ── 6. Save ───────────────────────────────────────────────────
write.csv(final, "uchicago_dissertations_analysis.csv", row.names = FALSE)


