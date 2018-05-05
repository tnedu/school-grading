library(acct)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

pools_immune <- read_csv("N:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible)

high_schools <- ceiling(0.05 * sum(pools_immune$pool == "HS", na.rm = TRUE))
k8_schools <- ceiling(0.05 * sum(pools_immune$pool == "K8", na.rm = TRUE))

one_year_success <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_for_accountability.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    inner_join(pools_immune, by = c("system", "school")) %>%
    filter(subgroup == "All Students",
       subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc),
       grade %in% as.character(3:12)) %>%
    mutate(grade = as.numeric(grade),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, pool, designation_ineligible, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress below 30 at subject/year level
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0, .))) %>%
    group_by(system, school, pool, designation_ineligible, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(system, school, pool, designation_ineligible, subgroup, valid_tests,
        pct_on_mastered = round5(100 * (n_on_track + n_mastered)/valid_tests, 1))

bottom_five <- one_year_success %>%
    group_by(pool, designation_ineligible) %>%
    mutate(rank_OM = if_else(valid_tests >= 30, rank(pct_on_mastered, ties.method = "min"), NA_integer_)) %>%
    ungroup() %>%
    mutate(bottom_five = if_else(designation_ineligible == 0 & pool == "HS" & rank_OM <= high_schools, 1, 0),
        bottom_five = if_else(designation_ineligible == 0 & pool == "K8" & rank_OM <= k8_schools, 1, bottom_five))

write_csv(bottom_five, path = "data/bottom_five.csv", na = "")
