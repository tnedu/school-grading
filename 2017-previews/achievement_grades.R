library(acct)
library(tidyverse)

subgroups <- c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
    "English Learners with T1/T2", "Students with Disabilities", "Super Subgroup",
    "American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic",
    "Native Hawaiian or Other Pacific Islander", "White")

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

pools <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool) %>%
    filter(!is.na(pool))

school_base <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_for_accountability.csv",
    col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    inner_join(pools, by = c("system", "school"))

# 2016 HS Success Rate
hs_amos <- school_base %>%
    filter(year == 2016,
        pool == "HS",
        subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc),
        grade %in% as.character(3:12),
        subgroup %in% subgroups) %>%
    mutate(grade = as.numeric(grade),
        subgroup = if_else(subgroup == "English Learners with T1/T2", "English Learners", subgroup),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, pool, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress below 30 at subject level
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0, .))) %>%
    group_by(year, system, school, pool, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(system, school, pool, subgroup,
        valid_tests_prior = valid_tests,
        pct_on_mastered_prior = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
        AMO_target = amo_target(valid_tests_prior, pct_on_mastered_prior),
        AMO_target_4 = amo_target(valid_tests_prior, pct_on_mastered_prior, double = TRUE))
    
# 2015 K-8 Success Rate
k8_success_2015 <- read_csv("K:/ORP_accountability/data/2015_sas_accountability/ASD included grades/school_base_2015_19jul2015.csv") %>%
    mutate(system = as.integer(system),
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners with T1/T2",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "Native American" ~ "American Indian or Alaska Native",
            TRUE ~ subgroup 
        )
    ) %>% 
    left_join(pools, by = c("system", "school")) %>%
    filter(year == 2015,
        pool == "K8",
        subject %in% c("Math", "RLA", "Science", math_eoc, english_eoc, science_eoc),
        grade %in% as.character(3:12),
        subgroup %in% subgroups) %>%
    mutate(grade = as.integer(grade),
        subgroup = case_when(
            subgroup == "English Learners with T1/T2" ~ "English Learners",
            TRUE ~ subgroup 
        ),
        subject = case_when(
               subject %in% math_eoc & grade %in% 3:8 ~ "Math",
               subject %in% english_eoc & grade %in% 3:8 ~ "RLA",
               subject %in% science_eoc & grade %in% 3:8 ~ "Science",
               TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, pool, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_prof", "n_adv"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress below 30 at subject level
    mutate_at(c("valid_tests", "n_prof", "n_adv"), funs(if_else(valid_tests < 30, 0L, .))) %>%
    group_by(year, system, school, pool, subgroup) %>%
    summarise_at(c("valid_tests", "n_prof", "n_adv"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(system, school, pool, subgroup,
        valid_tests_prior = valid_tests,
        pct_on_mastered_prior = if_else(valid_tests != 0, round5(100 * (n_prof + n_adv)/valid_tests, 1), NA_real_))

# 2017 Success Rates
success_rate_2017 <- school_base %>%
    filter(year == 2017,
        subgroup %in% subgroups, grade %in% as.character(3:12),
        subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc),
        grade %in% as.character(3:12)) %>%
    mutate(grade = as.numeric(grade),
        subgroup = if_else(subgroup == "English Learners with T1/T2", "English Learners", subgroup),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(system, school, pool, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress below 30 at subject level
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0, .))) %>%
    group_by(system, school, pool, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(system, school, pool, subgroup, valid_tests,
        pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
        upper_bound_ci = ci_upper_bound(valid_tests, pct_on_mastered)) %>%
    left_join(bind_rows(hs_amos, k8_success_2015), by = c("system", "school", "subgroup", "pool")) %>%
# K8 schools only count toward denominator if they have success rate in 2015 and 2017
    mutate(both_years = pool == "K8" & !is.na(pct_on_mastered) & !is.na(pct_on_mastered_prior)) %>%
    group_by(pool, subgroup) %>%
    mutate(denom = sum(both_years, na.rm = TRUE)) %>%
    group_by(pool, subgroup, both_years) %>%
# Rank success rates for K8
    mutate(rank_success = if_else(!is.na(pct_on_mastered) & both_years, rank(pct_on_mastered, ties.method = "max"), NA_integer_),
        rank_success_prior = if_else(!is.na(pct_on_mastered_prior) & both_years, rank(pct_on_mastered_prior, ties.method = "max"), NA_integer_)) %>%
    ungroup() %>%
# Calculate below percentile change
    mutate(pctile_current = round5(100 * rank_success/denom, 1),
        pctile_prior = round5(100 * rank_success_prior/denom, 1),
        pctile_change = pctile_current - pctile_prior)

ach_grades <- success_rate_2017 %>%
    transmute(system, school, subgroup,
        pct_on_mastered_prior, pct_on_mastered, upper_bound_ci, AMO_target, AMO_target_4, pctile_current, pctile_prior,
        grade_achievement_abs = case_when(
            pct_on_mastered >= 50 ~ "A",
            pct_on_mastered >= 45 ~ "B",
            pct_on_mastered >= 35 ~ "C",
            pct_on_mastered >= 25 ~ "D",
            pct_on_mastered < 25 ~ "F"
        ),
        grade_achievement_target = case_when(
            pool == "HS" & pct_on_mastered >= AMO_target_4 ~ "A",
            pool == "HS" & pct_on_mastered > AMO_target ~ "B",
            pool == "HS" & upper_bound_ci >= AMO_target ~ "C",
            pool == "HS" & upper_bound_ci > pct_on_mastered_prior ~ "D",
            pool == "HS" & upper_bound_ci <= pct_on_mastered_prior ~ "F",
            pool == "K8" & pctile_change >= 10 ~ "A",
            pool == "K8" & pctile_change > 0 ~ "B",
            pool == "K8" & round5(pctile_change, 1) >= -2 ~ "C",
            pool == "K8" & round5(pctile_change, 1) >= -10 ~ "D",
            pool == "K8" & pctile_change < -10 ~ "F"
        ),
        grade_achievement_target = if_else(pool == "K8" & pctile_current >= 95 & pctile_prior >= 95, "B", grade_achievement_target),
    # Not setting na.rm = TRUE because schools need both absolute and AMO to get a grade
        grade_achievement = pmin(grade_achievement_abs, grade_achievement_target)
    )

# Ensure one entry per school/subgroup combination
all_school_subgroups <- ach_grades %>%
    select(system, school, subgroup, grade_achievement) %>%
    spread(subgroup, grade_achievement) %>%
    gather(subgroup, grade_achievement, `All Students`:`White`) %>%
    select(system, school, subgroup) %>%
    left_join(ach_grades, by = c("system", "school", "subgroup")) %>%
    arrange(system, school, subgroup)

write_csv(all_school_subgroups, path = "data/achievement_grades.csv", na = "")
