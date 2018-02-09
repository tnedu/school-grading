library(acct)
library(haven)
library(tidyverse)

grad_target <- read_dta("K:/ORP_accountability/projects/Jessica/2018 Accountability Winter Preview/Data/School Accountability/grad_rate2016_targets.dta") %>%
    filter(!subgroup %in% c("Non-Economically Disadvantaged", "Non-English Language Learners with T1/T2", "Non-Students with Disabilities")) %>%
    transmute(system, school,
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
        subgroup = if_else(subgroup == "Hawaiian or Pacific Islander", "Native Hawaiian or Other Pacific Islander", subgroup),
        grad_rate_prior = if_else(grad_cohort >= 30, grad_rate, NA_real_),
        AMO_target = round5(AMO_target2015, 1), AMO_target_4 = round5(AMO_target2015_double, 1))

grad <- read_dta("K:/ORP_accountability/data/2016_graduation_rate/School_grad_rate2017_JP.dta") %>%
    filter(!subgroup %in% c("Non-Economically Disadvantaged", "Non-English Language Learners with T1/T2", "Non-Students with Disabilities")) %>%
    mutate(subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
        subgroup = if_else(subgroup == "Hawaiian or Pacific Islander", "Native Hawaiian or Other Pacific Islander", subgroup)) %>%
    select(system, school, subgroup, grad_cohort, grad_rate) %>%
    left_join(grad_target, by = c("system", "school", "subgroup")) %>%
    mutate(upper_bound_ci = ci_upper_bound(grad_cohort, grad_rate)) %>%
    transmute(system, school, subgroup,
        grade_grad_abs = case_when(
            grad_cohort < 30 ~ NA_character_,
            grad_rate >= 95 ~ "A",
            grad_rate >= 90 ~ "B",
            grad_rate >= 80 ~ "C",
            grad_rate >= 67 ~ "D",
            grad_rate < 67 ~ "F"
        ),
        grade_grad_target = case_when(
            grad_cohort < 30 ~ NA_character_,
            grad_rate >= AMO_target_4 ~ "A",
            grad_rate > AMO_target ~ "B",
            upper_bound_ci >= AMO_target ~ "C",
            upper_bound_ci > grad_rate_prior ~ "D",
            upper_bound_ci <= grad_rate_prior ~ "F"
        ),
        grade_grad = pmin(grade_grad_abs, grade_grad_target)
    )

write_csv(grad, path = "data/grad_grades.csv", na = "")

ready_grad_target <- read_dta("K:/ORP_accountability/projects/Jessica/2018 Accountability Winter Preview/Data/School Accountability/readygrad_school2016_winterpreview.dta") %>%
    filter(!subgroup %in% c("Non-Economically Disadvantaged", "Non-English Language Learners with T1/T2", "Non-Students with Disabilities")) %>%
    transmute(system = as.integer(system), school = as.integer(school),
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners",
            subgroup == "Hawaiian or Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native American" ~ "American Indian or Alaska Native",
            TRUE ~ subgroup
        ),
        ACT_grad_prior = if_else(eligble == 1, ready_graduates, NA_real_),
        AMO_target = AMO_target2017, AMO_target_4 = AMO_target2017_double
    )

ready_grad <- read_csv("K:/ORP_accountability/projects/2018_amo/school_readygrad_AMO_targets2018_JW_individualsubgroups.csv") %>%
    filter(!subgroup %in% c("Non-Economically Disadvantaged", "Non-Students with Disabilities")) %>%
    mutate(subgroup = case_when(
        subgroup == "English Language Learners with T1/T2" ~ "English Learners",
        TRUE ~ subgroup)
    ) %>%
    select(system, school, subgroup, ACT_grad = ready_grad2) %>%
    left_join(ready_grad_target, by = c("system", "school", "subgroup")) %>%
    transmute(system, school, subgroup,
        grade_ready_grad_abs = case_when(
            ACT_grad >= 40 ~ "A",
            ACT_grad >= 30 ~ "B",
            ACT_grad >= 25 ~ "C",
            ACT_grad >= 16 ~ "D",
            ACT_grad < 16 ~ "F"
        ),
        grade_ready_grad_target = case_when(
            ACT_grad >= AMO_target_4 ~ "A",
            ACT_grad >= AMO_target ~ "B",
            ACT_grad > ACT_grad_prior ~ "C",
            ACT_grad == ACT_grad_prior ~ "D",
            ACT_grad < ACT_grad_prior ~ "F"
        ),
        grade_ready_grad = pmin(grade_ready_grad_abs, grade_ready_grad_target)
    )

write_csv(ready_grad, path = "data/ready_grad_grades.csv", na = "")
