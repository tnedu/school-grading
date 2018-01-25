library(acct)
library(tidyverse)

pools <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible)

bottom_five <- read_csv("data/bottom_five.csv") %>%
    filter(bottom_five == 1) %>%
    transmute(system, school, final_grade = "F")

ach_grades <- read_csv("data/achievement_grades.csv")
growth_grades <- read_csv("data/growth_grades.csv") %>%
    mutate(subgroup = "All Students")
grad_grades <- read_csv("data/grad_grades.csv")
ready_grad_grades <- read_csv("data/ready_grad_grades.csv")
absenteeism_grades <- read_csv("data/absenteeism_grades.csv")
elpa_grades <- read_csv("data/elpa_grades.csv")

AF_grades_metrics <- pools %>%
    inner_join(ach_grades, by = c("system", "school")) %>%
    left_join(growth_grades, by = c("system", "school", "subgroup")) %>%
    left_join(grad_grades, by = c("system", "school", "subgroup")) %>%
    left_join(ready_grad_grades, by = c("system", "school", "subgroup")) %>%
    left_join(absenteeism_grades, by = c("system", "school", "subgroup")) %>%
    left_join(elpa_grades, by = c("system", "school", "subgroup")) %>%
    mutate_at(vars(starts_with("grade_")), funs(recode(., "A" = 4, "B" = 3, "C" = 2, "D" = 1, "F" = 0))) %>%
# Weights
    mutate(weight_achievement = if_else(!is.na(grade_achievement) & pool == "K8", 0.45, NA_real_),
        weight_achievement = if_else(!is.na(grade_achievement) & pool == "HS", 0.3, weight_achievement),
        weight_growth = if_else(!is.na(grade_growth) & pool == "K8", 0.35, NA_real_),
        weight_growth = if_else(!is.na(grade_growth) & pool == "HS", 0.25, weight_growth),
        weight_grad = if_else(!is.na(grade_grad) & pool == "HS", 0.05, NA_real_),
        weight_ready_grad = if_else(!is.na(grade_ready_grad) & pool == "HS", 0.2, NA_real_),
        weight_opportunity = if_else(!is.na(grade_absenteeism), 0.1, NA_real_),
        weight_elpa = if_else(!is.na(grade_elpa), 0.1, NA_real_),
    # If no ELPA, adjust achievement and growth weights accordingly
        weight_achievement = if_else(is.na(grade_elpa) & !is.na(grade_achievement) & pool == "K8", 0.5, weight_achievement),
        weight_achievement = if_else(is.na(grade_elpa) & !is.na(grade_achievement) & pool == "HS", 0.35, weight_achievement),
        weight_growth = if_else(is.na(grade_elpa) & !is.na(weight_growth) & pool == "K8", 0.4, weight_growth),
        weight_growth = if_else(is.na(grade_elpa) & !is.na(weight_growth) & pool == "HS", 0.3, weight_growth)) %>%
    rowwise() %>%
    mutate(total_weight = sum(weight_achievement, weight_growth, weight_opportunity,
            weight_grad, weight_ready_grad, weight_elpa, na.rm = TRUE),
        subgroup_average = round5(sum(weight_achievement * grade_achievement,
            weight_growth * grade_growth,
            weight_opportunity * grade_absenteeism,
            weight_grad * grade_grad,
            weight_ready_grad * grade_ready_grad,
            weight_elpa * grade_elpa, na.rm = TRUE)/total_weight, 1)) %>%
    ungroup()

# Achievement grades
all_students_grades_final <- AF_grades_metrics %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school, pool, designation_ineligible,
        priority_grad = if_else(subgroup == "All Students", !designation_ineligible & grade_grad_abs == 0, NA),
        achievement_average = subgroup_average,
        achievement_grade = case_when(
            achievement_average > 3 ~ "A",
            achievement_average > 2 ~ "B",
            achievement_average > 1 ~ "C",
            achievement_average > 0 ~ "D",
            achievement_average == 0 ~ "F"
        )
    )

# Targeted support schools
targeted_support <- AF_grades_metrics %>%
    mutate(subgroup_average = if_else(pool == "K8" & total_weight < 0.6, NA_real_, subgroup_average),
        subgroup_average = if_else(pool == "HS" & total_weight < 0.7, NA_real_, subgroup_average)) %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners",
        "Students with Disabilities", "American Indian or Alaska Native", "Asian", "Black or African American",
        "Hispanic", "Native Hawaiian or Other Pacific Islander", "White")) %>%
    select(system, school, subgroup, designation_ineligible, subgroup_average) %>%
    full_join(bottom_five, by = c("system", "school")) %>%
    group_by(subgroup) %>%
    mutate(denom = sum(!is.na(subgroup_average))) %>%
    group_by(subgroup, designation_ineligible, final_grade) %>%
    mutate(rank = rank(subgroup_average, ties.method = "min"),
        targeted_support = if_else(is.na(final_grade) & !designation_ineligible, as.integer(rank <= ceiling(0.05 * denom)), NA_integer_)) %>%
    ungroup() %>%
    select(system, school, subgroup, final_grade, targeted_support) %>%
    spread(subgroup, targeted_support) %>%
    transmute(system, school,
        targeted_support_BHN = `Black/Hispanic/Native American`,
        targeted_support_ED = `Economically Disadvantaged`,
        targeted_support_SWD = `Students with Disabilities`,
        targeted_support_EL = `English Learners`,
        targeted_support_Native = `American Indian or Alaska Native`,
        targeted_support_Asian = Asian,
        targeted_support_Black = `Black or African American`,
        targeted_support_Hispanic = Hispanic,
        targeted_support_HPI = `Native Hawaiian or Other Pacific Islander`,
        targeted_support_White = White,
        targeted_support = if_else(is.na(final_grade), 
            pmax(targeted_support_BHN, targeted_support_ED, targeted_support_SWD, targeted_support_EL,
                targeted_support_Black, targeted_support_Hispanic, targeted_support_Native,
                targeted_support_HPI, targeted_support_Asian, targeted_support_White, na.rm = TRUE), NA_integer_)
    )

# Gap closure grades
subgroup_grades_final <- AF_grades_metrics %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Learners", "Super Subgroup")) %>%
# Drop Super Subgroup observation if other subgroups are present
    mutate(temp = !is.na(subgroup_average)) %>%
    group_by(system, school) %>%
    mutate(subgroups_count = sum(temp)) %>%
    filter(!(subgroup == "Super Subgroup" & subgroups_count > 1)) %>%
# Weight by total weight of the indicators represented by each subgroup
    mutate(subgroup_average_weighted = total_weight * subgroup_average) %>%
    summarise_at(c("total_weight", "subgroup_average_weighted"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(system, school,
        gap_closure_average = round5(subgroup_average_weighted/total_weight, 1),
        gap_closure_grade = case_when(
            gap_closure_average > 3 ~ "A",
            gap_closure_average > 2 ~ "B",
            gap_closure_average > 1 ~ "C",
            gap_closure_average > 0 ~ "D",
            gap_closure_average == 0 ~ "F",
            is.na(gap_closure_average) ~ NA_character_
        )
    )

AF_grades_final <- all_students_grades_final %>%
    full_join(subgroup_grades_final, by = c("system", "school")) %>%
    full_join(bottom_five, by = c("system", "school")) %>%
    full_join(targeted_support, by = c("system", "school")) %>%
    mutate(overall_average = round5(0.6 * achievement_average + 0.4 * gap_closure_average, 1),
        overall_average = if_else(is.na(overall_average), achievement_average, overall_average),
        final_grade = case_when(
            designation_ineligible == 1 ~ NA_character_,
            final_grade == "F" ~ "F",
            priority_grad ~ "F",
            overall_average > 3 ~ "A",
            overall_average > 2 ~ "B",
            overall_average > 1 ~ "C",
            overall_average > 0 ~ "D",
            TRUE ~ final_grade
        ),
        targeted_support = if_else(final_grade == "D", 1L, targeted_support),
        targeted_support = if_else(designation_ineligible == 1, NA_integer_, targeted_support),
        priority_grad = if_else(is.na(priority_grad), FALSE, priority_grad),
        targeted_support = if_else(priority_grad, NA_integer_, targeted_support),
        targeted_support = if_else(is.na(targeted_support), 0L, targeted_support)) %>%
    select(system, school, pool, designation_ineligible, priority_grad,
        achievement_average, achievement_grade, gap_closure_average, gap_closure_grade, overall_average,
        contains("targeted_support"), final_grade)

# Output files
write_csv(AF_grades_metrics, path = "data/AF_bottom_five_amos_metrics.csv", na = "")
write_csv(AF_grades_final, path = "data/AF_bottom_five_amos_final_grades.csv", na = "")
