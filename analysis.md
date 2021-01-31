# Analyzing the results of *Registered Replication Report: Strack, Martin, and Stepper (1988)*

Josh Cook <br>
January 30, 2021

## Data preparation

``` r
subjects_to_remove <- c("63", "39")

data <- read_csv("data/Wagenmakers-data_modified.csv") %.% {
  janitor::clean_names()
  mutate(
    subject_number = factor(subject_number),
    participant_id = factor(participant_id),
    across(starts_with("cartoon") & ends_with("correct"), as.logical),
    aware_of_goal = as.logical(aware_of_goal),
    comprehension_of_cartoons = as.logical(comprehension_of_cartoons),
    student = as.logical(student),
    gender = case_when(
      gender == 1 ~ "M",
      gender == 0 ~ "F",
      TRUE ~ NA_character_
    )
  )
  filter(!subject_number %in% subjects_to_remove)
}
```

    #>
    #> ── Column specification ────────────────────────────────────────────────────────
    #> cols(
    #>   .default = col_double(),
    #>   guess_of_purpose = col_character(),
    #>   `Occupation / Field of study` = col_character()
    #> )
    #> ℹ Use `spec()` for the full column specifications.

``` r
head(data)
```

    #> # A tibble: 6 x 22
    #>   subject_number participant_id condition cartoon_1_corre… cartoon_2_corre…
    #>   <fct>          <fct>              <dbl> <lgl>            <lgl>
    #> 1 1              1                      0 TRUE             TRUE
    #> 2 2              2                      1 TRUE             TRUE
    #> 3 3              3                      0 TRUE             TRUE
    #> 4 4              4                      1 TRUE             TRUE
    #> 5 5              5                      0 TRUE             TRUE
    #> 6 6              6                      1 FALSE            FALSE
    #> # … with 17 more variables: cartoon_3_correct <lgl>, cartoon_4_correct <lgl>,
    #> #   total_correct <dbl>, task_1 <dbl>, task_2 <dbl>, cartoon_1_rating <dbl>,
    #> #   cartoon_2_rating <dbl>, cartoon_3_rating <dbl>, cartoon_4_rating <dbl>,
    #> #   self_reported_task_performance <dbl>, comprehension_of_cartoons <lgl>,
    #> #   aware_of_goal <lgl>, guess_of_purpose <chr>, age <dbl>, gender <chr>,
    #> #   student <lgl>, occupation_field_of_study <chr>

``` r
naniar::miss_var_summary(data) %>% filter(n_miss > 0)
```

    #> # A tibble: 3 x 3
    #>   variable                  n_miss pct_miss
    #>   <chr>                      <int>    <dbl>
    #> 1 guess_of_purpose               2    1.11
    #> 2 cartoon_2_rating               1    0.556
    #> 3 occupation_field_of_study      1    0.556

``` r
data <- data %.% {
  select(-contains("task"), -guess_of_purpose)
  pivot_longer(cols = c(contains("cartoon"), -comprehension_of_cartoons))
  mutate(name = str_remove(name, "^cartoon_"))
  separate(name, into = c("cartoon_number", "variable"), sep = "_")
  pivot_wider(names_from = variable, values_from = value)
  mutate(
    correct = as.logical(correct),
    cartoon_number = glue::glue("Cartoon {cartoon_number}"),
    condition = ifelse(condition, "frown", "smile")
  )
}
```

Exclude data:

-   were aware of the research goal
-   did not understand the cartoons

``` r
excluded_subjects <- data %.% {
  filter(aware_of_goal | !comprehension_of_cartoons | total_correct <= 2)
  u_pull(subject_number)
}

data <- data %>%
  mutate(excluded_data = subject_number %in% excluded_subjects)
```

48 subjects were excluded from the data.

## Data visualization

``` r
data %>%
  filter(!is.na(rating)) %>%
  ggplot(aes(x = condition, y = rating)) +
  facet_wrap(~cartoon_number) +
  geom_boxplot(
    aes(color = condition, fill = condition),
    alpha = 0.2,
    outlier.shape = NA
  ) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(x = NULL, y = "fraction")
```

![](analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
data %>%
  filter(!is.na(rating)) %>%
  ggplot(aes(x = condition, y = rating)) +
  facet_wrap(~cartoon_number) +
  geom_point(
    aes(color = gender),
    size = 1,
    alpha = 0.5,
    position = position_jitterdodge(0.2, 0.25, seed = 0)
  ) +
  geom_boxplot(
    aes(color = gender, fill = gender),
    alpha = 0.2,
    outlier.shape = NA
  ) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(x = NULL, y = "fraction")
```

![](analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
rating_counts <- data %.% {
  filter(!is.na(rating) & !excluded_data)
  count(condition, rating, cartoon_number)
  tidyr::complete(condition, rating, cartoon_number, fill = list(n = 0))
  mutate(cartoon_number = glue::glue("cartoon {cartoon_number}"))
  group_by(condition, cartoon_number)
  mutate(frac = n / sum(n))
  ungroup()
}


rating_counts %>%
  ggplot(aes(x = factor(rating), y = n)) +
  facet_grid(condition ~ cartoon_number) +
  geom_col(aes(fill = condition), position = "dodge") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_y_continuous(expand = expansion(c(0, 0.02))) +
  labs(
    x = "cartoon rating",
    y = "count",
    fill = "condition"
  )
```

![](analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
rating_counts %>%
  ggplot(aes(x = factor(rating), y = frac)) +
  facet_wrap(~cartoon_number) +
  geom_line(
    aes(group = condition, color = condition),
    alpha = 0.5,
    size = 1
  ) +
  geom_point(aes(color = condition), size = 2, alpha = 0.8) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_y_continuous(expand = expansion(c(0.01, 0.02))) +
  labs(
    x = "cartoon rating",
    y = "fraction",
    color = "condition"
  )
```

![](analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
rating_counts %.%
  {
    select(-n)
    pivot_wider(names_from = condition, values_from = frac)
    mutate(difference = smile - frown)
  } %>%
  ggplot(aes(x = factor(rating), y = difference)) +
  facet_wrap(~cartoon_number) +
  geom_hline(yintercept = 0) +
  geom_linerange(
    aes(ymax = difference),
    ymin = 0,
    alpha = 0.2,
    size = 1
  ) +
  geom_line(
    aes(color = difference),
    group = 1,
    alpha = 0.3,
    size = 1.3
  ) +
  geom_point(aes(color = difference), size = 2.5) +
  scale_color_gradient2(
    low = "blue",
    high = "red",
    mid = "grey70",
    guide = FALSE
  ) +
  labs(
    x = "cartoon rating",
    y = "diff. fraction (smile - frown)"
  )
```

![](analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
rating_summaries <- data %>%
  filter(!is.na(rating)) %>%
  group_by(
    participant_id,
    condition,
    gender,
    comprehension_of_cartoons,
    correct,
    excluded_data
  ) %>%
  summarize(
    avg_rating = mean(rating),
    avg_age = mean(age)
  ) %>%
  ungroup()
```

``` r
rating_summaries %>%
  ggplot(aes(x = avg_age, y = avg_rating)) +
  geom_jitter(
    aes(color = condition, shape = excluded_data),
    width = 0.5,
    height = 0.1,
    size = 1.5,
    alpha = 0.7
  ) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_shape_manual(values=c(19, 4), labels=c("included", "excluded")) +
  labs(
    x = "avgerage age",
    y = "average cartoon rating",
    color = "condition",
    shape = NULL
  )
```

![](analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
