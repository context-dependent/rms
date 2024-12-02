library(tidyverse)
library(readxl)

YEAR <- "23"

p23 <- here::here("data-raw", "rmr-canada-2023-en.xlsx")
s23 <- readxl::excel_sheets(p23)
d23 <- readxl::read_excel(p23, sheet = s23[2], skip = 3, col_names = FALSE) |> 
  select(-where(~is.na(.x[2])))


p19 <- here::here("data-raw", "rmr-canada-2019-en.xlsx")
s19 <- readxl::excel_sheets(p19)
d19 <- readxl::read_excel(p19, sheet = s19[1], skip = 3, col_names = FALSE) |> 
  select(-where(~is.na(.x[2])))

d19 |> View()

fill_forward <- function(x) {
  x_ <- x
  for (i in seq_along(x_)) {
    if (i == 1 && is.na(x_[i])) {
      x_[i] <- ""
    } else if (is.na(x_[i])) {
      x_[i] <- x_[i - 1]
    }
  }
  x_
}

fix_rms_t1_colnames <- function(d) {
  v <- 
    stringr::str_c(
      fill_forward(stringr::str_remove_all(d[1, ], "\\(.*\\)")), 
      fill_forward(d[2, ]), 
      sep = " "
    ) |> 
    stringr::str_replace_all("\\s+", " ") |> 
    stringr::str_trim()
  
  colnames(d) <- v |> 
    stringr::str_replace("Percentage.*(Oct-\\d\\d)$", "amr_2br_fixed_lag_delta \\1") |> 
    stringr::str_replace("Average.*(Oct-\\d\\d)$", "amr_2br \\1") |> 
    stringr::str_replace("Vacancy.*(Oct-\\d\\d)$", "vacancy \\1") |>
    stringr::str_replace("Turnover.*(Oct-\\d\\d)$", "turnover \\1")

  d |> 
    dplyr::slice(-c(1:2))
}

drop_note_rows <- function(d) {
  d[!is.na(d[[2]]), ]
}

pivot_yearly <- function(d) {
  d |> 
    tidyr::pivot_longer(
      matches("vacancy|turnover|amr_2br"), 
      names_pattern = ("([^\\s]+) Oct-(\\d\\d)"), 
      names_to = c(".value", "year"), 
      names_transform = list(
        year = ~as.integer(.x) + 2000
      )
    ) |> 
    dplyr::mutate(
      dplyr::across(
        vacancy:amr_2br_fixed_lag_delta, 
        ~ .x |>
          stringr::str_trim() |> 
          stringr::str_replace("\\+\\+", "0.0") |> 
          stringr::str_replace("\\*\\*", "") |>  
          readr::parse_number()
      )
    ) |> 
    dplyr::mutate(
      dplyr::across(
        c(vacancy, turnover, amr_2br_fixed_lag_delta),
        ~ .x * 0.01
      )
    )
}

fix_location_cols <- function(d) {
  d |> 
    dplyr::rename(centre = Centre) |> 
    dplyr::mutate(
      province = dplyr::case_when(
        centre |> stringr::str_detect("10,000\\+") 
          ~ centre |> stringr::str_extract("^[^\\d]+") |>
            stringr::str_trim(),
        TRUE ~ NA_character_
      ) |> fill_forward(), 
      centre = dplyr::case_when(
        centre |> stringr::str_detect("Belleville") ~ "Belleville CMA",
        TRUE ~ centre
      )
    )
}

read_rms_yr <- function(yy) {
  p <- here::here("data-raw", glue::glue("rmr-canada-20{yy}-en.xlsx"))
  s <- readxl::excel_sheets(p)
  s_ <- s[grep("Table 1.0", s)]
  d <- 
    suppressMessages(
      readxl::read_excel(p, sheet = s_, skip = 3, col_names = FALSE)
    ) |> 
    dplyr::select(-where(~is.na(.x[2])))
  d |> 
    fix_rms_t1_colnames() |> 
    drop_note_rows() |> 
    pivot_yearly() |> 
    fix_location_cols()
}

read_rms_yrs <- function(yys) {
  amr_index_var <- glue::glue("amr_2br_indexed_{min(yys) - 2}")
  
  d <- yys |> 
    purrr::map_dfr(read_rms_yr) |> 
    dplyr::arrange(centre, year) |> 
    dplyr::group_by(centre, year) |>
    dplyr::slice(1) |> 
    dplyr::group_by(centre) |>  
    dplyr::mutate(
      amr_2br_fixed_lag_delta = dplyr::case_when(
        is.na(amr_2br_fixed_lag_delta) ~ amr_2br / lag(amr_2br) - 1, 
        TRUE ~ amr_2br_fixed_lag_delta
      ),
      {{amr_index_var}} := as.integer(cumprod(1 + amr_2br_fixed_lag_delta) * 100)
    ) |> 
    dplyr::ungroup()

  d |> 
    dplyr::bind_rows(
      d |> 
        dplyr::group_by(province, centre) |> 
        dplyr::summarize(
          year = min(year) - 1L,
          {{amr_index_var}} := 100L,
          .groups = "drop"
        )
    ) |> 
    dplyr::ungroup()
}

rms_t1 <- read_rms_yrs(19:23)

rms_t1 |> count(year)
rms_t1 |> 
  arrange(centre, year) |> select(matches("amr"))

local_theme <- function() {
  bptheme::theme_blueprint(
    plot_background = "white", 
    base_size = 14, 
    strip_text_size = 14
  )
}

rms_t1 |> 
  filter(centre |> str_detect(province), year == 2018) |> 
  arrange(amr_2br) |> 
  mutate(centre = fct_inorder(centre)) |>  
  ggplot(aes(amr_2br, centre)) + 
  geom_col(aes(fill = centre |> str_detect("Ontario"))) + 
  geom_text(aes(x = 4, label = scales::dollar(amr_2br)), hjust = 0, size = 14, size.unit = "pt", colour = "white") +
  bpscales::scale_fill_blueprint(guide = "none", discrete = TRUE, type = "bipolar", option = "blue_green") +
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1), expand = expansion(mult = c(0, .05))) +
  local_theme() + 
  labs(x = "Average Market Rent (2br, 2017)", y = NULL)

rms_t1 |> 
  filter(year == 2023, centre |> str_detect(province)) |>
  arrange(amr_2br_indexed_17) |> 
  mutate(centre = fct_inorder(centre)) |>  
  ggplot(aes(amr_2br_indexed_17 - 100, centre)) + 
  geom_col(aes(fill = centre |> str_detect("Ontario"))) + 
  geom_text(aes(x = 1, label = scales::percent(amr_2br_indexed_17 - 100, scale = 1, accuracy = 1)), hjust = 0, size = 14, size.unit = "pt", colour = "white") +
  bpscales::scale_fill_blueprint(guide = "none", discrete = TRUE, type = "bipolar", option = "blue_green") + 
  scale_x_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), expand = expansion(mult = c(0, .05))) + 
  local_theme() + 
  labs(x = "AMR Growth (2017-23)", y = NULL)

rms_t1 |> 
  filter(year == 2023, province == "Ontario") |>
  arrange(amr_2br_indexed_17) |> 
  mutate(centre = fct_inorder(centre)) |>  
  ggplot(aes(amr_2br_indexed_17 - 100, centre)) + 
  geom_col(aes(fill = centre |> str_detect("Peterborough"))) + 
  geom_text(aes(x = 1, label = scales::percent(amr_2br_indexed_17 - 100, scale = 1, accuracy = 1)), hjust = 0, size = 14, size.unit = "pt", colour = "white") +
  bpscales::scale_fill_blueprint(guide = "none", discrete = TRUE, type = "bipolar", option = "blue_green") + 
  scale_x_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), expand = expansion(mult = c(0, .05))) + 
  local_theme()

rms_t1 |>
  filter(province == "Ontario") |> 
  ggplot(aes(year, amr_2br_indexed_17)) + 
  geom_smooth(aes(group = centre, colour = centre == "Peterborough CMA"), alpha = .3, se = FALSE) +
  bpscales::scale_colour_blueprint(guide = "none", discrete = TRUE, type = "bipolar", option = "blue_green")

rms_t1 |> 
  filter(province == "Ontario", year %in% c(2018, 2023)) |> 
  ggplot(aes(year, amr_2br)) + 
  geom_path(aes(group = centre, colour = centre == "Peterborough CMA"), size = 1) 
