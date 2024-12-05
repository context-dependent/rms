library(tidyverse)
library(readxl)

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

rmr_t1 <- read_rms_yrs(19:23)

usethis::use_data(rmr_t1, overwrite = TRUE)
