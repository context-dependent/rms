#' Rental Market Report Data Tables, T 1.0
#'
#' @source CMHC Rental Market Survey Data Tables 
#'  <https://www.cmhc-schl.gc.ca/professionals/housing-markets-data-and-research/housing-data/data-tables/rental-market/rental-market-report-data-tables>
#' @format Data frame with columns: 
#' \describe{
#' \item{centre}{Population centre name}
#' \item{year}{Year of the observation}
#' \item{vacancy, turnover}{Vacancy and turnover rates observed in October of \code{year}, as a porportion.}
#' \item{amr_2br}{Average monthly rent for an in-sample 2-bedroom unit in October of \code{yaer}}
#' \item{amr_2br_fixed_lag_delta}{For a fixed subset of units, proportional change in \code{amr_2br} between \code{year - 1} to \code{year}}
#' }
"rmr_t1"


#' @importFrom tibble tibble
NULL