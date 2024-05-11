#' COVID-19 Canada National Case Counts, 2020-2023
#'
#' Daily national COVID-19 case counts in Canada, from the start of the pandemic
#' until the end of 2023. Retrieved from the COVID-19 Canada Open Data Working
#' Group on 2024-05-11.
#'
#' @format A data frame with 1439 observations on 3 variables:
#' \describe{
#'   \item{date}{The date of reporting in YYYY-MM-DD format.}
#'   \item{cases}{The daily number of cases.}
#'   \item{cumulative_cases}{The cumulative number of cases.}
#' }
#'
#' @source \url{https://github.com/ccodwg/CovidTimelineCanada}
"COVIDCanada"

#' COVID-19 Canada Provincial and Territorial Case Counts, 2020-2023
#'
#' Daily COVID-19 case counts for each Canadian province and territory, from the
#' start of the pandemic until the end of 2023. Retrieved from the the COVID-19
#' Canada Open Data Working Group on 2024-05-11.
#'
#' @format A data frame with 16799 observations on 4 variables:
#' \describe{
#'   \item{region}{The two-letter code for the Canadian province or territory.}
#'   \item{date}{The date of reporting in YYYY-MM-DD format.}
#'   \item{cases}{The daily number of cases.}
#'   \item{cumulative_cases}{The cumulative number of cases.}
#' }
#'
#' @source \url{https://github.com/ccodwg/CovidTimelineCanada}
"COVIDCanadaPT"
