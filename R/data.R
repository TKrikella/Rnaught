#' COVID-19 Canada National Case Counts, 2020-2022
#'
#' Daily national COVID-19 case counts in Canada, from the start of the pandemic
#' until the end of 2022. Provided by the COVID-19 Canada Open Data Working
#' Group.
#'
#' @format A data frame with 1082 observations on 3 variables:
#' \describe{
#'   \item{date}{The date of reporting in YYYY-MM-DD format.}
#'   \item{cases}{The daily number of cases.}
#'   \item{cumulative_cases}{The cumulative number of cases.}
#' }
#'
#' @source \url{https://opencovid.ca}
"COVIDCanada"

#' COVID-19 Canada Provincial and Territorial Case Counts, 2020-2022
#'
#' Daily COVID-19 case counts for each Canadian province and territory, from the
#' start of the pandemic until the end of 2022. Provided by the COVID-19 Canada
#' Open Data Working Group.
#'
#' @format A data frame with 13169 observations on 4 variables:
#' \describe{
#'   \item{region}{The two-letter code for the Canadian province or territory.}
#'   \item{date}{The date of reporting in YYYY-MM-DD format.}
#'   \item{cases}{The daily number of cases.}
#'   \item{cumulative_cases}{The cumulative number of cases.}
#' }
#'
#' @source \url{https://opencovid.ca}
"COVIDCanadaPT"
