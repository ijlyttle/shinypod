#' Weather observations at Ames - Jan. 2014
#'
#' A dataset containing hourly weather observations at Ames, Iowa (US) for
#' the month of January 2014. Data obtained from Weather Undergound's API,
#' using the \code{rwunderground} package.
#'
#' @format A data frame with 983 rows and 19 variables:
#' \describe{
#'   \item{date}{POSIXct, instant of the observation}
#'   \item{temp}{double, dry-bulb temperature, °C}
#'   \item{dew_pt}{double, dew-point temperature, °C}
#'   \item{hum}{double, relative humidity, \%}
#'   \item{wind_spd}{double, wind-speed, m/s}
#'   \item{wind_gust}{double, wind-gust, m/s}
#'   \item{dir}{character, direction from which wind blows}
#'   \item{vis}{double, visibility, km}
#'   \item{pressure}{double, sea-level pressure, mbar}
#'   \item{wind_chill}{double, wind-chill temperature, °C}
#'   \item{heat_index}{double, head-index temperature, °C}
#'   \item{precip}{double, precipitation since previous observation, mm}
#'   \item{cond}{character, description of conditions}
#'   \item{fog}{integer, indicates if fog is present}
#'   \item{rain}{integer, indicates if it is raining is present}
#'   \item{snow}{integer, indicates if it is snowing}
#'   \item{hail}{integer, indicates if it is hailing}
#'   \item{thunder}{integer, indicates if thunder is present}
#'   \item{tornado}{integer, indicates if a tornado is present}
#' }
#' @source \url{http://www.wunderground.com/}
"wx_ames"
