library("lubridate")
library("readr")

context("df_with_tz")

tz_new <- "Europe/Paris"
wx_ames_new <- df_with_tz(wx_ames, tz_new)

test_that("we can set the time columns in a dataframe", {
  expect_equal(tz(wx_ames_new$date), tz_new)
})

context("df_names_inherits")

names_posixct <- "date"
names_numeric <-
  c("temp", "dew_pt", "wind_spd", "wind_gust", "vis", "pressure", "wind_chill", "heat_index", "precip")
names_character <- c("dir", "cond")
names_integer <- c("hum", "fog", "rain", "snow", "hail", "thunder", "tornado")

test_that("we can find columns with given classes", {
  expect_equal(df_names_inherits(wx_ames, "POSIXct"), names_posixct)
  expect_equal(df_names_inherits(wx_ames, "numeric"), names_numeric)
  expect_equal(df_names_inherits(wx_ames, "character"), names_character)
  expect_equal(df_names_inherits(wx_ames, "integer"), names_integer)
})


context("df_has_time_8601")

# some setup
df_ref <- data_frame(
  int = c(1L, 2L, 3L),
  dbl = c(1, 2, 3),
  char = c("a", "b", "c"),
  dtm_a = ymd("2012-01-02") + hours(seq(1, 3)),
  dtm_b = ymd("2012-01-02") + hours(seq(1, 3))
)

fmt_reg <- stamp("2012-03-04 05:06:07", quiet = TRUE)
fmt_iso <- stamp("2012-03-04T05:06:07Z", quiet = TRUE)

txt_reg <-
  df_ref %>%
  mutate(dtm_a = fmt_reg(dtm_a), dtm_b = fmt_reg(dtm_b)) %>%
  format_csv()

txt_iso <-
  df_ref %>%
  mutate(dtm_a = fmt_iso(dtm_a), dtm_b = fmt_iso(dtm_b)) %>%
  format_csv()

txt_reg_iso <-
  df_ref %>%
  mutate(dtm_a = fmt_reg(dtm_a), dtm_b = fmt_iso(dtm_b)) %>%
  format_csv()

# ISO-8601
str_date <- c("2015-01-02", "20150102")
str_delim <- c("T", " ")
str_time <- c(
  "03:04:05.678", "030405.678",
  "03:04:05", "030405",
  "03:04", "0304",
  "03"
)
str_zone <- c("Z", "+0200", "-0200", "+02:00", "-02:00", "+02", "-02")

str_iso_8601 <-
  expand.grid(date = str_date, delim = str_delim, time = str_time, zone = str_zone) %>%
  tbl_df() %>%
  mutate(string = paste0(date, delim, time, zone)) %>%
  `[[`("string")

str_not_iso_8601 <- c(
  "hello",
  "2019-09-27 21:47:00"
)

test_that("ISO-8601 regular expression works", {
  expect_true(all(is_time_8601(str_iso_8601)))
  expect_false(any(is_time_8601(str_not_iso_8601)))
})

test_that("we detect non-iso 8601 in dataframes", {
  expect_true(df_has_time_non_8601(txt_reg, ","))
  expect_false(df_has_time_non_8601(txt_iso, ","))
  expect_true(df_has_time_non_8601(txt_reg_iso, ","))
})
