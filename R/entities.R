#' @import lubridate
#' @import dplyr

NULL

wit.date <- function(dateval) {
  val <- switch(dateval$type,
                interval=list(start = ymd_hms(dateval$from$value),
                              end = ymd_hms(dateval$to$value),
                              start.grain = dateval$from$grain,
                              end.grain = dateval$to$grain,
                              type = "interval"),
                value=list(start = ymd_hms(dateval$value),
                           end = ymd_hms(dateval$value),
                           start.grain = dateval$grain,
                           end.grain = dateval$grain,
                           type = "value"))
  val$confidence=dateval$confidence
  val
}
