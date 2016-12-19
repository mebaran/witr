#' @import httr
#' @import dplyr
#' @include entities.R

NULL

#' Setup credential config for API
#'
#' @export
wit.token <- function(token=NULL) {
  if(is.null(token)) {
    token <- getOption('wit.api.token', Sys.getenv('WIT_API_TOKEN'))
  }
  add_headers("Authorization"=sprintf("Bearer %s", token))
}

#' Wrap verb for wit request
#'
#' @export
wit.request <- function(verb, url, ...) {
  VERB(verb, url, wit.token(), ...)
}

#' Analyze a single message
#'
#' @export
analyze.message <- function(q, msgid=NULL, threadid=NULL) {
  params <- list(q=q, msgid=msgid, threadid=threadid)
  wit.request("GET", 'https://api.wit.ai/message', query=params) %>%
    content() %>% {
      datetimes <- .$entities$datetime
      .$entities$intent <- bind_rows(.$entities$intent)
      .$entities$datetime <- bind_rows(Map(wit.date, datetimes))
      .
    }
}

#' Start conversation
#'
#' @export
start.converse <- function(q, context, sessionid) {
}

#' Continue conversation until stop is hit
#'
#' @export
continue.converse <- function(convo) {

}
