#' Get recent pastes
#'
#' @param limit number of recent pastes to fetch. Limit is 500, default is 50.
#' @param lang limit the recent paste list to a particular language. Default is all pastes
#' @references [Scraping API](http://pastebin.com/api_scraping_faq)
#' @export
get_recent_pastes <- function(limit=50, lang=NULL) {


  if (limit<1) limit <- 50
  if (limit>500) limit <- 500

  params <- list(limit=limit)
  if (!is.null(lang)) params$lang <- lang

  res <- httr::GET("http://pastebin.com/api_scraping.php",
                   query=params)
  httr::stop_for_status(res)

  httr::content(res, as="text", encoding="UTF-8") %>%
    jsonlite::fromJSON() %>%
    as_tibble() -> out

  out$date <- as.POSIXct(as.numeric(out$date), origin="1970-01-01")
  out$size <- as.numeric(out$size)
  out$expire <- as.numeric(out$expire)
  out$expire <- as.POSIXct(ifelse(out$expire==0, NA, out$expire), origin="1970-01-01")

  out

}

