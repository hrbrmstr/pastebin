#' Get paste metadata
#'
#' @md
#' @param paste_id paste id
#' @references [Scraping API](https://pastebin.com/api_scraping_faq)
#' @export
get_paste_metadata <- function(paste_id) {

  res <- httr::GET("https://pastebin.com/api_scrape_item_meta.php",
                   query=list(i=paste_id))

  httr::stop_for_status(res)

  out <- jsonlite::fromJSON(httr::content(res, as="text", encoding="UTF-8"))

  out$date <- as.POSIXct(as.numeric(out$date), origin="1970-01-01")
  out$size <- as.numeric(out$size)
  out$hits <- as.numeric(out$hits)
  out$expire <- as.numeric(out$expire)
  out$expire <- as.POSIXct(ifelse(out$expire==0, NA, out$expire), origin="1970-01-01")

  out

}
