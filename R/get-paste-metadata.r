#' Get paste metadata
#'
#' @md
#' @param x paste id
#' @param use_scraping_api if a pro member, set this to `TRUE`, otherwise leave it `FALSE`
#'        and be kind to their servers lest ye be banned.
#' @param include_metadata if `use_scraping_api` is `TRUE` and this is `TRUE`, the returned
#'        `list` will include metadata
#' @references [Scraping API](http://pastebin.com/api_scraping_faq)
#' @export
get_paste_metadata <- function(x) {

  res <- httr::GET("http://pastebin.com/api_scrape_item_meta.php", query=list(i=x))

  httr::stop_for_status(res)

  out <- jsonlite::fromJSON(httr::content(res, as="text", encoding="UTF-8"))

  out$date <- as.POSIXct(as.numeric(out$date), origin="1970-01-01")
  out$size <- as.numeric(out$size)
  out$hits <- as.numeric(out$hits)
  out$expire <- as.numeric(out$expire)
  out$expire <- as.POSIXct(ifelse(out$expire==0, NA, out$expire), origin="1970-01-01")

  out

}
