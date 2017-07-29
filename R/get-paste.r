#' Get raw paste data
#'
#' @md
#' @param x paste id
#' @param use_scraping_api if a pro member, set this to `TRUE`, otherwise leave it `FALSE`
#'        and be kind to their servers lest ye be banned.
#' @param include_metadata if `use_scraping_api` is `TRUE` and this is `TRUE`, the returned
#'        `list` will include metadata
#' @return a `list` with the paste text or the paste text plus metadata. A `list` is returned
#'         to make it easier to deal with the results programmatically. Returning a `list`
#'         in one call context and a `character` vector in another may be OK interactively
#'         bit it creates a situation where you need to write `if` logic to handle
#'         programmatically. Use [toString] to extract just the paste body
#' @references [Scraping API](https://pastebin.com/api_scraping_faq)
#' @export
get_paste <- function(x, use_scraping_api=FALSE, include_metadata=FALSE) {

  meta <- NULL

  if (!use_scraping_api) {

    res <- httr::GET(sprintf("https://pastebin.com/raw/%s", x))
    httr::stop_for_status(res)
    paste_text <- httr::content(res, as="text", encoding="UTF-8")

  } else {

    res <- httr::GET("https://pastebin.com/api_scrape_item.php",
                     query=list(i=x))
    httr::stop_for_status(res)
    paste_text <- httr::content(res, as="text", encoding="UTF-8")

    if (include_metadata) meta <- get_paste_metadata(x)

  }

  ret <- list(text=paste_text)
  if (!is.null(meta)) ret$meta <- meta

  class(ret) <- c("paste", "list")

  ret

}

#' Extract just the paste text from a paste object
#'
#' @param x paste object
#' @param ... unused
#' @export
toString.paste <- function(x, ...) { x$text }


#' Extract just the paste text from a paste object
#'
#' @param x paste object
#' @param ... unused
#' @export
as.character.paste <- function(x, ...) { x$text }
