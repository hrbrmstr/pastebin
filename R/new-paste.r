#' Create a new paste
#'
#' @md
#' @param text of paste
#' @param name name/title of paste
#' @param format hint for syntax highlighting. Defaults to `text`. See
#'               [the detail page](http://pastebin.com/api#5) for more info.
#' @param visibility one of `public`, `unlisted` or `private`. Defaults to `public`
#' @param expires either `n` for never or an abbreviated time expiration string in the form
#'                of a digit (the "number of") and a units character `m` for minute(s),
#'                `d` for day(s), `w` for week(s). Defaults to `n` (never). See
#'                [the detail page](http://pastebin.com/api#6) for more info.
#' @param pastebin_api_key pastebin API key
#' @note The maximum size a paste can be is 512 kilobytes (0.5 megabytes). Pro members are
#'       allowed to create pastes up to 10 megabytes.
#' @export
new_paste <- function(x, name=NULL, format="text", visibility=c("public", "unlisted", "private"),
                      expires="n", pastebin_api_key=pastebin_api_key()) {

  expires <- gsub(" ", "", toupper(expires))
  visibility <- match.arg(visibility, c("public", "unlisted", "private"))
  visibilty  <- which(visibility == c("public", "unlisted", "private"))

  httr::POST("http://pastebin.com/api/api_post.php",
             body=list(api_dev_key=pastebin_api_key,
                       api_paste_format=format,
                       api_paste_expire_date=exipres,
                       api_paste_private=visibility,
                       api_option="paste",
                       api_paste_code=x)) -> res

  httr::stop_for_status(res)

  jsonlite::fromJSON(httr::content(res, as="text", encoding="UTF-8"))

}


