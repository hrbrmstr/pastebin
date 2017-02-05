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
#' @export
new_paste <- function(x, name=NULL, format="text", visibility=c("public", "unlisted", "private"),
                      expires="N", pastebin_api_key=pastebin_api_key()) {

  httr::POST("http://pastebin.com/api/api_post.php",
             body=list(api_dev_key=pastebin_api_key,
                       api_option="paste",
                       api_paste_code=x))

}


