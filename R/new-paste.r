#' Create a new paste
#'
#' @md
#' @param text of paste
#' @param name name/title of paste
#' @param format hint for syntax highlighting. Defaults to `text`. See
#'               [the detail page](https://pastebin.com/api#5) for more info.
#' @param impersonate if `TRUE` then `PASTEBIN_USER` and `PASTEBIN_PASSWORD` _must_ be set
#'        in order to generate a user key to be applied with the API key. Don't blame me,
#'        blame [pastebin](https://pastebin.com/api#8).
#' @param visibility one of `public`, `unlisted` or `private`. Defaults to `public`
#' @param expires either `n` for never or an abbreviated time expiration string in the form
#'                of a digit (the "number of") and a units character `m` for minute(s),
#'                `d` for day(s), `w` for week(s). Defaults to `n` (never). See
#'                [the detail page](https://pastebin.com/api#6) for more info.
#' @param pastebin_key pastebin API key
#' @note The maximum size a paste can be is 512 kilobytes (0.5 megabytes). Pro members are
#'       allowed to create pastes up to 10 megabytes.
#' @export
new_paste <- function(text, name=NULL, format="text", impersonate=FALSE,
                      visibility=c("public", "unlisted", "private"),
                      expires="n", pastebin_key=pastebin_api_key()) {

  expires <- gsub(" ", "", toupper(expires))

  visibility <- match.arg(visibility, c("public", "unlisted", "private"))
  visibility <- which(visibility == c("public", "unlisted", "private"))

  params <- list(api_dev_key=pastebin_key,
                 api_option="paste",
                 api_paste_code=text,
                 api_paste_name=name,
                 api_paste_format=format,
                 api_user_key="",
                 api_paste_expire_date=expires,
                 api_paste_private=visibility)

  if (impersonate) {

    httr::POST("https://pastebin.com/api/api_login.php",
               body=list(api_dev_key=pastebin_key,
                         api_user_name=Sys.getenv("PASTEBIN_USER"),
                         api_user_password=Sys.getenv("PASTEBIN_PASSWORD")),
               encode="form") -> u_res

    httr::stop_for_status(u_res)

    params$api_user_key <- httr::content(u_res, as="text", encoding="UTF-8")

  }

  httr::POST("https://pastebin.com/api/api_post.php", body=params, encode="form") -> res

  httr::stop_for_status(res)

  httr::content(res, as="text", encoding="UTF-8")

}


