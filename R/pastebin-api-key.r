#' Get or set PASTEBIN_API_KEY value
#'
#' The API wrapper functions in this package all rely on a pastebin API
#' key residing in the environment variable \code{PASTEBIN_API_KEY}. The
#' easiest way to accomplish this is to set it in the `\code{.Renviron}` file in your
#' home directory.
#'
#' @param force force setting a new pastebin API key for the current environment?
#' @return atomic character vector containing the pastebin API key
#' @note an pastebin API key is only necessary for "poster" access
#' @export
pastebin_api_key <- function(force = FALSE) {

  env <- Sys.getenv('PASTEBIN_API_KEY')
  if (!identical(env, "") && !force) return(env)

  if (!interactive()) {
    stop("Please set env var PASTEBIN_API_KEY to your pastebin API key",
      call. = FALSE)
  }

  message("Couldn't find env var PASTEBIN_API_KEY See ?pastebin_api_key for more details.")
  message("Please enter your API key and press enter:")
  pat <- readline(": ")

  if (identical(pat, "")) {
    stop("pastebin API key entry failed", call. = FALSE)
  }

  message("Updating PASTEBIN_API_KEY env var")
  Sys.setenv(PASTEBIN_API_KEY = pat)

  pat

}
