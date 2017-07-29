#' Get trending pastes
#'
#' @md
#' @param pastebin_key pastebin API key
#' @references [https://pastebin.com/api#10](https://pastebin.com/api#10)
#' @export
get_trending_pastes <- function(pastebin_key=pastebin_api_key()) {

  res <- httr::POST("https://pastebin.com/api/api_post.php",
                    body=list(api_dev_key=pastebin_key,
                              api_option="trends"),
                    encode="form")

  httr::stop_for_status(res)
  httr::content(res, as="text", encoding="UTF-8") -> out

  read_html(out) %>%
    xml_find_all(".//paste") %>%
    map(xml_children) %>%
    map(~map(.x, ~setNames(list(xml_text(.)), xml_name(.)))) %>%
    map_df(flatten_df) %>%
    mutate(paste_date=as.POSIXct(as.numeric(paste_date), origin="1970-01-01"),
           paste_size=as.numeric(paste_size),
           paste_hits=as.numeric(paste_hits),
           paste_expire_date=as.numeric(paste_expire_date),
           paste_expire_date=as.POSIXct(ifelse(paste_expire_date==0, NA, paste_expire_date),
                                        origin="1970-01-01"),
           paste_private=as.logical(as.numeric(paste_private))) %>%
    setNames(gsub("^paste_", "", colnames(.)))

}
