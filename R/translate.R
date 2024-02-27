#' Translate strings using LibreTranslate selected instance
#'
#' Request as many details as you want (revenue, cast, original language, ...)
#' from media and add them to an existing dataframe.
#'
#' @param q The string you want to translate
#' @param from The origin language of the text. If you leave it empty, it will be set to "auto".
#' @param to The destination language you want the string to be translated to.
#' @export
#' @examples
#' \dontrun{
#' translate(q = "hello, nice to meet you", from = "en", to = "es")
#' }

translate <- function(q, from = "auto", to) {

  if (nzchar(Sys.getenv("lt_inst"))) {

    if (!nzchar(Sys.getenv("api_lt"))) {
      data <- list(
        q = q,
        source = from,
        target = to,
        format = "text",
        api_key = ""
      )
    } else if (nzchar(Sys.getenv("api_lt"))) {
      data <- list(
        q = q,
        source = from,
        target = to,
        format = "text",
        api_key = Sys.getenv("api_lt")
      )
    }
  }

  if (!nzchar(Sys.getenv("lt_inst"))) {
    stop("There is no instance set. Please, run the `set_config()` command to configure one.")
  }

  req <- request(Sys.getenv("lt_inst")) %>%
    req_url_path("translate") %>%
    req_body_json(data, type = "application/json") %>%
    req_perform()

  response <- req %>%
    resp_body_json()

  translation <- response$translatedText

  return(translation)
}


#' Automatically detect the language of a given string
#'
#' You don't know in which language is a string written? Detect it using this function.
#'
#' @param str The string you want to detect the language
#' @export
#' @examples
#' \dontrun{
#' detect_language("what language is this?")
#' }

detect_language <- function(str) {

  if (nzchar(Sys.getenv("lt_inst"))) {
    if (!nzchar(Sys.getenv("api_lt"))) {
      data <- list(
        q = str,
        api_key = ""
      )
    } else if (nzchar(Sys.getenv("api_lt"))) {
      data <- list(
        q = str,
        api_key = Sys.getenv("api_lt")
      )
    }
  } else if (!nzchar(Sys.getenv("lt_inst"))) {
    stop("There is no instance set. Please, run the `set_config()` command to configure one.")
  }

  req <- request(Sys.getenv("lt_inst")) %>%
    req_url_path_append("detect") %>%
    req_body_json(data, type = "application/json") %>%
    req_perform()

  response <- req %>%
    resp_body_json()

  confidence <- response[[1]]$confidence
  language <- response[[1]]$language

  return(paste0("The detected language is ", language, ", with a level of confidence of ", confidence, "."))
}

#' Get a list of supported languages for your working instance
#'
#' Run this function and you will be able to know the supported languages for this
#' instance and also their ISO-639 code, needed to set the from and to languages
#' when using `translate()` function.
#'
#' @export
#' @examples
#' \dontrun{
#' get_languages()
#' }

get_languages <- function() {

  if (nzchar(Sys.getenv("lt_inst"))) {
    if (!nzchar(Sys.getenv("api_lt"))) {
      req <- request(Sys.getenv("lt_inst")) %>%
        req_url_path_append("languages") %>%
        req_perform()

      response <- req %>%
        resp_body_json()

      languages <- lapply(response, function(element) {
        list(code = element$code, name = element$name)
      })

      lang <- toJSON(languages, auto_unbox = TRUE)

      return(cat(paste0("These are the available languages in your configured instance: ", "\n",
                        toJSON(languages, pretty = TRUE), collapse = "\n")))

    } else if (nzchar(Sys.getenv("api_lt"))) {
      req <- request(Sys.getenv("lt_inst")) %>%
        req_url_path_append("languages") %>%
        req_url_query(api_key = Sys.getenv("api_lt")) %>%
        req_perform()

      response <- req %>%
        resp_body_json()

      languages <- lapply(response, function(element) {
        list(code = element$code, name = element$name)
      })

      lang <- toJSON(languages, auto_unbox = TRUE)

      return(cat(paste0("These are the available languages in your configured instance: ", "\n",
                        toJSON(languages, pretty = TRUE), collapse = "\n")))
    }
  } else if (!nzchar(Sys.getenv("lt_inst"))) {
    stop("There is no instance set. Please, run the `set_config()` command to configure one.")
  }
}
