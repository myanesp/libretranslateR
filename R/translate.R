#' Translate strings using LibreTranslate selected instance
#'
#' Use this function to translate strings, from regular text
#' to entire dataframes.
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

  req <- httr2::request(Sys.getenv("lt_inst")) |>
    httr2::req_url_path("translate") |>
    httr2::req_body_json(data, type = "application/json") |>
    httr2::req_perform()

  response <- req |>
    httr2::resp_body_json()

  translation <- response$translatedText

  return(translation)
}

#' Translate files using LibreTranslate selected instance
#'
#' With this function, you can translate an external file.
#' Supported formats are .txt, .odt, .odp, .docx, .pptx, .epub and .html
#'
#' @param file The path to the file you want to translate (absolute or relative).
#' @param from The origin language of the text. If you leave it empty, it will be set to "auto".
#' @param to The destination language you want the text to be translated to.
#' @export
#' @examples
#' \dontrun{
#' translate_file <- function(file, from = "auto", to)
#' }

translate_file <- function(file, from = "auto", to) {

  extension <- "\\.(txt|odt|odp|docx|pptx|epub|html)$"

  translated_file <- sub("(\\.[a-zA-Z0-9]+)$", paste0("-", to, "\\1"), file)

  if (grepl(extension, file, ignore.case = TRUE)) {
  } else {
    stop("The file does not have a valid extension. Only .txt, .odt, .odp,
    .docx, .pptx, .epub, .html are allowed")
  }

  if (nzchar(Sys.getenv("lt_inst"))) {

    if (!nzchar(Sys.getenv("api_lt"))) {

      req <- httr2::request(Sys.getenv("lt_inst")) |>
        httr2::req_url_path("translate_file") |>
        httr2::req_body_multipart(file = curl::form_file(file), source = "es", target = "en", api_key = "") |>
        httr2::req_perform()

      response <- req |>
        httr2::resp_body_json()

      utils::download.file(response$translatedFileUrl, destfile = translated_file)

      message(paste0("File download correctly in ", translated_file))

    } else if (nzchar(Sys.getenv("api_lt"))) {

      req <- httr2::request(Sys.getenv("lt_inst")) |>
        httr2::req_url_path("translate_file") |>
        httr2::req_body_multipart(file = curl::form_file(file), source = "es", target = "en", api_key = Sys.getenv("api_lt")) |>
        httr2::req_perform()

      response <- req |>
        httr2::resp_body_json()

      utils::download.file(response$translatedFileUrl, destfile = translated_file)

      message(paste0("File download correctly in ", translated_file))

    }
  }

  if (!nzchar(Sys.getenv("lt_inst"))) {
    stop("There is no instance set. Please, run the `set_config()` command to configure one.")
  }
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

  req <- httr2::request(Sys.getenv("lt_inst")) |>
    httr2::req_url_path_append("detect") |>
    httr2::req_body_json(data, type = "application/json") |>
    httr2::req_perform()

  response <- req |>
    httr2::resp_body_json()

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
      req <- httr2::request(Sys.getenv("lt_inst")) |>
        httr2::req_url_path_append("languages") |>
        httr2::req_perform()

      response <- req |>
        httr2::resp_body_json()

      languages <- lapply(response, function(element) {
        list(code = element$code, name = element$name)
      })

      lang <- rjson::toJSON(languages, auto_unbox = TRUE)

      return(cat(paste0("These are the available languages in your configured instance: ", "\n",
                        rjson::toJSON(languages, pretty = TRUE), collapse = "\n")))

    } else if (nzchar(Sys.getenv("api_lt"))) {
      req <- httr2::request(Sys.getenv("lt_inst")) |>
        httr2::req_url_path_append("languages") |>
        httr2::req_url_query(api_key = Sys.getenv("api_lt")) |>
        httr2::req_perform()

      response <- req |>
        httr2::resp_body_json()

      languages <- lapply(response, function(element) {
        list(code = element$code, name = element$name)
      })

      lang <- rjson::toJSON(languages, auto_unbox = TRUE)

      return(cat(paste0("These are the available languages in your configured instance: ", "\n",
                        rjson::toJSON(languages, pretty = TRUE), collapse = "\n")))
    }
  } else if (!nzchar(Sys.getenv("lt_inst"))) {
    stop("There is no instance set. Please, run the `set_config()` command to configure one.")
  }
}
