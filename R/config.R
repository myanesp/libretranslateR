#' Set the LibreTranslate instance you want to use and the API key if needed
#'
#' This configuration wizard will help you to configure the LibreTranslate instance
#' you want to use. It also support storing the API key and exporting the configuration
#' in a .json file that can be read later with `import_config()` to avoid running this
#' command every time you initiate R.
#'
#' @export
#' @examples
#' \dontrun{
#' set_config()
#' }

set_config <- function() {

  message("Welcome to the configuration wizard for the LibreTranslate R package.")

  message("Let's start by selecting an instance.")

  select_instance <- utils::menu(
    choices = c(
      "Official instance (libretranslate.com), paid and needed API",
      "I want to specify a custom one"
    )
  )

  if (select_instance == 1) {

    Sys.setenv("lt_inst" = "https://libretranslate.com")

    message("This instance requires to have a valid API to use it.")

    message("If you don't have one, request it on the website (libretranslate.com)")

    api_key <- readline("Please, paste here your API key here, without spaces: ")

    regex_api <- "^[a-zA-Z0-9]{8}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{12}$"

    if (!grepl(regex_api, api_key)) {
      stop("The API key does not contain the valid format.")
    }

    data <- list(
      q = "Hello",
      source = "en",
      target = "es",
      format = "text",
      api_key = api_key
    )

    req <- httr2::request(Sys.getenv("lt_inst")) |>
      httr2::req_url_path_append("translate") |>
      httr2::req_body_json(data, type = "application/json") |>
      httr2::req_perform()

    if (req$status_code == 200) {
      Sys.setenv(lt_api = api_key)
      message("Successfully configured LibreTranslate instance. Let's translate!")

      message("Would you like to export your credentials (instance and API key) to a
              json file that can be exported in a new session with `import_config()` without
              having to run this config wizard again?")

      json_utils <- utils::menu(
        choices = c(
          "Yes! Of course",
          "No, thank you :)"
        )
      )

      if (json_utils == 1) {
        config_list <- list(
          instance = Sys.getenv("lt_inst"),
          api_key = Sys.getenv("lt_api")
        )

        config_json <- rjson::toJSON(config_list)

        file_name <- readline("Please, provide a filename for your configuration: ")

        if (!grepl("\\.json$", file_name)) {
          write(config_json, paste0(file_name, ".json"))
        } else if (grepl("\\.json$", file_name)) {
          write(config_json, file_name)
        }

      } else {
        message("All set. Let's translate!")
      }

    } else {
      stop("There is an error while setting the instance. Please, check the URL and the parameters.")
    }
  } else if (select_instance == 2) {

    custom_url <- readline("Please, type the URL, including the protocol (http/https): ")

    if (is.null(custom_url)) {
      stop("You must provide a LibreTranslate instance URL.")
    }

    if (!grepl("^https?://", custom_url)) {

      message("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
      custom_url <- readline("Please, type it again: ")

      if (!grepl("^https?://", custom_url)) {
        stop("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
      }
    }

    Sys.setenv("lt_inst" = custom_url)
  }

  message("Does this instance require an API key in order to use the API?")
  api_needed <- utils::menu(
    choices = c(
      "Yes",
      "No",
      "I don't know what the hell you're talking about"
    )
  )

  if (api_needed == 1)  {

    api_key <- readline("Please, paste here your API key here, without spaces: ")

    regex_api <- "^[a-zA-Z0-9]{8}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{12}$"

    if (!grepl(regex_api, api_key)) {
      stop("The API key does not contain the valid format.")
    }

    data <- list(
      q = "Hello",
      source = "en",
      target = "es",
      format = "text",
      api_key = api_key
    )

    req <- httr2::request(Sys.getenv("lt_inst")) |>
      httr2::req_url_path_append("translate") |>
      httr2::req_body_json(data, type = "application/json") |>
      httr2::req_perform()

    if (req$status_code == 200) {
      Sys.setenv(lt_api = api_key)
      message("Successfully configured instance. Let's translate!")

      message("Would you like to export your credentials (instance and API key) to a
              json file that can be exported in a new session with `import_config()` without
              having to run this config wizard again?")

      json_utils <- utils::menu(
        choices = c(
          "Yes! Of course",
          "No, thank you :)"
        )
      )

      if (json_utils == 1) {
        config_list <- list(
          instance = Sys.getenv("lt_inst"),
          api_key = Sys.getenv("lt_api")
        )

        config_json <- rjson::toJSON(config_list)

        file_name <- readline("Please, provide a filename for your configuration: ")

        if (!grepl("\\.json$", file_name)) {
          write(config_json, paste0(file_name, ".json"))
        } else if (grepl("\\.json$", file_name)) {
          write(config_json, file_name)
        }

      } else {
        message("All set. Let's translate!")
      }
    } else {
      stop("There is an error while setting your instance. Please, check the URL and the parameters.")
    }

  } else if (api_needed == 2) {

    message("Would you like to export your credentials (instance and API key) to a
              json file that can be exported in a new session with `import_config()` without
              having to run this config wizard again?")

    json_utils <- utils::menu(
      choices = c(
        "Yes! Of course",
        "No, thank you :)"
      )
    )

    if (json_utils == 1) {
      config_list <- list(
        instance = Sys.getenv("lt_inst"),
        api_key = Sys.getenv("lt_api")
      )

      config_json <- rjson::toJSON(config_list)

      file_name <- readline("Please, provide a filename for your configuration: ")

      if (!grepl("\\.json$", file_name)) {
        write(config_json, paste0(file_name, ".json"))
      } else if (grepl("\\.json$", file_name)) {
        write(config_json, file_name)
      }

    } else {
      message("All set. Let's translate!")
    }

    message("Fantastic, the instance has been successfully configured. Let's translate!")

  } else if (api_needed == 3) {

    message("If you don't know if you need an API key for this instance, go to the website of the instance
            and check, on the top bar, at the right side, if there is a button to 'Obtain API key'.
            If it is there, you can request access. Then, once you have it, come back here and execute
            the set_config() function again. If there is no button to obtain the API key, that means that
            is not needed to work with that instance, so execute the set_config() function again to setup.")

  }

}



#' Import configuration for an instance for a file
#'
#' Read the file that contains your instance and the API key for it (if needed),
#' instead of having to execute the `set_config()` wizard again.
#'
#' @param file .json file generated by `set_config()` with the needed information
#' @export
#' @examples
#' \dontrun{
#' import_config("my-custom-instance.json")
#' }

import_config <- function(file) {

  if (!grepl("\\.json$", file)) {
    stop("File must be a .json")
  }

  instance_config <- rjson::fromJSON(file)

  if (!is.null(instance_config[["instance"]])) {
    inst <- instance_config$instance
  } else {
    stop("The provided .json file does not have any instance. Check it and try again.")
  }

  if (!is.null(instance_config[["api_key"]])) {
    api_key <- instance_config$api_key

    regex_api <- "^[a-zA-Z0-9]{8}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{12}$"

    if (!grepl(regex_api, api_key)) {
      stop("The API key does not contain the valid format.")
    }

    data <- list(
      q = "hola",
      source = "es",
      target = "en",
      format = "text",
      api_key = api_key
    )} else if (is.null(instance_config[["api_key"]])) {
      data <- list(
        q = "hola",
        source = "es",
        target = "en",
        format = "text",
        api_key = ""
      )
    }

  req <- httr2::request(inst) |>
    httr2::req_url_path("translate") |>
    httr2::req_body_json(data, type = "application/json") |>
    httr2::req_perform()

  if (req$status_code == 200) {
    Sys.setenv(lt_inst = inst)
    message("Successfully configured LibreTranslate instance.")
  } else {
    stop(paste0("There is an error ", req$status_code, " with your configuration.
         Please, check that your instance and API key are valid and try again."))
  }
}
