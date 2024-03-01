# libretranslateR

<!-- badges: start -->

[![CodeFactor](https://www.codefactor.io/repository/github/myanesp/libretranslateR/badge)](https://www.codefactor.io/repository/github/myanesp/libretranslateR) [![](https://img.shields.io/github/languages/code-size/myanesp/libretranslateR.svg)](https://github.com/myanesp/libretranslateR) [![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://badgen.net/badge/icon/github?icon=github&label)](https://github.com/myanesp/libretranslateR)
![](https://badgen.net/github/stars/myanesp/libretranslateR?icon=github&label=stars)
![Github last-commit](https://img.shields.io/github/last-commit/myanesp/libretranslateR)
![Github license](https://badgen.net/github/license/myanesp/libretranslateR)
<!-- badges: end -->

## Connect to LibreTranslate API from R

[API Docs](https://libretranslate.com/docs) | [Official instance](https://libretranslate.com) | [List of instances](https://github.com/LibreTranslate/LibreTranslate?tab=readme-ov-file#mirrors)

### Features
- Translate texts, strings of R objects without leaving R using an opensource engine.
- Automatically detect languages
- It can connect to any LibreTranslate instance, so you're not tied to anything, even you can host your own!
- As you can install your LibreTranslate instance, offline translation is available

### Examples and usage
```
set_config() # Config wizard for setting the instance you're going to use and your API key

translate(q = "hola, amigos", to = "en") # Autodetect language

translate(q = "hola", from = "es", to = "fr")

get_languages() # Display available languages

detect_language("do you know what i'm talking about?") # Detect language and the level of confidence
```

### Install your own instance

You can follow the [official instructions located at LibreTranslate's repository](https://github.com/LibreTranslate/LibreTranslate?tab=readme-ov-file#install-and-run) to host your own instance. You can do it through the Python's package or through a docker container.

### Planned features

-   [ ] Support for translating files
-   [ ] Transform ISO names in `get_languages()` to real user-friendly names
-   [ ] Automatically detect if the chosen instance needs API or not
-   [ ] Rework and rethink config process
