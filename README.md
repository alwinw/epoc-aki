# The EPOC-AKI Study <img src='doc/images/logo.png' align="right" height="138.5" />

Evaluation of the Predictive value of short-term Oliguria and minor Creatinine increases for Acute Kidney Injury in ICU

## Data Overview

![Data Overview](/doc/images/overview.png)

## Running

1. Clone the repository and open the R project file `epoc-aki.Rproj` with RStudio

    ```bash
    git clone git@github.com:AlwinW/epoc-api.git
    ```

2. Preview the relevant `bookdown` file

   - Option 1: Open the file directly in RStudio

      1. Open up the bookdown index, e.g. `doc/full/index.Rmd`
      2. In RStudio run `Addins` > `Preview Book`, or from the console run

        ```R
        bookdown::serve_book()
        ```

   - Option 2: Run `serve_book` from the console

        ```R
        bookdown::serve_book(dir = "doc/full/")
        ```

3. Preview the bookdown site at `http://localhost:4321/` or `http://127.0.0.1:4321/`

## Contributors

Alwin Wang and Lisa Toh

## Acknowledgements

## Activity

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/alwinw/epoc-aki?style=flat-square)
![GitHub last commit](https://img.shields.io/github/last-commit/alwinw/epoc-aki?style=flat-square)
![Visits](https://badges.pufler.dev/visits/alwinw/epoc-aki?style=flat-square&label=visits)
![Hits](https://img.shields.io/jsdelivr/gh/hm/alwinw/epoc-aki?label=hits&style=flat-square)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/alwinw/epoc-aki?style=flat-square)

## License

[![GitHub](https://img.shields.io/github/license/alwinw/epoc-aki?style=flat-square)](LICENSE)

- [GPL-3.0 License](https://opensource.org/licenses/GPL-3.0)
- Copyright 2020 © [Alwin Wang](https://github.com/alwinw) and [Lisa Toh](https://github.com/lymt)
