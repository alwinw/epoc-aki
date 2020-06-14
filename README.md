# The EPOC-AKI Study

Evaluation of the Predictive value of short-term Oliguria and minor Creatinine increases for Acute Kidney Injury in ICU

Alwin Wang and Lisa Toh

## Running

1. Clone the repository

    ```bash
    git clone git@github.com:AlwinW/epoc-api.git
    ```

2. Open the R project file `epoc-aki.Rproj` with RStudio

3. Open the relevant `bookdown` file

4. In RStudio run `Addins` > `Preview Book`. Or from the console run

    ```R
    bookdown::serve_book()
    ```

5. Preview the bookdown at `http://localhost:4321/` or `http://127.0.0.1:4321/`

## Git Version Control and Development Flow

1. New branch for a new feature

    ```bash
    git checkout master
    git fetch
    git pull
    git checkout -b dev/feature-name
    git push --set-upstream origin dev/feature-name
    ```

2. Run the existing code to build RData

    ```r
    rm(list = ls(all. names = TRUE))
    source("main.R")
    ```

3. New script for development work

    ```bash
    R/XX_feature_name.R
    ```

4. Development in new script

    ```r
    # ---- helper_function ----
    helper <- function(...)

    # ---- sub_calculation ----
    data %>%
        mutate(...)
    ```

5. Commit and push changes -- potentially numerous when working across multiple devices

    ```bash
    git fetch
    git status
    git add --all
    git commit -m "Summary here
    Additional details here"
    git push
    ```

6. Read new script as chunks in Rmarkdown

    ````r
    # New Feature

    ```{r XX_feature_name, include=FALSE, cache=FALSE}
    knitr::read_chunk("R/XX_feature_name.R")
    ```

    ## Sub feature

    ```{r, helper_function, echo=FALSE}
    ```

    ```{r, sub_calculation}
    ```
    ````

7. Check output of main script and bookdown

    ```R
    source("main.R")
    bookdown::serve_book()
    # Check output
    servr::daemon_stop()
    ```

8. Merge changes via pull request on GitHub (preferred) or command line (not preferred)

    * Create pull on Github
    * Merge pull on Github
    * Delete branch on Github
