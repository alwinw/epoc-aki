# Creatinine Time Series

Creatinine Time Series Analysis

Alwin Wang

## Running

Instructions to come

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
    source("index.R")
    ```

3. New script for development work

    ```bash
    R/00X-feature-name
    ```

4. Development in new script

    ```r
    data %>%
        mutate(...)
    ```

5. Commit and push changes -- potentially numerous when working across multiple devices

    ```bash
    git fetch
    git status
    git add .
    git commit -m "Summary here
    Additional details here"
    git push
    ```

6. Functionalise code in script

    ```r
    feature <- function(data, args, kwargs) {
        data %>%
            mutate(...)
        return(data)
    }
    ```

7. Call function in main script and Rmarkdown

    ```r
    featured_data <- feature(data, args, kwargs)
    dplyr::glimpse(featured_data, width = 20)
    ```

8. Check output of main script

    ```r
    rm(list = ls(all. names = TRUE))
    source("index.R")
    ```

9. Check output of bookdown via Addins or

    ```r
    bookdown::serve_book()
    ```

10. Pull request on GitHub (preferred)

    * Create pull
    * Merge pull
    * Delete branch

    or merge via command line (not preferred)

    ```bash
    git checkout master
    git merge dev/feature
    git push
    ```

## Outstanding Items

* Pretty print for structure of `xlsx_data`
* Flowchart of screening log as per previous paper
