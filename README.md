# Creatinine Time Series

Creatinine Time Series Analysis

## Git Development

1. New branch:

    ```bash
    git checkout -b dev/feature-name
    ```

2. Run existing code: 

    ```r
    source("index.R")
    ```

3. New script: 

    ```bash
    R/00X-feature-name
    ```

4. Development in new script

    ```r
    data %>%
        mutate(...)
    ```

5. Functionalise code in script

    ```r
    feature <- function(data, args, kargs) {
        data %>%
            mutate(...)
        return(data)
    }
    ```

6. Call function in main script and check output

    ```r
    # clear data
    source("index.R")
    ```

7. Call function in Rmd