# BIO503

## *Plant ecology in a global change perspective*

### What you will learn

This is a brief and condensed guide to help you grasp the **fundamentals** of the **R language** and **tidyverse**.

## Introduction to R and RStudio:

-   Navigating RStudio.
-   Understanding the basic R syntax.
-   Tidyverse Fundamentals:
    -   Core tidyverse packages: dplyr, ggplot2, tidyr.
    -   Loading data into R. Transforming and cleaning data using dplyr.
    -   Data Visualization with ggplot2
-   Basics of linear models

### For the R introductory session:

-   Download and install **both** R ([download R](https://cran.rstudio.com)) **and** RStudio ([download RStudio](https://rstudio.com/products/rstudio/download/#download)) if you have not yet.

-   Run the following commands on the Rstudio console:

```         
# Check if tidyverse package is installed, if not, install it
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

# Check if vegan package is installed, if not, install it
if (!requireNamespace("vegan", quietly = TRUE)) {
  install.packages("vegan")
}
```
- Presentation [here](https://github.com/Lacapary/BIO503/blob/main/PDFs/Intro_R.pdf)

## Mixed effects models

-   What are hierarchical linear models
-   Identify situations in which the use of mixed effects is appropriate
-   Implement basic linear mixed models (LMM) with `R`

Required packages
```
install.packages(c("lme4",
                      "ggeffects",
                      "stargazer"
                      ))
```
- Presentation [here](https://github.com/Lacapary/BIO503/blob/main/PDFs/Hierarchical_Linear_Models.pdf)

## Multivariate analysis

- Learn the basics of multivariate analysis to reveal patterns
- Use `R` to perform Unconstrained ordinations
```
install.packages(c("vegan",
                   "ape",
                   "factoextra",
                   "dendextend"))
```
- Presentation [here](https://github.com/Lacapary/BIO503/blob/main/PDFs/Multivariate.pdf)


Happy coding and welcome to the world of R!
