Statistical assignment 3
================
Ben Machon 680011539
17/02/2020

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
library(data.table)

# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths

files <- dir(
             
             "/Users/benmachon/Downloads/UKDA-6614-tab/tab/",
             
             pattern = "indresp",
             recursive = TRUE,
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "/Users/benmachon/Downloads/UKDA-6614-tab/tab//ukhls_w1/a_indresp.tab"
    ## [2] "/Users/benmachon/Downloads/UKDA-6614-tab/tab//ukhls_w2/b_indresp.tab"
    ## [3] "/Users/benmachon/Downloads/UKDA-6614-tab/tab//ukhls_w3/c_indresp.tab"
    ## [4] "/Users/benmachon/Downloads/UKDA-6614-tab/tab//ukhls_w4/d_indresp.tab"
    ## [5] "/Users/benmachon/Downloads/UKDA-6614-tab/tab//ukhls_w5/e_indresp.tab"
    ## [6] "/Users/benmachon/Downloads/UKDA-6614-tab/tab//ukhls_w6/f_indresp.tab"
    ## [7] "/Users/benmachon/Downloads/UKDA-6614-tab/tab//ukhls_w7/g_indresp.tab"
    ## [8] "/Users/benmachon/Downloads/UKDA-6614-tab/tab//ukhls_w8/h_indresp.tab"
    ## [9] "/Users/benmachon/Downloads/UKDA-6614-tab/tab//ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
Long <- all7 %>%

pivot_longer(a_memorig:g_vote6, names_to = "variable", values_to = "value") %>%
  
      separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  
      pivot_wider(names_from = variable, values_from = value) %>%

view(Long)
```

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

``` r
Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex_dv = case_when(sex_dv == 1 ~ "Male", sex_dv == 2 ~ "Female")) %>%
        mutate(vote6 = recode(vote6, "1" = 1, "2" = 2, "3" = 3, "4" = 4, .default = NA_real_)) 
         
table(Long$vote6) 
```

    ## 
    ##     1     2     3     4 
    ## 21660 70952 56134 52145

``` r
table(Long$sex_dv) 
```

    ## 
    ## Female   Male 
    ## 117665 100342

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
meanVote6 <- Long %>%
group_by(wave, sex_dv) %>%
summarise(meanVote6 = mean(vote6, na.rm = TRUE))%>%
na.omit()
        
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   wave [7]
    ##    wave  sex_dv meanVote6
    ##    <chr> <chr>      <dbl>
    ##  1 a     Female      2.84
    ##  2 a     Male        2.53
    ##  3 b     Female      2.82
    ##  4 b     Male        2.51
    ##  5 c     Female      2.87
    ##  6 c     Male        2.54
    ##  7 d     Female      2.89
    ##  8 d     Male        2.55
    ##  9 e     Female      2.87
    ## 10 e     Male        2.51
    ## 11 f     Female      2.81
    ## 12 f     Male        2.47
    ## 13 g     Female      2.73
    ## 14 g     Male        2.42

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
meanVote6 %>%
pivot_wider(names_from = wave, values_from = meanVote6)
```

    ## # A tibble: 2 x 8
    ##   sex_dv     a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 Male    2.53  2.51  2.54  2.55  2.51  2.47  2.42

The vote6 variable runs from value 1 (very interested in politics) to 4
(not interested in politics). In this study, females have shown a slight
increase in political interest (0.111037) from wave 1 to wave 7. Males
have also shown an increase in political of around 0.11 (0.111114).
therefore we can infer that the subjects of this study have shown an
overall increase in political interest at a similar average rate. The
values for females begin at 2.839437 and finish at 2.728400 where as the
male figure begins at 2.527112 and finishes at 2.415998. Therefore, it
can be stated that from wave 1 to wave 7, the males within the study
have shown a greater interest in politics than the females.

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.
5.  Write a short interpretation of your findings.

<!-- end list -->

``` r
#1
stability <- Long %>%
  na.omit(vote6)

#2
stability <- stability %>%
  group_by(pidp) %>%
mutate(delta = order_by(wave, abs(vote6 - lag(vote6))))%>%
  na.omit(delta)

#3 
 mean(stability$delta)
```

    ## [1] 0.440193

``` r
 # I struggled to find a solution to part 3 and therefore was also unable to answer part 4
```
