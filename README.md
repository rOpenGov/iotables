
<!-- README.md is generated from README.Rmd. Please edit that file -->
iotables
========

The symmetric input-output tables (SIOTs) are complex statistical products that present inter-related statistics in a predefined structure. They are often found in spreadsheets that follow this structure, or in the case Eurostat in a data repository. In both cases they in reproducible research must be downloaded and restructured to programmatically accessible form. Often these highly structured statistics need to be analyzed together with other data, for example, when employment effects and multipliers are calculated. In this case processing the employment data to SIOT conforming format is a significant preprocessing challenge.

The iotables are exactly designed for these tasks. Currently the package downloads and processes standardized European SIOTs conforming to the latest statistical regulations, i.e. SIOTs starting from the year 2010.

The aim of this introduction is not to introduce input-output economics, or SIOTs in detail. The Eurostat manual on input-output tables and the [Eurostat tematic page](http://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/methodology/symmetric-input-output-tables) should be consulted for further information about the data and the metadata.

In order to test the analytical functions of the package and to have a manageable sized example data set, we use the real-life data from the Eurostat manual. The `germany_1990` dataset is a simplified 6x6 sized SIOT taken from the Eurostat SIOT manual. The package function examples can be checked against Jörg Beutel’s published results.

Installation
------------

You can install iotools from github with:

``` r
# install.packages("devtools")
devtools::install_github("rOpenGov/iotables")

#with vignettes:
#devtools::install_github("rOpenGov/iotables", build_vignettes = TRUE)
```

YOu can follow changes on the NEWS.md file.

Acquiring data
--------------

Eurostat's data can be downloaded in several tidy, long-form, files, and a lot of filtering is needeed to start working with it. In case you need to work with a SIOT given in Excel, for example, Eurostat's pre-2010 files, which cannot be downloaded with the eurostat package, please consult the Croatia vignette. What you want to do is to arrive to a similar format to Eurostat's long-form.

Currently the following Eurostat SIOTs can be used: "naio\_10\_cp1700", "naio\_10\_cp1750", "naio\_10\_pyp1700", "naio\_10\_pyp1750", "naio\_cp17\_r2", "naio\_17\_agg\_60\_r2", "naio\_17\_agg\_10\_r2". The tables follow a similar structure, but they are differing in two important aspects. The first quadrant may be derived from goods and services sales data based on COICOP codes, or from sales data of enterprises based on their NACE Rev 2. Industry codes. Eurostat uses different labelling for product x product, product x industry and industry x industry cases. You can preserve this labelling with choosing the labelling = short option.

There is currently no downloader function for the [OECD STAN](https://stats.oecd.org/Index.aspx?DataSetCode=STAN_IO_TOT_DOM_IMP) SIOT databases, which contain the SIOT’s of some OECD countries, often overlapping with the EU countries who create their SIOTs, but this is planned in the near future. The code below gets the 2011 Australian SIOT in USD terms in long-form. *Beware that the `RJSDMX` package requires `rJava`, whicn in turn on Windows machines requires the 64-bit version of Java to be installed on your computer.* The next pre-processing steps that are already available for Eurostat will be included in the 0.3 version of the package.

The Eurostat bulk downloader will give you a very long file: all country data in a long-form, with different currencies, and for different years. The `iotable_get` function will filter out a table for you.

-   The default labelling = 'iotables' parameter will change all row and column names to a standard snake\_var\_name format naming. The alternative is to keep the 'short' Eurostat codes, but they vary accross statistical products, reflecting the COICOP, NACE or mixed source of the data, and sometimes the aggregation level, too.

The code above depends on the `get_eurostat()` function of the Eurostat package. This function call will create a temporary .rds file during your session to avoid too large data volumes. The `iotable_get()` filters out in the example of the Slovak IO table for the year 2010 in million Euros. Given that the Eurostat bulk file has all year and two currency units (`unit = MIO_EUR` and `unit = MIO_NAC`, which is equivalent in the eurozone member Slovakia) it is a rather large file. Parameter `labelling='short'` will keep the original Eurostat labelling, `labelling='iotables'` will use more readable, *snake\_format* variable names.

You can access the temporary file name from the function message: `A temporary file is is saved as C:\Users\...AppData\Local\Temp\RtmpcPsWmy\naio_cp17_r2_SK_2010_MIO_EUR.rds.`

or create it yourself

``` r
require (dplyr) ; require (iotables)

#Retrieve the bulk file from the temporary directory
retrieve_from_temp_bulk <- readRDS(paste0(tempdir(), "\\eurostat/naio_cp17_r2_date_code_TF.rds" ))

source <- "naio_cp17_r2"
#Get the temporary file name of any already existing temporary file with:
tmp_rds <- paste0(tempdir(), "\\", source, 
          "_", geo, "_", year , "_", unit, ".rds")

#Retrieve the filtered file from the temporary directory
retrieve_from_temp <- readRDS(tmp_rds)
```

Preprocessing
-------------

The `use_table_get ()` function extracts only the input flow matrix from the table.

-   The `unit` parameter is necessary because Eurostat files contain the same data in national currency units and euros.

-   The `year` should be given as a numeric.

-   The `geo` can be inputed with 2 letter country codes or country names. Beware, that Eurostat UK for the United Kingdom instead of GB and EL for Greece instead of GR.

-   The `stk_flow` variable is only used when the 'DOM' and 'IMP' type SIOTs differ. In the case of SIOTs that were created differently, this parameter is not in use.

### Final demand

The second quadrant contains the data of macroeconomic demand. Currently only the most often used vector, the final demand / output is processed. However, if you have some specific analytical need, you can filter out any details from the second quadrant following the logic of working with the first quadrant.

In all cases, the `households = TRUE` parameter will add the household consumption expenditure column, and in the case of the use table the wages (or compensation) row.

    #>    iotables_row agriculture_group manufacturing_group construcion_group
    #> 16    output_bp             43910             1079446            245606
    #>    trade_group business_services_group other_services_group
    #> 16      540063                  692487               508918
    #>    consumption_expenditure_household
    #> 16                           1001060

Analytical functions
--------------------

The most frequently used input-output equations are programmed into convenient R functions. In almost all statistics and data scientific context, data pre-processing is usually the most time consuming effort, and this is probably even more true in the case of SIOT’s, because of their complexity. If all elements are in place, working with the elements is easy with R’s matrix algebraic operators such as `%*%`. (Needless to say that in the othervise userful vectorization nature of R’s main operators and functions is avoidable.) However, because of the high complexity of the data, any violation of matrix algebraic rules causes a very hard-to-detect problem. If a 63x63 matrix has a single faulty element, the mathematical error message of the matrix operation is not very helpful on localizing and the correcting the element in question. For this reason we create a few helper functions that cover the most used scenarios.

The `leontieff_matrix_create()` function creates the Leontieff-matrix from the appropriate SIOT fields. The `leontieff_inverse_create(L)` creates the inverse of the Leontieff-matrix which is used in all basic equations.

``` r
de_use <- use_table_get ( source = "germany_1990", geo = "DE",
                       year = 1990, unit = "MIO_EUR", 
                       households = FALSE, labelling = "iotables")

de_output <- output_get ( source = "germany_1990", geo = "DE",
                       year = 1990, unit = "MIO_EUR", 
                       households = FALSE, labelling = "iotables")

de_coeff <- input_coefficient_matrix_create( de_use, de_output, digits = 4)

L <- iotables::leontieff_matrix_create( technology_coefficients_matrix =
                                de_coeff )

print (L)
#>              iotables_row agriculture_group manufacturing_group
#> 1       agriculture_group            0.9742             -0.0236
#> 2     manufacturing_group           -0.1806              0.7178
#> 3       construcion_group           -0.0097             -0.0068
#> 4             trade_group           -0.0811             -0.0674
#> 5 business_services_group           -0.0828             -0.0890
#> 6    other_services_group           -0.0353             -0.0139
#>   construcion_group trade_group business_services_group
#> 1            0.0000     -0.0011                 -0.0010
#> 2           -0.2613     -0.0761                 -0.0173
#> 3            0.9842     -0.0098                 -0.0339
#> 4           -0.0578      0.8622                 -0.0156
#> 5           -0.1263     -0.1218                  0.7210
#> 6           -0.0071     -0.0208                 -0.0217
#>   other_services_group
#> 1              -0.0015
#> 2              -0.0597
#> 3              -0.0180
#> 4              -0.0413
#> 5              -0.0672
#> 6               0.9566
```

The Leontieff matrix shown here can be checked against the Eurostat manuals table 15.9 on page 487. (Beware, the ordering of the industries is different.)

``` r
I <- leontieff_inverse_create(L)
print (I)
#>              iotables_row agriculture_group manufacturing_group
#> 1       agriculture_group        1.03390950          0.03501839
#> 2     manufacturing_group        0.28968590          1.42923465
#> 3       construcion_group        0.02070433          0.01910180
#> 4             trade_group        0.12698600          0.12146217
#> 5 business_services_group        0.18418167          0.20706399
#> 6    other_services_group        0.04945504          0.02953987
#>   construcion_group trade_group business_services_group
#> 1        0.01000918 0.005052355             0.002987288
#> 2        0.39622326 0.142036672             0.059631573
#> 3        1.02897000 0.021085502             0.050076278
#> 4        0.10646629 1.178454208             0.035494905
#> 5        0.25032070 0.223971579             1.412619060
#> 6        0.02175724 0.033111432             0.034164749
#>   other_services_group
#> 1          0.004422993
#> 2          0.107427569
#> 3          0.025014479
#> 4          0.063154485
#> 5          0.126826042
#> 6          1.051529078
```

Testing and documentation
-------------------------

Testing the package has two important difficulties. One is related to the size of the tables: working with large SIOT’s is resource intensive so some functionality cannot be usefully checked on CRAN.

For easier automated and human testing the example input-output table is imported from the Eurostat manual. The "CPA\_" preffix makes the formatting similar to the Eurostat raw files. The "t\_rows2" and "t\_cols2" variable names (and their labelled pairs, i.e. "t\_row2\_lab" and "t\_cols2\_lab") follows the naming conventions of Eurostat, too.

``` r
require (tidyr) ; require (dplyr)
head ( germany_1990, 2)
#>   t_rows2       t_rows2_lab             t_cols2 values         t_cols2_lab
#> 1   cpa_a Agriculture group   agriculture_group   1131   Agriculture group
#> 2   cpa_a Agriculture group manufacturing_group  25480 Manufacturing group
#>   geo geo_lab       time    unit     unit_lab
#> 1  DE Germany 1990-01-01 MIO_EUR Million euro
#> 2  DE Germany 1990-01-01 MIO_EUR Million euro
```

The other problem is that the input-output economics problems are very complex and do not necessarily have existing solutions. A failure of a function may be due to a bug to be fixed in the code, or maybe the logical consequence of an input failure. For this reason the test functions are ran with well-documented examples from the literature. The SIOT’s used in these examples and in testing are small, simple or simplified data which are included with the package. The expected test results are the published and rounded examples from the literature. Rounding is necessary because the underlying equations often use iterations that may yield slightly different results on different platforms.
