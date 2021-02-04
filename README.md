
<!-- README.md is generated from README.Rmd. Please edit that file -->

# erpcore

The goal of `erpcore` is to provide some simple command line functions
for downloading the ERP CORE datasets (Kappenman, Farrens, Zhang,
Stewart, & Luck, 2020) from the [Open Science
Framework](https://osf.io/) website.

In their own words:

> The ERP CORE is a freely available online resource consisting of
> optimized paradigms, experiment control scripts, example data from 40
> participants, data processing pipelines and analysis scripts, and a
> broad set of results for 7 different ERP components obtained from 6
> different ERP paradigms:
>
> -   N170 (Face Perception Paradigm)
>
> -   MMN (Passive Auditory Oddball Paradigm)
>
> -   N2pc (Simple Visual Search Paradigm)
>
> -   N400 (Word Pair Judgement Paradigm)
>
> -   P3b (Active Visual Oddball Paradigm)
>
> -   LRP and ERN (Flankers Paradigm)

The ERP CORE is developed and maintained by

Kappenman, E., Farrens, J., Zhang, W., Stewart, A. X., & Luck, S. J.
(2020). ERP CORE: An Open Resource for Human Event-Related Potential
Research \[Preprint\]. PsyArXiv. <https://doi.org/10.31234/osf.io/4azqm>

For more information about the ERP CORE, visit [their
website](https://erpinfo.org/erp-core).

You can also directly access the data at the [Open Science Framework
website](https://doi.org/10.18115/D5JW4R)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("craddm/erpcore")
```

## Usage

The `get_erpcore()` function can be used

``` r
get_erpcore(component = "n170")
```

Or you can use one of the specific functions

``` r
get_n400(type = "bids")
```
