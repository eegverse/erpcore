
<!-- README.md is generated from README.Rmd. Please edit that file -->

# erpcore

The goal of `erpcore` is to provide some simple command line functions
for downloading the ERP CORE (Kappenman, Farrens, Zhang, Stewart, &
Luck, 2021) datasets from the [Open Science Framework](https://osf.io/)
website.

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

<center>

![ERP CORE
components](https://images.squarespace-cdn.com/content/v1/5abefa62d274cb16de90e935/1607295176882-299LXAX6HQDAVXTOGEZN/ke17ZwdGBToddI8pDm48kAkf_b2_D9TWbiyP0N7V4rwUqsxRUqqbr1mOJYKfIPR7LoDQ9mXPOjoJoqy81S2I8PaoYXhp6HxIwZIk7-Mi3Tsic-L2IOPH3Dwrhl-Ne3Z2_qvAvDjSegT0JnqIGXgpCjZQtO2QR5CCUQNW-e73k9gfdurHDt8XjyaGurlSWvQe/ERP_CORE_Summary.jpg?format=500w)

</center>

Kappenman, E., Farrens, J., Zhang, W., Stewart, A. X., & Luck, S. J.
(2021). ERP CORE: An Open Resource for Human Event-Related Potential
Research. *NeuroImage*.
<https://doi.org/10.1016/j.neuroimage.2020.117465>

For more information about the ERP CORE, visit [their
website](https://erpinfo.org/erp-core).

You can also directly access the data at the [Open Science Framework
website](https://doi.org/10.18115/D5JW4R)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("eegverse/erpcore")
```

## Usage

The `get_erpcore()` function can be used to download individual
components from the ERP CORE datasets. For example, you can request to
download the N170 data. By default, the data will create a new directory
in your current working directory.

``` r
get_erpcore(component = "n170")
```

The data can be requested in three different formats:

``` r
get_erpcore(component = "n170", type = "raw")
get_erpcore(component = "n170", type = "bids")
get_erpcore(component = "n170", type = "all")
```

-   `"raw"` returns the original data in `.set` format, along with
    processing scripts for the Matlab ERPLAB toolbox.
-   `"bids"` returns the same data in a BIDS compliant organization.
-   `"all"` returns the raw data, copies of fully-processed data, and
    copies of the data at every stage of preprocessing.

You can also use one of the specific functions for each component. For
example, to request the data for the N400 component in BIDS format, you
can use:

``` r
get_n400(type = "bids")
```

## Information about Sharing and Copyright

These resources are shared under the terms of a Creative Commons license
(CC BY-SA 4.0). Briefly, you are free to copy, share, remix, transform,
and build upon these resources in any way you desire. However, you are
required to give appropriate credit, and if you share the resulting
materials, you must distribute your contributions under the same CC
BY-SA 4.0 license. Please credit us in any resulting publications,
reports, or data distributions as: Kappenman, E. S., Farrens, J. L.,
Zhang, W., Stewart, A. X., & Luck, S. J. (2020). ERP CORE: An open
resource for human event-related potential research. *NeuroImage.*
<https://doi.org/10.1016/j.neuroimage.2020.117465>
