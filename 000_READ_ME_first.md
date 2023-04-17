# Fast-track Delphi: code with demo data and results

## Purpose

The `fast-track Delphi` was developed by [Unisanté](https://www.unisante.ch/) to provide consensus information for political decisions in context of public health crisis.

The process consists in three rounds of consultation. The participants are brought together in a first round during which a series of topics/questions/statements will be collected using a nominal group technique (NGT). The participants then express their opinion on these statements by responding individually to questions in the second and third rounds using an online questionnaire built with [**`REDCapR`**](https://ouhscbbmc.github.io/REDCapR/).

In order to produce results in a very short time frame, we use an [R project](https://r4ds.had.co.nz/workflow-projects.html) to analyse the results and produce **editable word documents** :

-   a generic report and individualised reports for the second round (dft2)

-   a generic report and individualised reports for the third round (dft3)

-   an overall executive summary

## Softwares and packages {#sec-software-and-packages}

-   interface and language : [**`RStudio`**](https://www.rstudio.com/) and [**`R Statistical Software`**](https://www.r-project.org/)

-   obtain data : [**`REDCapR`**](https://ouhscbbmc.github.io/REDCapR/) (not necessary for the demo)

-   data management, analysis and visualisation : mainly [**`data.table`**](https://rdatatable.gitlab.io/data.table/), with some [**`tidyverse`**](https://www.tidyverse.org/) and other packages. Regex expressions are sometimes used.

-   reports : [**`officedown`**](https://ardata-fr.github.io/officeverse/officedown-for-word.html) which builds on [**`bookdown`**](https://pkgs.rstudio.com/bookdown/), and [**`flextable`**](https://davidgohel.github.io/flextable/) from the [**`officeverse`**](https://ardata-fr.github.io/officeverse/index.html)


## User guide

Please go through the [Fast-track Delphi: user guide](https://github.com/Unisante) to discover the steps with demo data, see what it produces and then try it out.



## Licence

© 2023 by Olivier Duperrex is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-nc-sa/4.0/).

![CC BY-NC-SA 4.0](https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png){fig-align="left"}

Citation: Duperrex O and Velarde Crézé C. Fast-track Delphi: code with demo data and results. 2023. Département Promotion de la santé et préventions, Unisanté, Lausanne, Suisse. Available from: [https://github.com/Unisante/](https://github.com/Unisante/)
