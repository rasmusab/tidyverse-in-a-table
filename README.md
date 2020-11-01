# The Tidyverse in a Table

This is a submission to the [2020 RStudio Table Contest](https://blog.rstudio.com/2020/09/15/announcing-the-2020-rstudio-table-contest/) in the Tutorials for interactive-HTML tables category. The rendered tutorial is here: https://rpubs.com/rasmusab/684202

If you have docker installed you should be able to build the tutorial like this:

```
docker run --rm \
-v $(pwd):/tmp/working_dir \
-w /tmp/working_dir \
rocker/tidyverse:4.0.3 \
Rscript -e 'install.packages("DT"); rmarkdown::render("tidyverse_in_a_table.Rmd")'
```

Here's a screenshot of the rendered table:

![](tidyverse_in_a_table_screenshot.png?raw=true)