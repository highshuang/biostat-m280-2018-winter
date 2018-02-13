*Shuang Gao*

### Overall Grade: 94/100

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline?

    Yes. `Feb 1, 2018, 2:24 PM PST`.

-   Is the final report in a human readable format html?

    Yes. `html`.

-   Is the report prepared as a dynamic document (R markdown) for better reproducibility?

    Yes. `Rmd`.

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how are results produced by just reading the report?

    Yes. Excellent job!

### Correctness and efficiency of solution: 50/50

-   Q1

-   Q2

-   Q3

### Usage of Git: 10/10

-   Are branches (`master` and `develop`) correctly set up? Is the hw submission put into the `master` branch?

    Yes.

-   Are there enough commits? Are commit messages clear?

    Yes.

-   Is the hw1 submission tagged?

    Yes.

-   Are the folders (`hw1`, `hw2`, ...) created correctly?

    Yes.

-   Do not put a lot auxillary files into version control.

    Yes.

### Reproducibility: 8/10

-   Are the materials (files and instructions) submitted to the `master` branch sufficient for reproducing all the results? (-2 pts)

    In general, I was able to run Rmd file and reproduce all the results. Note in your `format.R`, the path `setwd("~/biostat-m280-2018-winter/hw1")` is for your own server. Make sure your collaborators can easily run your code. You may use something like

    ``` r
    setwd(".")
    ```

    for easier reproducibility.

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

    Yes.

### R code style: 16/20

-   [Rule 3.](https://google.github.io/styleguide/Rguide.xml#linelength) The maximum line length is 80 characters.

-   [Rule 4.](https://google.github.io/styleguide/Rguide.xml#indentation) When indenting your code, use two spaces.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place spaces around all binary operators (=, +, -, &lt;-, etc.). (-2 pt)

    Some violations:
    -   `format.R`: lines 32, 47-49

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place a space before a comma, but always place one after a comma. (-2 pts)

    Some violations:
    -   `format.R`: lines 7, 26, 54
    -   `autoSim.R`: line 12
    -   `runSim.R`: line 40

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place a space before left parenthesis, except in a function call.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place spaces around code in parentheses or square brackets.
