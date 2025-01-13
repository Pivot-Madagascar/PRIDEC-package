# Lab notebook for PRIDEC Package

## Development workflow

*All use the devtools package*

`use_r()`: creates an R script with that filename to hold the function
`use_test()`: creates an R script to hold the tests for that function
`use_package()`: adds package to `Import` in `DESCRIPTION`. call functions using `package::function`
`use_import_from()`: imports a specific function from a package to use without the `::`. Not recommended.


`test()`: runs the code tests
`document()`: update package documentation
`build_readme()`: builds the README. better to use becuase it uses source version of your package
`load_all()`: builds and loads package locally
`check()`: runs through all the CRAN and other tests to make sure it is okay
`install()`: install the source version of the package

For more info, use [R packages book](https://r-pkgs.org/).

## 2024-01-13

Working on CV split functions. I've got these written with some tests, but they aren't anything crazy.

Also wrote tests for the prep_caseData functions.

Started writing documents for the demo data, but having issues with the `check` picking it up. 

**TO DO:**
- ~~write tests for seasonal_fill~~
- ~~write more tests for prep_caseData~~
- document demo data (demo_malaria, demo_polygon)
- start a naive modeling function

## 2024-01-08

Started development in package architecture. This involves moving things over from the modeling repo. One thing I really needed to be able to do this was a demo dataset to test things on. I created a simulated dataset of malaria cases from some data in Ifanadiana, `demo_malaria`, and the spatial polygon that goes with it, `demo_polygon`. This should work for testing everything out (hopefully).

I have worked through the data_prep step, now I need to do the cv_split steps to finish all the data treatment

**TO DO:**
- cv split functions
- start modeling functions (start with naive and glm?)