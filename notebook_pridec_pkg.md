# Lab notebook for PRIDEC Package

## Development workflow

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

## 2024-01-08

Started development in package architecture. This involves moving things over from the modeling repo.