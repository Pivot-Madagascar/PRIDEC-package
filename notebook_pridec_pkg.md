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

## 2025-01-16

Working on the evaluation code, particularly the calculation of the WIS. My plan is to write a function myself and double check with scoringutils that it works. Eh, for now I will just stick with using `scoringutils` and I can transition to soemthing different later if I want.

**TO DO**:
- ~~write test for naive model~~
- document demo data (demo_malaria, demo_polygon) [not sure why this isn't currently working]
- ~~finish naive model~~
- start ARIMAX model functions

## 2025-01-14

Starting work on the modeling functions. We want to have the same language/API call for each model, so we have

- `fit`: fits to a list containing analysis and assessment data (basically trains on analysis and predicts on analysis and assessment). Returns prediction intervals on both sets.
- `evaluate`: takes a data.frame of prediction intervals and true_values and returns performance metrics (ae_median, r2, mae_scaled, interval_score, over and under estimation). Not all performance metrics are available for all models.
- `tune`: not for all models, this allows the user to tune parameters in the model using multiple `CV_set`s
  - this basically runs `fit` and `evaluate` multiple times to identify the best tuned model
- `investigate`: investigates variablbes in the model (counter-factual plots, coefficients, etc.)

a `CV_set` is a list object that contains an `analysis` and `assessment` data.frame that will be used to train a model and predict, respectively

The `evaluate` step uses some functionality from the `scoringutils` package, but some of it isn't quite flexible enough for what I need and I am worried it is going to change, so I am going to use some intermediate functions from there. Actually, in the end I just updated the `scoringutils` version I use and am going with that since it exists for the functionality of automatically calculating scores. A future version could write these myself, but I kind of trust the `epiforecast` group to keep track of these well. Okay, actually now the only thing I am really using is the `wis` (weighted interval score), which I kind of feel like I could calculate myself using the intermediate function from `scoringutils`, so I will go back to that.


**TO DO:**
- document demo data (demo_malaria, demo_polygon)
- start a naive modeling function [started but currently working on evaluation step]
- finish evaluation code (particularly the wis calculation, although maybe best is just to write it myself and double-check with scoringutils)

## 2025-01-13

Working on CV split functions. I've got these written with some tests, but they aren't anything crazy.

Also wrote tests for the prep_caseData functions.

Started writing documents for the demo data, but having issues with the `check` picking it up. 

**TO DO:**
- ~~write tests for seasonal_fill~~
- ~~write more tests for prep_caseData~~
- document demo data (demo_malaria, demo_polygon)
- start a naive modeling function

## 2025-01-08

Started development in package architecture. This involves moving things over from the modeling repo. One thing I really needed to be able to do this was a demo dataset to test things on. I created a simulated dataset of malaria cases from some data in Ifanadiana, `demo_malaria`, and the spatial polygon that goes with it, `demo_polygon`. This should work for testing everything out (hopefully).

I have worked through the data_prep step, now I need to do the cv_split steps to finish all the data treatment

**TO DO:**
- cv split functions
- start modeling functions (start with naive and glm?)