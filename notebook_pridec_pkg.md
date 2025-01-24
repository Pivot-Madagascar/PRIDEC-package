# Lab notebook for PRIDEC Package

## Development workflow

*All use the devtools or usethis packages*

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

## 2025-01-24

Working on Ranger RF function. This one is a little more complicated because it can involve tuning and variable importance.

Okay, to get this working, there is something a bit confusing because of the nestedness of how we estimate variable importance. The best is probably to provide all the same info we provide to the initial fitting function to this one so we can call it. This can maybe be done with `...`.

Also needed to create all of the visualizations while making this since it includes counter-factual plots and also to plot predictions.

And I made the ranger tuning function. This can be used as the basis for tuning INLA if we want, like to follow the same API structure

**TO DO:** 
- look into log-transforming for ARIMAX: idea would be to add this as an argument to the function
- GLM model: shouldn't be too bad to set up since there is no tuning (1/2 day?)
- INLA model: this will probably take 1-2 days to do, especially if I want to add tuning of hyper-parameters.
- code for launching models (running multiple)
- code for creating ensemble

## 2025-01-22

Finally got the data documentation to work. I did two things (not sure what fixed it), I changed the name of the script holding the data documentation to `data.R` and I added  string of the data name under the roxygen stuff. The issue is I can't figure out how to add multiple to the same script? Ah okay I just needed an empty line between each one and to create a seperate thing for each one. This should work okay now.


**TO DO:**
- ~~document demo data (demo_malaria, demo_polygon)~~
- ~~Finish ARIMAX: Evaluation function~~
- start ranger/RF function 

## 2025-01-21


Okay the current evaluation function does work on the arimax model, although it results in a lot of error messages. I think the best way to solve that is to move away from the `scoringutils` package. I decided to use the intermediate `wis` function because it already had a bunch of stuff built in that I didn't want to recreate. But I am now using it directly on the data so I don't get all those error messages. It is much slower now though becuase I didn't vectorize it, so I rewrote it to take better advantage of vectorizing it.

Also cleaned up some undefined variables bits in existing functions.

**TO DO:**
- document demo data (demo_malaria, demo_polygon) [not sure why this isn't currently working]
- ~~Finish ARIMAX: Evaluation function~~
- start ranger/RF function 


## 2025-01-17

Start work on ARIMAX model. The ARIMA fits to one orgUnit at a time and has a bunch of internal functions, which means I have donea  bunch of trouble-shooting.

One issue is that the forecast package has a conflict with the new `scoringutils` package. I get this error when doing `load_all()`:

```
Registered S3 method overwritten by 'scoringutils':
  method         from    
  print.forecast forecast
```

But I don't want the `forecast` method to be overwritten because that is the package that hosts the ARIMA functions. One solution is to use the 3-colon `:::` to access functions that aren't exported. but the issue is it is a type of `print.` method. Nope that wasn't it. It is still messing up the print methods, but I have just wrapped it in `suppressWarnings` for now in the `get_arima_pi` function, but I think probably the best long term solution is to replace `scoringutils` with my own function.

Running the tests, now becuase I ahve switched to using `.data$` for NSE and not having unnamed variables, it now gives me an error message that `Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.`, so I am not sure the best way to use this within a package. Info is here:https://forum.posit.co/t/use-of-data-in-tidyselect-expressions-is-now-deprecated/150092/5

So what I need to do is switch to using `all_of` plus character names of columns, like `all_of(c("s1","x2","x_new" = "x_old")). Okay, I think I have solved all of these dumb testing errors but it took way too long. I guess I could have just used base R rather than dply for this

**TO DO:**
- document demo data (demo_malaria, demo_polygon) [not sure why this isn't currently working]
- ~~create a util function to just check the y_var, pred_var, and data arguments are okay. This can then be run for all modeling functions.~~
- Finish ARIMAX: Evaluation function
- start ranger/RF function 

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