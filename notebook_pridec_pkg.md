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

## 2025-02-11

I will work on a vignette of the full workflow where multiple models are trained. This will also allow me to write the full wrapper/workflow function. Okay, yes while working on this I have realized I need a wrapper `train_models` function. This takes in the data and user-provided parameters and outputs the quarto report. 

Notes for self (what does train_model function do):
- takes cv_set list, list of models to fit, list of pred_vars, y_var, results_directory
- applies each model over each element of the list to get evaluation 
- gets inv_var for last cv_set
- saves model outputs somewhere
- launches script to create quarto doc from model output

This may also help streamline the `ensemble_forecast` function. Atlhough I think realistically any streamlining of that will happen within the script within the docker container, since that is where the user-input will enter.

**TO DO**:
- INLA hyper-parameter tuning (later)
- write an example vignette of full workflow
- script to create quarto document of training step (like a launching shell script). similar to output of workshops
- data import and export scripts to DHIS2 (need to get d2 working first)

## 2025-02-10

Working on setting things up on github and having a pkgdown website. I fixed the github commit error. The problem was I was using the wrong email.

Setting this up as a github pages, and also making the Pivot website itself nicer.

**TO DO**:
- INLA hyper-parameter tuning (later)
- write an example vignette of full workflow
- script to create quarto document of training step (like a launching shell script). similar to output of workshops
- data import and export scripts to DHIS2 (need to get d2 working first)


## 2025-02-07

Working on creating some documentation and ultimately maybe even pushing things to github. Also started writing the function for `ensemble_forecast`.  This is a stacked ensemble, but there are other ways of getting the weighted coefficients like blending via  lasso regression, but this one allows us to use the participant input.

I am also working on uploading everything to github and gettinga  working website.

**TO DO**:
- ~~make map/workflow of APIs~~
- INLA hyper-parameter tuning (later)
- ~~build launch model function based on map~~
- upload to github
- write an example vignette
- script to create quarto document of training step (like a launching shell script)

## 2025-02-06

Going back to the INLA code to finish that up. Working on creating the counterfactual data function. This will then be combined with the variable importance one into one function to keep with the standard API.

**TO DO**:
- make map/workflow of APIs
- INLA hyper-parameter tuning (later)
- build launch model function based on map

## 2025-01-29

Working on adding INLA to the modules. So far transfered over the model and the code for variable importance. Still need to do the code for counter-factual plots and function that does both together to match the API of other models. Then we may also want to add some tuning?

I also got a note about too many non-default packages and how I could move this to suggests and use them conditionally. I think this could probably be done by module (so for example add a check that if the required packages are not installed for a module to prompt that they should be). Hweover this does add more work and potnetial break points for end users that may not be as familiar with R. Something to think about but that can be updated later.

**TO DO:** 
- INLA model: 
  - finish up counterfactual plots and explorign variables
  - Tuning of hyperparameters (can also be later)
- code for launching models (running multiple) [this should probably be a function]. See notes below.
- code for creating ensemble

## 2025-01-28

Added functionality to log-transform arimax. In certain instances, it seems to perform better on predictions (which makes sense because these are poisson processes). It is controlled via the `log_trans` param.

Starting work on glm.nb model from the `MASS` package. I had some code from before for doing variable selection where each variable is dropped in a certain order to tune the model, but I am not going to move that over to this package. There is also some code to estimate the prediction inteval via simulations, but I will stick with doing it analytically because I think it is cleaner.

I'm going to wait to do the INLA stuff until tomorrow. but in the meantime I am thinking about how to write the script to launch an ensemble of models. I have a bunhc of make scripts from the 3rd workshop, and I think I would need to turn some of them into R scripts. Or I leave it as something that is called via a sh script with arguments from the command line (probably best for interacting with ETL).

For the model fitting, it could be something similar to `fit_models_template.R`, which fits for each disease. It takes a file of the data and a file containing the names of predictor variables (but maybe that could be just provided as strings), as well as the name of the disease, which is used to identify the `y_var`. It would probably also need a directory to output into (which is currently manually set). It also needs info on which variables to lag and scale, and perhaps a step to adjust to turn them into predictor variables. It would probably also take the names of the models to fit (naive, glm, ranger,inla, arimax). Maybe look at what DHIS2 does for this to get an idea, because it would likely need to be passed into a docker container.

**TO DO:** 
- ~~look into log-transforming for ARIMAX: idea would be to add this as an argument to the function~~
- ~~GLM model: shouldn't be too bad to set up since there is no tuning (1/2 day?)~~
- INLA model: this will probably take 1-2 days to do, especially if I want to add tuning of hyper-parameters.
- code for launching models (running multiple) [this should probably be a function]
- code for creating ensemble

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