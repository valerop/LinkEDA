#!/usr/bin/env Rscript

# Rebuild the public example datasets distributed with LinkEDA.  Every source
# is an established R dataset with online documentation; the transformations
# below only make row identifiers, paired measurements, or contingency-table
# frequencies explicit for use in a rectangular data sheet.

arguments <- commandArgs(trailingOnly = FALSE)
script_argument <- sub("^--file=", "", arguments[grepl("^--file=", arguments)][1L])
root <- if (length(script_argument) && !is.na(script_argument)) {
  normalizePath(file.path(dirname(script_argument), ".."), mustWork = TRUE)
} else normalizePath(getwd(), mustWork = TRUE)
out <- file.path(root, "inst", "examples", "statistical")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

write_example <- function(data, file) {
  utils::write.csv(as.data.frame(data, stringsAsFactors = FALSE),
                   file.path(out, file), row.names = FALSE, na = "")
}

mtcars_example <- data.frame(car = row.names(datasets::mtcars), datasets::mtcars,
                             row.names = NULL, check.names = FALSE)
write_example(mtcars_example, "mtcars.csv")

anscombe_long <- do.call(rbind, lapply(seq_len(4L), function(set) {
  data.frame(set = factor(set), observation = seq_len(nrow(datasets::anscombe)),
             x = datasets::anscombe[[paste0("x", set)]],
             y = datasets::anscombe[[paste0("y", set)]])
}))
write_example(anscombe_long, "anscombe.csv")

airquality_example <- transform(datasets::airquality,
  date = as.character(as.Date(sprintf("1973-%02d-%02d", Month, Day))))
airquality_example <- airquality_example[c("date", "Month", "Day", "Ozone",
                                            "Solar.R", "Wind", "Temp")]
write_example(airquality_example, "airquality.csv")

chickweight <- as.data.frame(datasets::ChickWeight)
chickweight$Chick <- as.character(chickweight$Chick)
chickweight$Diet <- paste("Diet", chickweight$Diet)
write_example(chickweight, "chickweight.csv")

iris_example <- data.frame(specimen = sprintf("I%03d", seq_len(nrow(datasets::iris))),
                           datasets::iris, row.names = NULL, check.names = FALSE)
write_example(iris_example, "iris.csv")

sleep_wide <- reshape(datasets::sleep, idvar = "ID", timevar = "group",
                      direction = "wide")
names(sleep_wide) <- c("participant", "extra_drug_1", "extra_drug_2")
sleep_wide$paired_difference <- sleep_wide$extra_drug_2 - sleep_wide$extra_drug_1
write_example(sleep_wide, "sleep_paired.csv")

toothgrowth <- datasets::ToothGrowth
toothgrowth$dose_group <- factor(toothgrowth$dose,
                                 levels = c(0.5, 1, 2),
                                 labels = c("0.5 mg", "1 mg", "2 mg"))
write_example(toothgrowth, "toothgrowth.csv")

usarrests <- data.frame(state = row.names(datasets::USArrests), datasets::USArrests,
                        row.names = NULL, check.names = FALSE)
write_example(usarrests, "usarrests.csv")
write_example(datasets::warpbreaks, "warpbreaks.csv")

titanic_counts <- as.data.frame(datasets::Titanic, stringsAsFactors = FALSE)
titanic <- titanic_counts[rep(seq_len(nrow(titanic_counts)), titanic_counts$Freq),
                           c("Class", "Sex", "Age", "Survived"), drop = FALSE]
titanic$passenger <- sprintf("P%04d", seq_len(nrow(titanic)))
titanic <- titanic[c("passenger", "Class", "Sex", "Age", "Survived")]
row.names(titanic) <- NULL
write_example(titanic, "titanic.csv")

data("quine", package = "MASS")
school_absence <- MASS::quine
names(school_absence) <- c("ethnicity", "sex", "age_group", "learner", "days_absent")
write_example(school_absence, "school_absence.csv")

data("Insurance", package = "MASS")
insurance_claims <- MASS::Insurance
insurance_claims$log_holders <- log(insurance_claims$Holders)
names(insurance_claims) <- c("district", "car_group", "age_group", "holders",
                             "claims", "log_holders")
write_example(insurance_claims, "insurance_claims.csv")

data("GasolineYield", package = "betareg")
gasoline_yield <- get("GasolineYield", inherits = FALSE)
gasoline_yield$batch <- as.character(gasoline_yield$batch)
write_example(gasoline_yield, "gasoline_yield.csv")

theoph <- as.data.frame(datasets::Theoph)
theoph$Subject <- as.character(theoph$Subject)
names(theoph) <- c("subject", "weight_kg", "dose_mg_per_kg", "time_hours",
                   "concentration_mg_l")
write_example(theoph, "theoph.csv")

data("bfi", package = "psych")
bfi <- get("bfi", inherits = FALSE)
bfi$gender <- factor(bfi$gender, levels = c(1, 2), labels = c("male", "female"))
write_example(bfi, "bfi.csv")

data("nhanes2", package = "mice")
nhanes2 <- get("nhanes2", inherits = FALSE)
write_example(nhanes2, "nhanes_missing.csv")
set.seed(20260925)
nhanes_mice <- mice::mice(nhanes2, m = 5, maxit = 5, printFlag = FALSE,
                          seed = 20260925)
saveRDS(nhanes_mice, file.path(out, "nhanes_mice.rds"), version = 3)

catalog <- data.frame(
  id = c("mtcars", "anscombe", "airquality", "chickweight", "iris",
         "sleep_paired", "toothgrowth", "usarrests", "warpbreaks", "titanic",
         "school_absence", "insurance_claims", "gasoline_yield", "theoph",
         "bfi", "nhanes_missing", "nhanes_mice"),
  title = c("Motor Trend cars", "Anscombe's quartet", "New York air quality",
            "Chick growth", "Iris flowers", "Paired sleep data",
            "Guinea-pig tooth growth", "US arrests", "Warp breaks",
            "Titanic passengers", "School absence", "Car insurance claims",
            "Gasoline yield", "Theophylline concentration",
            "Big Five personality items", "NHANES with missing values",
            "NHANES multiple imputation"),
  category = c("General", "Linear models", "Missing data", "Longitudinal",
               "Multivariate", "Mean comparisons", "Mean comparisons",
               "Multivariate", "Count models", "Categorical data",
               "Count models", "Count models", "Proportion models",
               "Positive continuous models", "Psychometrics", "Missing data",
               "Multiple imputation"),
  file = c("mtcars.csv", "anscombe.csv", "airquality.csv", "chickweight.csv",
           "iris.csv", "sleep_paired.csv", "toothgrowth.csv", "usarrests.csv",
           "warpbreaks.csv", "titanic.csv", "school_absence.csv",
           "insurance_claims.csv", "gasoline_yield.csv", "theoph.csv",
           "bfi.csv", "nhanes_missing.csv", "nhanes_mice.rds"),
  categorical_columns = c(
    "", "set", "", "Chick;Diet", "", "", "", "", "", "", "", "",
    "batch", "subject", "", "", ""
  ),
  techniques = c(
    "descriptives; correlation; one-sample tests; linear and binary models; model comparison; scatterplot; matrix; parallel coordinates; boxplot; histogram; bar chart",
    "linear models and diagnostics; grouped scatterplots; Trellis scatterplots",
    "missing-data overview and imputation; time series; scatterplot; histogram",
    "time series; Trellis plots; grouped scatterplots; linear models",
    "correlation; PCA/factor analysis; quick cluster; scatterplot matrix; parallel coordinates; grouped plots",
    "paired-samples tests; histogram; scatterplot",
    "two-sample tests; one-way ANOVA; factorial linear models; interaction interpretation; boxplot; bar chart; Trellis plots",
    "correlation; PCA/factor analysis; quick cluster; scatterplot matrix; parallel coordinates",
    "count models: Poisson and negative-binomial; factorial interactions; boxplot; bar chart; Trellis plots",
    "contingency tables; binary models; interactions; bar charts; Table 1",
    "Poisson and negative-binomial models; interactions; boxplot; histogram",
    "Poisson models with an offset; model comparison; categorical predictors",
    "beta proportion models; linear models; regression diagnostics; scatterplot",
    "positive-continuous models; time series; Trellis plots; grouped scatterplots",
    "scale analysis; correlation; PCA/factor analysis; missing data; Table 1",
    "missing-data overview; imputation; descriptives",
    "multiple-imputation analyses, pooled models, diagnostics, plots, and tables"
  ),
  source_package = c("datasets", "datasets", "datasets", "datasets", "datasets",
                     "datasets", "datasets", "datasets", "datasets", "datasets",
                     "MASS", "MASS", "betareg", "datasets", "psych", "mice", "mice"),
  source_url = c(
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/anscombe.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ChickWeight.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/iris.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/sleep.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/USArrests.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/warpbreaks.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/Titanic.html",
    "https://search.r-project.org/CRAN/refmans/MASS/html/quine.html",
    "https://search.r-project.org/CRAN/refmans/MASS/html/Insurance.html",
    "https://search.r-project.org/CRAN/refmans/betareg/html/GasolineYield.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/Theoph.html",
    "https://search.r-project.org/CRAN/refmans/psych/html/bfi.html",
    "https://search.r-project.org/CRAN/refmans/mice/html/nhanes.html",
    "https://search.r-project.org/CRAN/refmans/mice/html/nhanes.html"
  ),
  analysis_objective = c(
    "Explore relations among fuel economy and vehicle design, then compare descriptive and regression views.",
    "Show why plots and diagnostics are essential even when regression summaries are almost identical.",
    "Explore environmental time patterns and the consequences of incomplete measurements.",
    "Compare individual growth trajectories over time and assess differences among diets.",
    "Explore multivariate separation of three iris species.",
    "Estimate the paired change in sleep produced by two drugs.",
    "Compare tooth growth across supplement types and vitamin C doses.",
    "Explore multivariate differences in violent-crime indicators among US states.",
    "Model overdispersed counts of loom breaks by wool type and tension.",
    "Study associations among passenger class, sex, age, and survival.",
    "Model counts of school absences from pupil characteristics.",
    "Model insurance claim rates while accounting for the number of exposed policyholders.",
    "Model a bounded gasoline-yield proportion and inspect regression diagnostics.",
    "Describe concentration over time and between-subject pharmacokinetic variation.",
    "Evaluate scale reliability and the five-factor structure of personality items.",
    "Explore missing-data patterns and practice imputation.",
    "Fit and diagnose analyses pooled across multiple imputations."
  ),
  citation = c(
    "Henderson & Velleman (1981), Biometrics 37, 391-411; R datasets: mtcars.",
    "Anscombe (1973), The American Statistician 27, 17-21; R datasets: anscombe.",
    "Chambers et al. (1983), Graphical Methods for Data Analysis; R datasets: airquality.",
    "Crowder & Hand (1990), Analysis of Repeated Measures, Example 5.3; R datasets: ChickWeight.",
    "Fisher (1936), Annals of Eugenics 7, 179-188; R datasets: iris.",
    "Cushny & Peebles (1905); Student (1908); R datasets: sleep.",
    "Crampton (1947), Journal of Nutrition 33, 491-504; R datasets: ToothGrowth.",
    "McNeil (1977), Interactive Data Analysis; R datasets: USArrests.",
    "Tippett (1950), Technological Applications of Statistics; R datasets: warpbreaks.",
    "Dawson (1995), Journal of Statistics Education 3; R datasets: Titanic.",
    "Quine, cited in Aitkin (1978), JRSS A 141, 195-223; MASS: quine.",
    "Baxter, Coutts & Ross (1980), 21st International Congress of Actuaries; MASS: Insurance.",
    "Prater (1956), Petroleum Refiner 35(5), 236-238; betareg: GasolineYield.",
    "Boeckmann, Sheiner & Beal (1994), NONMEM Users Guide; R datasets: Theoph.",
    "Goldberg (1999) and Revelle, Wilt & Rosenthal (2010); psych: bfi.",
    "Schafer (1997), Analysis of Incomplete Multivariate Data, Table 6.14; mice: nhanes2.",
    "Schafer (1997), Analysis of Incomplete Multivariate Data, Table 6.14; mice: nhanes2."
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(catalog, file.path(out, "catalog.csv"), row.names = FALSE,
                 na = "")

descriptions <- list(
  mtcars = c(
    car = "Vehicle model identifier.", mpg = "Fuel economy in miles per US gallon.",
    cyl = "Number of engine cylinders.", disp = "Engine displacement in cubic inches.",
    hp = "Gross horsepower.", drat = "Rear axle ratio.",
    wt = "Vehicle weight in thousands of pounds.", qsec = "Quarter-mile time in seconds.",
    vs = "Engine shape code: 0 = V-shaped, 1 = straight.",
    am = "Transmission code: 0 = automatic, 1 = manual.",
    gear = "Number of forward gears.", carb = "Number of carburetors."
  ),
  anscombe = c(
    set = "Quartet member (1-4).", observation = "Observation number within the quartet member.",
    x = "Predictor value for the selected quartet member.",
    y = "Outcome value for the selected quartet member."
  ),
  airquality = c(
    date = "Calendar date in 1973.", Month = "Month number (May through September).",
    Day = "Day of month.", Ozone = "Mean ozone concentration in parts per billion.",
    Solar.R = "Solar radiation in langleys.", Wind = "Mean wind speed in miles per hour.",
    Temp = "Maximum daily temperature in degrees Fahrenheit."
  ),
  chickweight = c(
    weight = "Body weight of the chick in grams.",
    Time = "Days since birth at measurement.",
    Chick = "Unique chick identifier; repeated observations with the same value form one growth trajectory.",
    Diet = "Experimental protein diet assigned to the chick (four groups)."
  ),
  iris = c(
    specimen = "Specimen identifier added for linked selection.",
    Sepal.Length = "Sepal length in centimetres.", Sepal.Width = "Sepal width in centimetres.",
    Petal.Length = "Petal length in centimetres.", Petal.Width = "Petal width in centimetres.",
    Species = "Iris species: setosa, versicolor, or virginica."
  ),
  sleep_paired = c(
    participant = "Participant identifier for paired measurements.",
    extra_drug_1 = "Increase in sleep, in hours, under drug 1.",
    extra_drug_2 = "Increase in sleep, in hours, under drug 2.",
    paired_difference = "Drug 2 minus drug 1 increase in sleep for the same participant."
  ),
  toothgrowth = c(
    len = "Tooth length response.", supp = "Vitamin C delivery method: orange juice or ascorbic acid.",
    dose = "Vitamin C dose in milligrams per day.",
    dose_group = "Categorical label for the vitamin C dose."
  ),
  usarrests = c(
    state = "US state identifier.", Murder = "Murder and non-negligent manslaughter arrests per 100,000 residents.",
    Assault = "Assault arrests per 100,000 residents.",
    UrbanPop = "Percentage of the population living in urban areas.",
    Rape = "Rape arrests per 100,000 residents."
  ),
  warpbreaks = c(
    breaks = "Number of warp-thread breaks per loom.", wool = "Wool type (A or B).",
    tension = "Loom tension level (low, medium, or high)."
  ),
  titanic = c(
    passenger = "Synthetic passenger-row identifier added when expanding the published frequency table.",
    Class = "Passenger class: first, second, third, or crew.", Sex = "Recorded sex.",
    Age = "Age group: child or adult.", Survived = "Whether the passenger survived."
  ),
  school_absence = c(
    ethnicity = "Ethnic background classification: Aboriginal or non-Aboriginal.",
    sex = "Recorded sex.", age_group = "School form or primary-school age group.",
    learner = "Learner status: average or slow learner.",
    days_absent = "Number of days absent during the school year."
  ),
  insurance_claims = c(
    district = "District of residence; category 4 denotes major cities.",
    car_group = "Ordered car-engine-size group.", age_group = "Ordered age group of the insured.",
    holders = "Number of policyholders exposed to risk.", claims = "Number of insurance claims.",
    log_holders = "Natural log of exposed policyholders, supplied as the Poisson-model offset."
  ),
  gasoline_yield = c(
    yield = "Proportion of crude oil converted to gasoline.", gravity = "Crude-oil gravity in degrees API.",
    pressure = "Crude-oil vapour pressure in pounds-force per square inch.",
    temp10 = "Temperature in degrees Fahrenheit when 10% of the crude oil has vaporized.",
    temp = "Temperature in degrees Fahrenheit when all gasoline has vaporized.",
    batch = "Identifier for a unique set of controlled crude-oil conditions."
  ),
  theoph = c(
    subject = "Subject identifier for repeated concentration measurements.",
    weight_kg = "Subject body weight in kilograms.", dose_mg_per_kg = "Oral dose in milligrams per kilogram.",
    time_hours = "Hours since the oral dose.", concentration_mg_l = "Theophylline concentration in milligrams per litre."
  ),
  bfi = c(
    stats::setNames(
      as.character(psych::bfi.dictionary$Item[grepl("^[ACENO][1-5]$", row.names(psych::bfi.dictionary))]),
      row.names(psych::bfi.dictionary)[grepl("^[ACENO][1-5]$", row.names(psych::bfi.dictionary))]
    ),
    gender = "Recorded gender code: 1 = male, 2 = female.",
    education = "Education code from 1 (high school) to 5 (graduate degree).",
    age = "Age in years."
  ),
  nhanes_missing = c(
    age = "Age group: 20-39, 40-59, or 60 years and over.",
    bmi = "Body mass index in kilograms per square metre.",
    hyp = "Hypertension indicator: no or yes.", chl = "Total serum cholesterol in milligrams per decilitre."
  )
)
descriptions$nhanes_mice <- descriptions$nhanes_missing

variable_descriptions <- do.call(rbind, lapply(names(descriptions), function(id) {
  values <- descriptions[[id]]
  data.frame(id = id, variable = names(values), description = unname(values),
             stringsAsFactors = FALSE)
}))
for (id in catalog$id) {
  data_file <- catalog$file[catalog$id == id]
  variables <- if (grepl("[.]rds$", data_file)) names(readRDS(file.path(out, data_file))$data) else
    names(utils::read.csv(file.path(out, data_file), check.names = FALSE))
  described <- variable_descriptions$variable[variable_descriptions$id == id]
  if (!setequal(variables, described)) {
    stop("Variable-description coverage is incomplete for ", id, ": ",
         paste(setdiff(variables, described), collapse = ", "))
  }
}
utils::write.csv(variable_descriptions,
                 file.path(out, "variable_descriptions.csv"),
                 row.names = FALSE, na = "")

message("Built ", nrow(catalog), " LinkEDA statistical examples in ", out)
