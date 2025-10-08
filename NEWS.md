# qualcontrol (development version)

## New features
1. `check_befvekst` added, performing a special check of population growth cube vs the population cube. 

## Other changes
1. Updated internal data, including population weights. 

# qualcontrol 1.3.3

## New features
1. `check_friskvik()` now returns output to environment as well as writing .csv 
2. `recode_geo()` now reports on recoded 99-geocodes. 
3. `add_geoparams()` now manually sets GEOniv for recoded 99-codes, as missing values caused problems when flagging outlier
4. `split_kommuneniv()` ignores 99-kommunecodes when splitting small kommune into GEOniv = "k"
5. Implemented .parquet format, and standardized column types
6. Updated internal data (georecode, population info, etc.)
7. Moved internal dimension and value lists to options for easier updates

## Other changes
1. Deprecated modus argument, as NH is no longer used.
2. Fixed tests failing due to new file format and deleted source files

## Bugfix
1. `check_barometer()` now warns of untrustworthy results when barometer is not complete.
2. `get_plot_subset()` correctly identifies panels. Previously dimensions containing "..NA..", e.g. STONADSLENGDE, matched and was not included in the calculation

# qualcontrol 1.3.2

## Bugfix
1. Reading error in `check_friskvik()` fixed

# qualcontrol 1.3.1

## New features
1. `check_nevner_change_ungdata()` changes name to `check_nevner_change()`, aggregates all dims except GEO/AAR, and provide sumNEVNER_last in output. 
2. `archive_old_files()` handles cases when file is open
3. All functions producing a table output gains a save argument (default = T) to be able to not save output if not wanted
4. `readfiles()` now cleans environment, removing all existing cube objects when reading new files
5. `make_comparecube()` is now implemented in `readfiles()`
6. Added functions `check_coverage_geolevel()` and `check_coverage_geocode()` to assess the proportion of geo codes getting any and how much data.

## Bugfix
1. `check_censoring_timeseries()` now orders output when oldcube is not provided
2. All functions using comparecube as input now output a proper message when comparecube does not exist.
3. `check_comparecube()` now handles data files without TELLER/NEVNER
4. `check_friskvik()` rewritten and now functions properly

# qualcontrol 1.2.10

## Bugfix
1. strip.text of output plot are now left-aligned (@hanhel)

# qualcontrol 1.2.9

## Bugfix
1. make_comparecube correctly reflects differences in the any_diffs column. Previously it didn't indicate differences in SPVFLAGG. 

# qualcontrol 1.2.8

## New features

1. Generate new folder for table outputs
2. Save csv output for `comparecube_summary()`, `diffvals_summary()`, `compare_censoring()`, `compare_censoring_timeseries()`, `compare_geolevels()`, and `unknown_bydel()`
3. New function: `check_nevner_change_ungdata()`, comparing NEVNER to the previous and maximum NEVNER in the same strata. 


## Bugfixes

# qualcontrol 1.2.7

## New features

1. The any_diffs column in comparecube now only reflects any differences > 0.1 (requested by @hanhel)
2. `unknown_bydel()` no longer prints out a summary of number of complete strata, unless there are no complete strata in the file. 
3. The startup message now provide information on the population file used for weighting and categorizing geo-level, as well as geo-recode year (#6)

## Bugfixes

1. `unknown_bydel()` gave a warning due to different column classes of target columns. Now all targets are converted to double.

# qualcontrol 1.2.6

## New features

1. All old filedumps will be moved to archive when new files are requested

## Bugfixes

1. All plotting functions archive old files also when the function exits due to no data. 
2. Fixed test for `make_comparecube()` 

# qualcontrol 1.2.5

## Bugfixes

1. Increased the width of time series plots, to make room for large numbers on the x-axis without cropping the plot.
2. `plot_timeseries()` and `plot_timeseries_country()` handles files with no teller columns
3. In all functions using collapse::join, overid is changed from 0 to 2, to ensure matching on all columns. Early termination of the algorithm caused wrong matching in comparecube. 

# qualcontrol 1.2.4

## New features

1. `plot_boxplot()`, `plot_timeseries_country()`, `plot_timeseries_bydel()`, and `plot_diff_timetrends()` now prints the plot in the console.
2. `compare_geolevels()` prints a message if lower geographical level > higher geographical level, e.g. kommune > fylke. 

## Bugfixes

1. `add_changeval()` adds a small constant equal to half of the minimum non-zero value of the outlier variable. This prevents Inf-values for change variables in the flagged cube.
2. `plot_timeseries()` correctly shows GEO-codes in the plots
3. In plotting functions, `archive_old_files()` now matches on cube name and not cube file (including date tag) to archive all old files
4. Bug fixed in `compare_geolevels()` where missing data interfered with checking if any lower level > higher level. 
5. `is_valid_outcols()` only applied if data file is not censored on geo.
6. In all functions using collapse::join, overid is set to 0 to prevent overidentification of joins. 

# qualcontrol 1.2.3

## Bugfixes

1. Fixed bug where `diffvals_summary(byyear = TRUE)` only displayed first 10 years in long time series.

# qualcontrol 1.2.2

## Bugfixes

1. Fixed bug where `split_kommuneniv()` failed when GEOniv == "K" was not present in data.

# qualcontrol 1.2.1

## New features

1. add tests for friskvik and barometer 

## Bugfixes

1. `plot_diff_timetrends()` now archives old plots if existing. 
2. Fixed bug in `plot_timeseries_country` where no plot was produced for dimensions with > 9 categories. 

# qualcontrol 1.2.0

## New features

1. `plot_timeseries_country()` now generate a total plot, where all dimensions except AAR are aggregated (#1)
2. `check_friskvik()` function added (#2)
3. `check_barometer()` function added (#3)
4. `archive_old_plots()` renamed to `archive_old_files()`, for future implementation for file dumps. 

## Bugfixes
1. `plot_timeseries_bydel()` now handles situations where GEO and AAR are the only dimensions present (#4)

# qualcontrol 1.1.0

## New features

1. Added `diffvals_summary()`
2. `comparecube` now gets the `any_diffs` column indicating whether any diff column != 0
3. `comparecube` now handles expired and new dimensions

# qualcontrol 1.0.0

Implemented all functions from KHvalitetskontroll del 1 and del 2.
