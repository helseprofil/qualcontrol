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
