# qualcontrol (development version)
* Bug fixed in `compare_geolevels()` where missing data interfered with checking if any lower level > higher level. 
* `is_valid_outcols()` only applied if data file is not censored on geo.
* sets overid = 0 in all functions using collapse::join, to prevent overidentification of joins. 
* `plot_boxplot()`, `plot_timeseries_country()`, `plot_timeseries_bydel()`, and `plot_diff_timetrends()` now prints the plot in the console.
* `compare_geolevels()` prints a message if lower geographical level > higher geographical level, e.g. kommune > fylke. 

# qualcontrol 1.2.3
* Fixed bug where `diffvals_summary(byyear = TRUE)` only displayed first 10 years in long time series.

# qualcontrol 1.2.2
* Fixed bug where `split_kommuneniv()` failed when GEOniv == "K" was not present in data.

# qualcontrol 1.2.1
* add tests for friskvik and barometer 
* `plot_diff_timetrends()` now archives old plots if existing. 
* Fixed bug in `plot_timeseries_country` where no plot was produced for dimensions with > 9 categories. 

# qualcontrol 1.2.0
* `plot_timeseries_country()` now generate a total plot, where all dimensions except AAR are aggregated (#1)
* `check_friskvik()` function added (#2)
* `check_barometer()` function added (#3)
* `plot_timeseries_bydel()` now handles situations where GEO and AAR are the only dimensions present (#4)
* `archive_old_plots()` renamed to `archive_old_files()`, for future implementation for file dumps. 

# qualcontrol 1.1.0
* Added `diffvals_summary()`
* `comparecube` now gets the `any_diffs` column indicating whether any diff column != 0
* `comparecube` now handles expired and new dimensions

# qualcontrol 1.0.0

* Implemented all functions from KHvalitetskontroll del 1 and del 2.
