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
