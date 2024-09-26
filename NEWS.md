# qualcontrol 1.2.0
* `check_friskvik()` function added
* `check_barometer()` function added
* `archive_old_plots()` renamed to `archive_old_files()`, and implemented also for file dumps
* `plot_timeseries_bydel()` now handles situations where GEO and AAR are the only dimensions present (#4)
* `plot_timeseries_country()` now generate a total plot, where all dimensions except AAR are aggregated (#1)

# qualcontrol 1.1.0
* Added `diffvals_summary()`
* `comparecube` now gets the `any_diffs` column indicating whether any diff column != 0
* `comparecube` now handles expired and new dimensions

# qualcontrol 1.0.0

* Implemented all functions from KHvalitetskontroll del 1 and del 2.
