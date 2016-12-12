## ---- message=FALSE------------------------------------------------------
# devtools::install_github("madams1/courserapackage")
require(courserapackage)
require(dplyr)
fars_2015_file <- system.file("extdata", make_filename(2015), package = "courserapackage")
fars_path <- dirname(fars_2015_file)
fars_2015_data <- fars_read(fars_2015_file)

## ------------------------------------------------------------------------
fars_2015_data

## ------------------------------------------------------------------------
fars_summarize_years(2013:2015, path = fars_path)

## ---- fig.show='hold', message=FALSE-------------------------------------
require(maps)
fars_map_state(6, 2015, path = fars_path)

