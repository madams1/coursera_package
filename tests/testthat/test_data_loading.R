context("Data Loading")

fars_2015_file <- system.file("extdata", make_filename(2015), package = "courserapackage")

test_that(
    "Data can be loaded properly",
    {
        expect_true(is.data.frame(fars_read(fars_2015_file)))
    }
)
