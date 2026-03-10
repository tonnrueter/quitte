test_that(
  'Test converting GLO back to World',
  {
    a <- magclass::new.magpie(cells_and_regions = c("GLO", "CHN"),
                              years = c(2010, 2020),
                              fill = 1,
                              names = c("VAR1", "VAR2"),
                              sets = c("region", "year", "variable"))

    quitteReport <- useWorld(as.quitte(a))
    expect_true("World" %in% quitteReport$region)
    expect_false("GLO" %in% quitteReport$region)
  }
)