test_that(
    'write .mif files',
    {
        f <- tempfile()
        quitte_example_data %>%
            write.mif(f)

        expect_equal(
            object = read.quitte(f) %>%
              character.data.frame() %>%
              arrange(!!sym('model'), !!sym('scenario'), !!sym('region'),
                      !!sym('variable'), !!sym('unit'), !!sym('period')),
            expected = quitte_example_data %>%
              character.data.frame() %>%
              arrange(!!sym('model'), !!sym('scenario'), !!sym('region'),
                      !!sym('variable'), !!sym('unit'), !!sym('period'))
        )
    })

test_that(
    'write .mif file with comment header',
    {
        f <- tempfile()
        x <- read.quitte(system.file('extdata', 'comment_header.mif',
                                     package = 'quitte'))
        write.mif(x = x, path = f, comment_header = attr(x, 'comment_header'))

        expect_equal(
            object = read_lines(f),
            expected = read_lines(system.file('extdata', 'comment_header.mif',
                                              package = 'quitte')))
    })

test_that(
    'write .mif file passing empty data',
    {
        f <- tempfile()
        expect_warning(write.mif(filter(quitte_example_data, model == "x"), f),
                       "Writing empty data frame")
    })

test_that(
    'write .mif file with comment header',
    {
      f <- tempfile()
      x <- read.quitte(system.file('extdata', 'comment_header.mif',
                                   package = 'quitte'))
      write.mif(x = x, path = f, comment_header = attr(x, 'comment_header'))

      expect_equal(
          object = read_lines(f),
          expected = read_lines(system.file('extdata', 'comment_header.mif',
                                            package = 'quitte')))
    })

test_that(
    'write IAMC format',
    {
        f <- tempfile(fileext = '.csv')
        write.IAMCcsv(quitte_example_dataAR6, f)
        expect_equal(object = read.snapshot(f),
                     expected = quitte_example_dataAR6 %>%
                         filter(!is.na(.data$value)))
    })

test_that(
    'write.mif(append = TRUE) does not duplicate data',
    {
        f <- tempfile(fileext = '.mif')

        d <- quitte_example_data %>%
            filter(first(scenario) == scenario,
                   first(variable) == variable,
                   first(period) == period) %>%
            droplevels()

        d %>%
            head(n = floor(nrow(d) / 2)) %>%
            write.mif(f, append = FALSE)

        d %>%
            tail(n = ceiling(nrow(d) / 2)) %>%
            write.mif(f, append = TRUE)

        expect_equal(object = read.quitte(f),
                     expected = d)
    })

test_that(
    'write.mif(append = FALSE) overwrites the file',
    {
        f <- tempfile(fileext = '.mif')

        d <- quitte_example_data %>%
            filter(first(scenario) == scenario,
                   first(variable) == variable,
                   first(period) == period)

        d %>%
            head(n = floor(nrow(d) / 2)) %>%
            write.mif(f, append = FALSE)

        d2 <- d %>%
            tail(n = ceiling(nrow(d) / 2)) %>%
            droplevels()

        d2 %>%
            write.mif(f, append = FALSE)

        expect_equal(object = read.quitte(f),
                     expected = d2)
    })
