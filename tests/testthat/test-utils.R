library(testthat)

test_that("Internal utility functions work correctly", {
    
    # Test .clean_numeric_string
    expect_equal(GenomicCoordinates:::.clean_numeric_string("1,000"), "1000")
    expect_equal(GenomicCoordinates:::.clean_numeric_string("1,000,000"), "1000000")
    expect_equal(GenomicCoordinates:::.clean_numeric_string("1000"), "1000")
    
    # Test .parse_coordinates 
    coords <- GenomicCoordinates:::.parse_coordinates("1000-2000")
    expect_equal(coords$start, 1000)
    expect_equal(coords$end, 2000)
    expect_null(coords$single)
    
    coords <- GenomicCoordinates:::.parse_coordinates("1000")
    expect_equal(coords$start, 1000)
    expect_equal(coords$end, 1000) 
    expect_true(coords$single)
    
    coords <- GenomicCoordinates:::.parse_coordinates("1,000-2,000")
    expect_equal(coords$start, 1000)
    expect_equal(coords$end, 2000)
    
    coords <- GenomicCoordinates:::.parse_coordinates("1000..2000")
    expect_equal(coords$start, 1000)
    expect_equal(coords$end, 2000)
    
    coords <- GenomicCoordinates:::.parse_coordinates("1000 2000")
    expect_equal(coords$start, 1000)
    expect_equal(coords$end, 2000)
})

test_that("Genomic string parsing works", {
    
    # Test standard format
    parsed <- GenomicCoordinates:::.parse_genomic_string("chr1:1000-2000")
    expect_equal(parsed$seqnames, "chr1")
    expect_equal(parsed$start, 1000)
    expect_equal(parsed$end, 2000)
    expect_equal(parsed$strand, "*")
    expect_null(parsed$single)
    
    # Test with strand
    parsed <- GenomicCoordinates:::.parse_genomic_string("chr1:1000-2000:+")
    expect_equal(parsed$strand, "+")
    
    # Test single position
    parsed <- GenomicCoordinates:::.parse_genomic_string("chr1:1000")
    expect_equal(parsed$start, 1000)
    expect_equal(parsed$end, 1000)
    expect_true(parsed$single)
    
    # Test space-delimited
    parsed <- GenomicCoordinates:::.parse_genomic_string("chr1 1000 2000")
    expect_equal(parsed$seqnames, "chr1")
    expect_equal(parsed$start, 1000)
    expect_equal(parsed$end, 2000)
})

test_that("GInteractions string parsing works", {
    
    parsed <- GenomicCoordinates:::.parse_ginteractions_string("chr1:1-10|chr2:20-30")
    expect_equal(parsed$anchor1$seqnames, "chr1")
    expect_equal(parsed$anchor1$start, 1)
    expect_equal(parsed$anchor1$end, 10)
    expect_equal(parsed$anchor2$seqnames, "chr2")
    expect_equal(parsed$anchor2$start, 20)
    expect_equal(parsed$anchor2$end, 30)
})

test_that("Special format handling works", {
    
    # Test single position detection
    expect_true(GenomicCoordinates:::.is_single_position("chr1:1000"))
    expect_true(GenomicCoordinates:::.is_single_position("chr1:1,000"))
    expect_false(GenomicCoordinates:::.is_single_position("chr1:1000-2000"))
    
    # Test special format parsing
    result <- GenomicCoordinates:::.handle_special_formats("chr1:1,234")
    expect_equal(result$seqnames, "chr1")
    expect_equal(result$start, 1234)
    expect_equal(result$end, 1234)
    expect_true(result$single)
})

test_that("Error handling in utility functions", {
    
    # Invalid coordinate strings should throw errors
    expect_error(GenomicCoordinates:::.parse_coordinates("abc-def"))
    expect_error(GenomicCoordinates:::.parse_coordinates(""))
    expect_error(GenomicCoordinates:::.parse_genomic_string(""))
})
