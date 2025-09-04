# Error handling and boundary condition tests for GenomicCoordinates.R
# These tests ensure robust error handling and edge case coverage

library(testthat)
library(GenomicRanges)
library(IRanges)
library(InteractionSet)

# Source test cases
source("test-cases.R")

test_that("GenomicCoordinates error handling is robust", {
    
    for (input in invalid_inputs) {
        if (is.null(input) || (length(input) == 1 && is.na(input))) {
            # NULL and NA should error
            expect_error(GenomicCoordinates(input), info = paste("Testing invalid input:", deparse(input)))
        } else if (length(input) == 0) {
            # Empty character vector should work (return empty object)
            result <- GenomicCoordinates(input)
            expect_s4_class(result, "GRanges")
            expect_equal(length(result), 0)
        } else {
            # Other invalid inputs should error gracefully
            expect_error(GenomicCoordinates(input), info = paste("Invalid genomic string format:", input))
        }
    }
})

test_that("Boundary coordinates are handled correctly", {
    
    # Test boundary cases for coordinates
    boundary_cases <- list(
        # Single base positions
        list(input = "chr1:1", expected_class = "GPos", check_pos = 1),
        list(input = "chr1:1-1", expected_class = "GRanges", check_start = 1, check_end = 1),
        
        # Zero coordinates (valid in 0-based systems)
        list(input = "chr1:0", expected_class = "GPos", check_pos = 0),
        list(input = "chr1:0-0", expected_class = "GRanges", check_start = 0, check_end = 0),
        list(input = "chr1:0-1", expected_class = "GRanges", check_start = 0, check_end = 1),
        
        # Large coordinates
        list(input = "chr1:2147483647", expected_class = "GPos", check_pos = 2147483647), # Near 32-bit max
        list(input = "chr1:1000000000-2000000000", expected_class = "GRanges", check_start = 1000000000, check_end = 2000000000)
    )
    
    for (case in boundary_cases) {
        result <- GenomicCoordinates(case$input)
        expect_s4_class(result, case$expected_class)
        
        if (case$expected_class == "GPos") {
            expect_equal(pos(result), case$check_pos)
        } else if (case$expected_class == "GRanges") {
            expect_equal(start(result), case$check_start)
            expect_equal(end(result), case$check_end)
        }
    }
})

test_that("Memory and performance edge cases", {
    
    # Test with very long vectors
    long_vector <- rep("chr1:1000-2000", 1000)
    result <- GenomicCoordinates(long_vector)
    expect_s4_class(result, "GRanges")
    expect_equal(length(result), 1000)
    
    # Test with very long chromosome names
    very_long_chr <- paste0(rep("a", 100), collapse = "")
    long_chr_input <- paste0(very_long_chr, ":1000-2000")
    result <- GenomicCoordinates(long_chr_input)
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), very_long_chr)
    
    # Test with many unique chromosome names
    many_chrs <- paste0("chr", 1:100, ":1000-2000")
    result <- GenomicCoordinates(many_chrs)
    expect_s4_class(result, "GRanges")
    expect_equal(length(result), 100)
    expect_equal(length(unique(as.character(seqnames(result)))), 100)
})

test_that("Special character handling in chromosome names", {
    
    for (chr_test in special_char_chrs) {
        result <- GenomicCoordinates(chr_test)
        expect_s4_class(result, "GRanges")
        expect_equal(length(result), 1)
    }

    expect_error(GenomicCoordinates("chr1|2:1000-2000"), "Invalid genomic string format: chr1")

})

test_that("Whitespace and control character edge cases", {
    
    for (ws_test in whitespace_cases) {
        result <- GenomicCoordinates(ws_test)
        expect_true(inherits(result, c("GRanges", "GPos", "IRanges")))
        expect_equal(length(result), 1)
    }
})

test_that("Force class error conditions", {
    
    test_input <- "chr1:1000-2000"
    
    result <- GenomicCoordinates(test_input, force_class = "GRanges")
    expect_s4_class(result, "GRanges")
    result <- GenomicCoordinates(test_input)
    expect_s4_class(result, "GRanges")

    for (force_class in invalid_force_classes) {
        if (is.null(force_class)) {
            # NULL should work (no forcing)
            result <- GenomicCoordinates(test_input, force_class = force_class)
            expect_s4_class(result, "GRanges")
        } else {
            # Invalid force_class should error
            expect_error(
                GenomicCoordinates(test_input, force_class = force_class),
                info = paste("Testing invalid force_class:", deparse(force_class))
            )
        }
    }
})

test_that("Mixed valid and invalid inputs in vectors", {
    
    for (mixed_vec in mixed_vectors) {
        # These should error because of the invalid entries
        expect_error(GenomicCoordinates(mixed_vec))
    }
})

test_that("Numeric input edge cases", {
    
    for (num_input in numeric_inputs) {
        expect_error(GenomicCoordinates(num_input))
    }
})

test_that("Factor input handling", {
    
    # Test factor input (should be converted to character)
    factor_input <- factor(c("chr1:1000-2000", "chr2:3000-4000"))
    result <- GenomicCoordinates(factor_input)
    expect_s4_class(result, "GRanges")
    expect_equal(length(result), 2)
    
    # Test factor with invalid levels
    invalid_factor <- factor(c("chr1:1000-2000", "invalid_format"))
    expect_error(GenomicCoordinates(invalid_factor))
})

test_that("Very large numbers cause appropriate behavior", {
    
    for (large_coord in very_large_coords) {
        expect_error(GenomicCoordinates(large_coord), regexp = "each range must have a start that is |cannot contain NAs")
    }
})
