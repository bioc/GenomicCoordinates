# Additional tests for space handling and coercion edge cases
# Testing edge cases that may not be fully covered

library(testthat)
library(GenomicRanges)
library(IRanges)
library(InteractionSet)

test_that("Space normalization works correctly", {
    
    # Test various spacing patterns
    spacing_variants <- list(
        # Single spaces
        list(input = "chr1 1000 2000", expected_seqname = "chr1", expected_start = 1000, expected_end = 2000),
        # Multiple spaces
        list(input = "chr1    1000     2000", expected_seqname = "chr1", expected_start = 1000, expected_end = 2000),
        # Tabs mixed with spaces
        list(input = "chr1\t1000\t\t2000", expected_seqname = "chr1", expected_start = 1000, expected_end = 2000),
        # Leading/trailing spaces
        list(input = "  chr1 1000 2000  ", expected_seqname = "chr1", expected_start = 1000, expected_end = 2000)
    )
    
    for (variant in spacing_variants) {
        result <- GenomicCoordinates(variant$input)
        expect_s4_class(result, "GRanges")
        expect_equal(as.character(seqnames(result)), variant$expected_seqname)
        expect_equal(start(result), variant$expected_start)
        expect_equal(end(result), variant$expected_end)
    }
})

test_that("Chromosome name parsing with spaces works", {
    
    # Test chromosome names that contain spaces  
    chr_with_spaces <- c(
        "chr I:1000-2000",
        "chr II:1000-2000", 
        "chromosome 1:1000-2000",
        "linkage group I:1000-2000",
        "scaffold 123:1000-2000"
    )
    
    expected_names <- c(
        "chr I",
        "chr II", 
        "chromosome 1",
        "linkage group I",
        "scaffold 123"
    )
    
    for (i in seq_along(chr_with_spaces)) {
        result <- GenomicCoordinates(chr_with_spaces[i])
        expect_s4_class(result, "GRanges")
        expect_equal(as.character(seqnames(result)), expected_names[i])
        expect_equal(start(result), 1000)
        expect_equal(end(result), 2000)
    }
})

test_that("Mixed format detection edge cases", {
    
    # Test cases where IRanges detection might be ambiguous
    ambiguous_cases <- list(
        # Should be IRanges (pure numeric)
        list(input = "123-456", expected_class = "IRanges"),
        list(input = "1 200", expected_class = "IRanges"),
        list(input = "1000", expected_class = "IRanges"),
        
        # Should NOT be IRanges (has chromosome info)
        list(input = "chr123:456-789", expected_class = "GRanges"),
        list(input = "scaffold1:100-200", expected_class = "GRanges"),
        list(input = "1p:100-200", expected_class = "GRanges"),  # Chromosome arm notation
        list(input = "I:100-200", expected_class = "GRanges")    # Roman numeral chromosome
    )
    
    for (case in ambiguous_cases) {
        result <- GenomicCoordinates(case$input)
        expect_true(inherits(result, case$expected_class),
                   info = paste("Input:", case$input, "Expected:", case$expected_class, "Got:", class(result)[1]))
    }
})

test_that("Large coordinate handling", {
    
    # Test very large coordinates
    large_coords <- c(
        "chr1:1000000000-2000000000",           # 1-2 billion
        "chr1:123,456,789-987,654,321",         # With commas
        "chr1 999999999 1000000000",            # Space-delimited
        "chr1:999,999,999"                      # Single large position
    )
    
    expected_starts <- c(1000000000, 123456789, 999999999, 999999999)
    expected_ends <- c(2000000000, 987654321, 1000000000, 999999999)
    
    for (i in seq_along(large_coords)) {
        result <- GenomicCoordinates(large_coords[i])
        expect_true(inherits(result, c("GRanges", "GPos")))
        
        if (inherits(result, "GRanges")) {
            expect_equal(start(result), expected_starts[i])
            expect_equal(end(result), expected_ends[i])
        } else if (inherits(result, "GPos")) {
            expect_equal(pos(result), expected_starts[i])
        }
    }
})

test_that("Vector consistency across different input types", {
    
    # Test that mixed vectors maintain consistency
    mixed_vector_tests <- list(
        # All should become GRanges (mixed single/range)
        list(
            input = c("chr1:1000", "chr2:2000-3000"),
            expected_class = "GRanges",
            expected_length = 2
        ),
        
        # All should become GPos (all single)
        list(
            input = c("chr1:1000", "chr2:2000", "chr3:3000"),
            expected_class = "GPos", 
            expected_length = 3
        ),
        
        # All should become GRanges (all ranges)
        list(
            input = c("chr1:1000-2000", "chr2:3000-4000"),
            expected_class = "GRanges",
            expected_length = 2
        ),
        
        # All should become IRanges (no chromosome info)
        list(
            input = c("1000-2000", "3000-4000"),
            expected_class = "IRanges",
            expected_length = 2
        )
    )
    
    for (test in mixed_vector_tests) {
        result <- GenomicCoordinates(test$input)
        expect_s4_class(result, test$expected_class)
        expect_equal(length(result), test$expected_length)
    }
})

test_that("Unusual separator handling", {
    
    # Test various separators in coordinates (these should mostly fail gracefully)
    unusual_separators <- c(
        "chr1:1000..2000",    # Double dot
        "chr1:1000_2000",     # Underscore  
        "chr1:1000~2000",     # Tilde
        "chr1:1000|2000",     # Pipe (might be confused with interactions)
        "chr1:1000;2000"      # Semicolon
    )
    
    for (sep_test in unusual_separators) {
        # These should either work (if parser is flexible) or error gracefully
        result <- tryCatch({
            GenomicCoordinates(sep_test)
        }, error = function(e) {
            "error"
        })
        
        # Acceptable outcomes: valid object or error
        expect_true(
            inherits(result, c("GRanges", "GPos", "IRanges", "GInteractions")) || result == "error",
            info = paste("Testing unusual separator:", sep_test)
        )
    }
})

test_that("Interaction parsing with unusual chromosome names", {
    
    # Test interactions with complex chromosome names
    complex_interactions <- c(
        "chr I:1-10|chr II:20-30",
        "scaffold_123:100-200|scaffold_456:300-400",
        "GL000001.1:1000-2000|NC_000002.1:3000-4000",
        "HLA-DRB1*15:01:1000-2000|HLA-DQB1*06:02:3000-4000"
    )
    
    for (interaction in complex_interactions) {
        result <- GenomicCoordinates(interaction)
        expect_s4_class(result, "GInteractions")
        expect_equal(length(result), 1)
        
        # Check that both anchors are valid
        anchor1 <- anchors(result, "first")
        anchor2 <- anchors(result, "second")
        expect_s4_class(anchor1, "GRanges")
        expect_s4_class(anchor2, "GRanges")
        expect_equal(length(anchor1), 1)
        expect_equal(length(anchor2), 1)
    }
})

test_that("Force class with unusual chromosome names", {
    
    unusual_input <- "chr I:1000-2000"
    
    # Test all force_class options
    force_classes <- c("GRanges", "GPos", "IRanges", "GInteractions")
    
    for (force_class in force_classes) {
        result <- GenomicCoordinates(unusual_input, force_class = force_class)
        expect_s4_class(result, force_class)
        expect_equal(length(result), 1)
    }
})

test_that("detect_genomic_class edge cases", {
    
    # Test edge cases for detect_genomic_class function
    edge_cases <- c(
        "",                          # Empty string
        " ",                         # Whitespace only
        "chr1:",                     # Incomplete format
        "chr1:1000-",               # Incomplete range
        "chr1:-2000",               # Incomplete range (other side)
        "|",                        # Just separator
        "chr1:1000|",               # Incomplete interaction
        "|chr2:2000",               # Incomplete interaction (other side)
        "123",                      # Just a number
        "chr",                      # Just chromosome prefix
        "1000-2000-3000"           # Too many parts
    )
    
    for (case in edge_cases) {
        # detect_genomic_class should handle these gracefully
        result <- tryCatch({
            detect_genomic_class(case)
        }, error = function(e) {
            "error"
        })
        
        # Should either return a valid class or error gracefully
        expect_true(
            result %in% c("GRanges", "GPos", "IRanges", "GInteractions") || result == "error",
            info = paste("Testing edge case:", case)
        )
    }
})
