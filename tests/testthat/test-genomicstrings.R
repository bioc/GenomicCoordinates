library(testthat)
library(GenomicRanges)
library(IRanges)
library(InteractionSet)

# Source test cases
source("test-cases.R")

test_that("GenomicCoordinates main function auto-detection works", {
    
    # Test GRanges auto-detection
    result <- GenomicCoordinates("chr1:1000-2000")
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "chr1")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    
    # Test GPos auto-detection  
    result <- GenomicCoordinates("chr1:1000")
    expect_s4_class(result, "GPos")
    expect_equal(as.character(seqnames(result)), "chr1")
    expect_equal(pos(result), 1000)
    
    # Test GInteractions auto-detection
    result <- GenomicCoordinates("chr1:1-10|chr2:20-30")
    expect_s4_class(result, "GInteractions")
    expect_equal(length(result), 1)
    
    # Test IRanges auto-detection
    result <- GenomicCoordinates("1000-2000")
    expect_s4_class(result, "IRanges")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
})

test_that("GenomicCoordinates handles irregular spacing", {
    
    # Test GRanges with irregular spacing
    result <- GenomicCoordinates("chr1  1000   2000")
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "chr1")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    
    # Test GPos with spacing
    result <- GenomicCoordinates("chr1  1000")
    expect_s4_class(result, "GPos")
    expect_equal(as.character(seqnames(result)), "chr1")
    expect_equal(pos(result), 1000)
    
    # Test mixed format with strand and spacing
    result <- GenomicCoordinates("chr1  1000   2000:+")
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(strand(result)), "+")
    
    # Test GInteractions with spacing
    result <- GenomicCoordinates("chr1: 1-10  |  chr2: 20-30")
    expect_s4_class(result, "GInteractions")
    expect_equal(length(result), 1)
    
    # Test IRanges with spacing
    result <- GenomicCoordinates("1000   2000")
    expect_s4_class(result, "IRanges")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
})

test_that("Force class parameter works", {
    
    # Force GRanges for single position
    result <- GenomicCoordinates("chr1:1000", force_class = "GRanges")
    expect_s4_class(result, "GRanges")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 1000)
    
    # Force GPos for range
    result <- GenomicCoordinates("chr1:1000-2000", force_class = "GPos")
    expect_s4_class(result, "GPos")
    
    # Force IRanges
    result <- GenomicCoordinates("chr1:1000-2000", force_class = "IRanges")
    expect_s4_class(result, "IRanges")
})

test_that("detect_genomic_class function works", {
    
    classes <- detect_genomic_class(c(
        "chr1:1000-2000",      # GRanges
        "chr1:1000",           # GPos
        "chr1:1-10|chr2:20-30", # GInteractions
        "1000-2000"            # IRanges
    ))
    
    expect_equal(classes, c("GRanges", "GPos", "GInteractions", "IRanges"))
})

test_that("GRanges coercion works for standard formats", {
    
    for (test_string in granges_standard) {
        result <- as_granges(test_string)
        expect_s4_class(result, "GRanges")
        expect_equal(length(result), 1)
    }
})

test_that("GRanges coercion works for comma-separated formats", {
    
    # Test comma-separated numbers
    result <- as_granges("chr1:1,000-2,000")
    expect_s4_class(result, "GRanges")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    
    # Test large numbers with commas
    result <- as_granges("chr1:1,000,000-2,000,000") 
    expect_equal(start(result), 1000000)
    expect_equal(end(result), 2000000)
    
    for (test_string in granges_comma) {
        result <- as_granges(test_string)
        expect_s4_class(result, "GRanges")
        expect_equal(length(result), 1)
        expect_true(start(result) <= end(result))
    }
})

test_that("GRanges coercion works for space-delimited formats", {
    
    result <- as_granges("chr1 1000 2000")
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "chr1")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    
    for (test_string in granges_space) {
        result <- as_granges(test_string)
        expect_s4_class(result, "GRanges")
        expect_equal(length(result), 1)
    }
})

test_that("GRanges coercion works for irregular spacing", {
    
    # Test space-delimited with irregular spacing
    result <- as_granges("chr1  1000   2000")
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "chr1")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    
    # Test mixed space-colon format with strand
    result <- as_granges("chr1  1000   2000:+")
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "chr1")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    expect_equal(as.character(strand(result)), "+")
    
    for (test_string in granges_space_irregular) {
        result <- as_granges(test_string)
        expect_s4_class(result, "GRanges")
        expect_equal(length(result), 1)
        expect_true(start(result) <= end(result))
    }
    
    for (test_string in granges_mixed_spacing) {
        result <- as_granges(test_string)
        expect_s4_class(result, "GRanges")
        expect_equal(length(result), 1)
        expect_true(start(result) <= end(result))
    }
})

test_that("GRanges strand handling works", {
    
    result <- as_granges("chr1:1000-2000:+")
    expect_equal(as.character(strand(result)), "+")
    
    result <- as_granges("chr1:1000-2000:-") 
    expect_equal(as.character(strand(result)), "-")
    
    result <- as_granges("chr1:1000-2000:*")
    expect_equal(as.character(strand(result)), "*")
})

test_that("GPos coercion works", {
    
    # Standard single position
    result <- as_gpos("chr1:1000")
    expect_s4_class(result, "GPos")
    expect_equal(as.character(seqnames(result)), "chr1")
    expect_equal(pos(result), 1000)
    
    # Comma-separated single position
    result <- as_gpos("chr1:1,000")
    expect_equal(pos(result), 1000)
    
    # With strand
    result <- as_gpos("chr1:1000:+")
    expect_equal(as.character(strand(result)), "+")
    
    for (test_string in gpos_standard) {
        result <- as_gpos(test_string)
        expect_s4_class(result, "GPos")
        expect_equal(length(result), 1)
    }
    
    for (test_string in gpos_comma) {
        result <- as_gpos(test_string)
        expect_s4_class(result, "GPos")
        expect_equal(length(result), 1)
    }
})

test_that("GPos coercion works with irregular spacing", {
    
    # Test space-separated single position with irregular spacing
    result <- as_gpos("chr1  1000")
    expect_s4_class(result, "GPos")
    expect_equal(as.character(seqnames(result)), "chr1")
    expect_equal(pos(result), 1000)
    
    # Test colon format with spaces
    result <- as_gpos("chr1: 1000")
    expect_s4_class(result, "GPos")
    expect_equal(pos(result), 1000)
    
    for (test_string in gpos_space_irregular) {
        result <- as_gpos(test_string)
        expect_s4_class(result, "GPos")
        expect_equal(length(result), 1)
    }
    
    for (test_string in gpos_mixed_spacing) {
        result <- as_gpos(test_string)
        expect_s4_class(result, "GPos")
        expect_equal(length(result), 1)
    }
})

test_that("GInteractions coercion works", {
    
    # Basic interaction
    result <- as_ginteractions("chr1:1-10|chr2:20-30")
    expect_s4_class(result, "GInteractions")
    expect_equal(length(result), 1)
    
    # Check anchors
    anchor1_gr <- anchors(result, "first")
    anchor2_gr <- anchors(result, "second") 
    expect_equal(as.character(seqnames(anchor1_gr)), "chr1")
    expect_equal(as.character(seqnames(anchor2_gr)), "chr2")
    expect_equal(start(anchor1_gr), 1)
    expect_equal(end(anchor1_gr), 10)
    expect_equal(start(anchor2_gr), 20)
    expect_equal(end(anchor2_gr), 30)
    
    for (test_string in ginteractions_standard) {
        result <- as_ginteractions(test_string)
        expect_s4_class(result, "GInteractions")
        expect_equal(length(result), 1)
    }
    
    for (test_string in ginteractions_comma) {
        result <- as_ginteractions(test_string)
        expect_s4_class(result, "GInteractions")
        expect_equal(length(result), 1)
    }
})

test_that("GInteractions coercion works with irregular spacing", {
    
    # Test irregular spacing around pipe separator
    result <- as_ginteractions("chr1: 1-10  |  chr2: 20-30")
    expect_s4_class(result, "GInteractions")
    expect_equal(length(result), 1)
    
    # Check that anchors are parsed correctly despite spacing
    anchor1_gr <- anchors(result, "first")
    anchor2_gr <- anchors(result, "second")
    expect_equal(as.character(seqnames(anchor1_gr)), "chr1")
    expect_equal(as.character(seqnames(anchor2_gr)), "chr2")
    expect_equal(start(anchor1_gr), 1)
    expect_equal(end(anchor1_gr), 10)
    expect_equal(start(anchor2_gr), 20)
    expect_equal(end(anchor2_gr), 30)
    
    # Test space-delimited coordinates with pipe
    result <- as_ginteractions("chr1  1000   2000 | chr2  3000   4000")
    expect_s4_class(result, "GInteractions")
    expect_equal(length(result), 1)
    
    for (test_string in ginteractions_spacing) {
        result <- as_ginteractions(test_string)
        expect_s4_class(result, "GInteractions")
        expect_equal(length(result), 1)
    }
})

test_that("IRanges coercion works", {
    
    # Basic range
    result <- as_iranges("1000-2000")
    expect_s4_class(result, "IRanges")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    
    # Comma-separated
    result <- as_iranges("1,000-2,000")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    
    # Space-separated 
    result <- as_iranges("1000 2000")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    
    for (test_string in iranges_standard) {
        result <- as_iranges(test_string)
        expect_s4_class(result, "IRanges")
        expect_equal(length(result), 1)
    }
    
    for (test_string in iranges_comma) {
        result <- as_iranges(test_string)
        expect_s4_class(result, "IRanges")
        expect_equal(length(result), 1)
    }
})

test_that("Multiple element vectors work", {
    
    # Multiple GRanges
    result <- as_granges(multi_element_cases$granges)
    expect_s4_class(result, "GRanges")
    expect_equal(length(result), 3)
    expect_equal(as.character(seqnames(result)), c("chr1", "chr2", "chrX"))
    
    # Multiple GPos
    result <- as_gpos(multi_element_cases$gpos)
    expect_s4_class(result, "GPos")
    expect_equal(length(result), 3)
    
    # Multiple IRanges
    result <- as_iranges(multi_element_cases$iranges)
    expect_s4_class(result, "IRanges")
    expect_equal(length(result), 3)
})

test_that("Edge cases are handled correctly", {
    
    # Single base range
    result <- as_granges("chr1:1000-1000")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 1000)
    expect_equal(width(result), 1)
    
    # Zero start (should work)
    result <- as_granges("chr1:0-100")
    expect_equal(start(result), 0)
    expect_equal(end(result), 100)
    
    for (test_string in edge_cases) {
        result <- as_granges(test_string)
        expect_s4_class(result, "GRanges")
        expect_equal(length(result), 1)
    }
})

test_that("Empty inputs are handled", {
    
    expect_s4_class(GenomicCoordinates(character(0)), "GRanges")
    expect_s4_class(as_granges(character(0)), "GRanges")
    expect_s4_class(as_gpos(character(0)), "GPos")
    expect_s4_class(as_iranges(character(0)), "IRanges")
    expect_s4_class(as_ginteractions(character(0)), "GInteractions")
    
    expect_equal(length(GenomicCoordinates(character(0))), 0)
})

test_that("Error cases fail appropriately", {
    
    # These should throw errors
    expect_error(as_granges("invalid_string"))
    expect_error(as_granges("chr1:"))
    expect_error(as_granges(":1000-2000"))
    expect_error(as_granges("chr1:abc-def"))
})




test_that("GenomicCoordinates handles unusual chromosome names", {
    
    # Chromosome names with spaces
    result <- GenomicCoordinates("chr I:1000-2000")
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "chr I")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    
    # Chromosome names with underscores
    result <- GenomicCoordinates("chr_a:1000-2000")
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "chr_a")
    
    # Chromosome names with dashes
    result <- GenomicCoordinates("chr-01:1000-2000")
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "chr-01")
    
    for (test_string in test_chr_names) {
        result <- GenomicCoordinates(test_string)
        expect_s4_class(result, "GRanges")
        expect_equal(length(result), 1)
        expect_true(start(result) <= end(result))
    }
})

test_that("GenomicCoordinates handles unusual chromosome names in GPos", {
    
    for (test_string in gpos_unusual_chr) {
        result <- GenomicCoordinates(test_string)
        expect_s4_class(result, "GPos")
        expect_equal(length(result), 1)
    }
    
    # Test with detect_genomic_class
    classes <- detect_genomic_class(gpos_unusual_chr)
    expect_true(all(classes == "GPos"))
})

test_that("GenomicCoordinates handles unusual chromosome names in GInteractions", {
    
    for (test_string in gint_unusual_chr) {
        result <- GenomicCoordinates(test_string)
        expect_s4_class(result, "GInteractions")
        expect_equal(length(result), 1)
    }
    
    # Test with detect_genomic_class
    classes <- detect_genomic_class(gint_unusual_chr)
    expect_true(all(classes == "GInteractions"))
})

test_that("force_class parameter works with edge cases", {
    
    # Force IRanges for genomic coordinates
    result <- GenomicCoordinates("chr1:1000-2000", force_class = "IRanges")
    expect_s4_class(result, "IRanges")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 2000)
    
    # Force GRanges for single position
    result <- GenomicCoordinates("chr1:1000", force_class = "GRanges")
    expect_s4_class(result, "GRanges")
    expect_equal(start(result), 1000)
    expect_equal(end(result), 1000)
    
    # Force GPos for range (should still work)
    result <- GenomicCoordinates("chr1:1000-2000", force_class = "GPos")
    expect_s4_class(result, "GPos")
    
    # Test with empty input and force_class
    result <- GenomicCoordinates(character(0), force_class = "GPos")
    expect_s4_class(result, "GPos")
    expect_equal(length(result), 0)
    
    result <- GenomicCoordinates(character(0), force_class = "IRanges")
    expect_s4_class(result, "IRanges")
    expect_equal(length(result), 0)
    
    result <- GenomicCoordinates(character(0), force_class = "GInteractions")
    expect_s4_class(result, "GInteractions")
    expect_equal(length(result), 0)
})

test_that("Mixed vectors with unusual chromosome names work", {
    
    # Should detect as GRanges (mixed single and ranges)
    result <- GenomicCoordinates(mixed_unusual)
    expect_s4_class(result, "GRanges")
    expect_equal(length(result), 4)
    
    # Check specific chromosome names are preserved
    expected_chrs <- c("chr I", "chr_a", "chr-01", "scaffold_123")
    expect_equal(as.character(seqnames(result)), expected_chrs)
})

test_that("Edge cases with unusual formats", {
    
    # Very long chromosome names
    long_chr <- "supercalifragilisticexpialidocious_chromosome_1:1000-2000"
    result <- GenomicCoordinates(long_chr)
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "supercalifragilisticexpialidocious_chromosome_1")
    
    # Chromosome names with dots and numbers
    dotted_chr <- "chr1.2.3.4:1000-2000"
    result <- GenomicCoordinates(dotted_chr)
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "chr1.2.3.4")
    
    # Mixed separators in chromosome names
    mixed_sep_chr <- "chr_1-2.3:1000-2000"
    result <- GenomicCoordinates(mixed_sep_chr)
    expect_s4_class(result, "GRanges")
    expect_equal(as.character(seqnames(result)), "chr_1-2.3")
})

test_that("IRanges detection edge cases", {
    
    for (test_string in ambiguous_cases) {
        result <- GenomicCoordinates(test_string)
        expect_s4_class(result, "IRanges")
    }
    
    for (test_string in not_iranges_cases) {
        result <- GenomicCoordinates(test_string)
        expect_false(inherits(result, "IRanges"))
    }
})

test_that("GCoordinates alias works", {
    
    # Test that GCoordinates is an alias for GenomicCoordinates
    result1 <- GenomicCoordinates("chr1:1000-2000")
    result2 <- GCoordinates("chr1:1000-2000")
    
    expect_equal(result1, result2)
    expect_s4_class(result2, "GRanges")
})

test_that("Error handling with invalid force_class", {
    
    # Invalid force_class should throw error
    expect_error(GenomicCoordinates("chr1:1000-2000", force_class = "InvalidClass"))
    expect_error(GenomicCoordinates(character(0), force_class = "InvalidClass"))
})

test_that("Error cases", {
    
    for (test_string in error_cases) {
        expect_error(GenomicCoordinates(test_string))
    }
})

test_that("detect_genomic_class handles unusual chromosome names", {
    
    unusual_chr_cases <- c(
        "chr I:1000-2000",           # Should be GRanges
        "chr_a:1000",                # Should be GPos
        "chr-01:1-10|chr-02:20-30",  # Should be GInteractions
        "123-456"                    # Should be IRanges
    )
    
    classes <- detect_genomic_class(unusual_chr_cases)
    expected_classes <- c("GRanges", "GPos", "GInteractions", "IRanges")
    expect_equal(classes, expected_classes)
    
    # Test that result is unnamed vector
    expect_null(names(classes))
})

test_that("Single position vs range detection with unusual chromosomes", {
    
    # All single positions with unusual chromosome names
    single_positions <- c("chr I:1000", "chr_a:2000", "chr-01:3000")
    result <- GenomicCoordinates(single_positions)
    expect_s4_class(result, "GPos")
    expect_equal(length(result), 3)
    
    # All ranges with unusual chromosome names  
    ranges <- c("chr I:1000-2000", "chr_a:2000-3000", "chr-01:3000-4000")
    result <- GenomicCoordinates(ranges)
    expect_s4_class(result, "GRanges")
    expect_equal(length(result), 3)
    
    # Mixed positions and ranges (should default to GRanges)
    mixed <- c("chr I:1000", "chr_a:2000-3000")
    result <- GenomicCoordinates(mixed)
    expect_s4_class(result, "GRanges")
    expect_equal(length(result), 2)
})
