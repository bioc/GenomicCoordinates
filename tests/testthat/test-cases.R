# Master test cases for GenomicCoordinates package
# This file contains all test strings organized by category

# ==== TEST CASES FOR GRanges ====

# Standard formats (should work with existing GenomicRanges)
granges_standard <- c(
    "chr1:1000-2000",
    "chr1:1000-2000:+", 
    "chr1:1000-2000:-",
    "chr1:1000-2000:*",
    "chrX:1000-2000",
    "chr22:1000-2000"
)

# Comma-separated numbers
granges_comma <- c(
    "chr1:1,000-2,000",
    "chr1:10,000-20,000", 
    "chr1:100,000-200,000",
    "chr1:1,000,000-2,000,000",
    "chr1:1,234-56,345",
    "chr1:1,234,567-9,876,543"
)

# Comma-separated with strand
granges_comma_strand <- c(
    "chr1:1,000-2,000:+",
    "chr1:100,000-200,000:-",
    "chr1:1,000,000-2,000,000:*"
)

# Space-delimited format
granges_space <- c(
    "chr1 1000 2000",
    "chr1 100000 200000",
    "chrX 1000 2000",
    "chr22 50000 100000"
)

# Space-delimited format with irregular spacing
granges_space_irregular <- c(
    "chr1  1000   2000",      # Multiple spaces between parts
    "chr1    100000     200000",  # Many spaces
    "chrX  1000      2000",   # Mixed spacing
    "chr22   50000  100000"   # Irregular spacing
)

# Mixed space-colon format with irregular spacing  
granges_mixed_spacing <- c(
    "chr1  1000   2000:+",    # Space-delimited coords with strand
    "chr1    100000     200000:-", # Many spaces with strand
    "chrX  1000      2000:*", # Mixed spacing with strand
    "chr22   50000  100000:+" # Irregular spacing with strand
)

# Different separators
granges_separators <- c(
    "chr1:1000..2000",  # double dot
    "chr1:1000_2000",   # underscore
    "chr1:1000:2000"    # double colon
)

# Completely invalid inputs
invalid_inputs <- c(
    "",                     # Empty string
    "   ",                  # Only whitespace
    "invalid_format",       # No genomic structure
    "chr1:",               # Missing coordinates
    "chr1:abc-def",        # Non-numeric coordinates
    "chr1:-2000",          # Missing start coordinate
    "chr1:2000-1000",      # End before start
    "chr1:1000-2000-3000", # Too many coordinate parts
    NULL,                  # NULL input
    NA_character_,         # NA input
    character(0)           # Zero-length character vector (should work)
)

# Complex chromosome names
test_chr_names <- c(
    "chr I:1000-2000",           # Roman numerals with space
    "chr_a:1000-2000",           # Underscore
    "chr-01:1000-2000",          # Dash with numbers
    "scaffold_123:1000-2000",    # Scaffold names
    "GL000001.1:1000-2000",      # GenBank accession
    "NC_000001.11:1000-2000",    # RefSeq accession
    "2L:1000-2000",              # Drosophila naming
    "chrUn_GL000220v1:1000-2000", # Unplaced scaffolds
    "chr1_KI270706v1_random:1000-2000", # Random contigs
    "HLA-DRB1*15:01:1000-2000"   # HLA nomenclature
)
    
# Mixed vector with unusual chromosome names
mixed_unusual <- c(
    "chr I:1000-2000",
    "chr_a:3000",
    "chr-01:5000-6000",
    "scaffold_123:7000"
)

# Chromosome names with special characters that might break parsing
special_char_chrs <- c(
    "chr1+2:1000-2000",          # Plus sign
    "chr1*2:1000-2000",          # Asterisk
    "chr1?2:1000-2000",          # Question mark
    "chr1[2]:1000-2000",         # Brackets
    "chr1(2):1000-2000",         # Parentheses
    "chr1{2}:1000-2000",         # Braces
    "chr1\\2:1000-2000",         # Backslash
    "chr1/2:1000-2000",          # Forward slash
    "chr1#2:1000-2000",          # Hash
    "chr1@2:1000-2000",          # At symbol
    "chr1$2:1000-2000",          # Dollar sign
    "chr1%2:1000-2000",          # Percent
    "chr1^2:1000-2000",          # Caret
    "chr1&2:1000-2000",          # Ampersand
    "chr1=2:1000-2000",          # Equals
    "chr1~2:1000-2000",          # Tilde
    "chr1`2:1000-2000"           # Backtick
)

# Various whitespace and control characters
whitespace_cases <- c(
    "chr1\t1000\t2000",         # Tab-separated
    "chr1\n1000\n2000",         # Newline-separated (probably will error)
    "chr1\r1000\r2000",         # Carriage return
    "chr1\f1000\f2000",         # Form feed
    "chr1\v1000\v2000",         # Vertical tab
    "\tchr1:1000-2000\t",       # Leading/trailing tabs
    "\nchr1:1000-2000\n",       # Leading/trailing newlines
    "  \t  chr1:1000-2000  \t  " # Mixed whitespace
)

# Iinvalid force_class values
invalid_force_classes <- c(
    "InvalidClass",
    "granges",           # Wrong case
    "GRANGES",           # Wrong case
    "GRange",            # Typo
    "character",         # Wrong type
    "numeric",           # Wrong type
    "",                  # Empty string
    NA_character_,       # NA
    NULL                 # NULL (should work - means no forcing)
)

# Vectors that mix valid and invalid entries
mixed_vectors <- list(
    c("chr1:1000-2000", "invalid_format"),
    c("chr1:1000-2000", "chr2:abc-def"),
    c("valid1:100-200", "", "valid2:300-400"),
    c("chr1:1000-2000", NA_character_, "chr2:2000-3000")
)

# Numeric inputs
numeric_inputs <- list(
    1000,
    c(1000, 2000),
    1000.5,
    Inf,
    -Inf,
    NaN
)

# Coordinates that should error
very_large_coords <- c(
    "chr1:999999999999-1000000000000" # Trillion range
)


# ==== TEST CASES FOR GPos (single positions) ====

gpos_standard <- c(
    "chr1:1000",
    "chrX:50000", 
    "chr22:100000"
)

gpos_comma <- c(
    "chr1:1,000",
    "chr1:100,000",
    "chr1:1,000,000"
)

gpos_strand <- c(
    "chr1:1000:+",
    "chr1:1000:-", 
    "chr1:1000:*"
)

# Single positions with irregular spacing
gpos_space_irregular <- c(
    "chr1  1000",         # Space between chr and position
    "chr1    100000",     # Multiple spaces
    "chrX     50000"      # Many spaces
)

# Single positions with mixed format and spacing
gpos_mixed_spacing <- c(
    "chr1: 1000",         # Space after colon
    "chr1 :1000",         # Space before colon
    "chr1  : 1000",       # Spaces around colon
    "chr1:1,000:+",       # Comma with strand (no extra spaces)
    "chr1   1000"         # Space-separated format
)

# Single positions with unusual chromosome names
gpos_unusual_chr <- c(
    "chr I:1000",                # Roman numerals with space
    "chr_a:50000",               # Underscore  
    "chr-01:100000",             # Dash with numbers
    "scaffold_123:5000",         # Scaffold names
    "GL000001.1:10000",          # GenBank accession
    "2L:25000",                  # Drosophila naming
    "chrUn_GL000220v1:30000"     # Unplaced scaffolds
)

# ==== TEST CASES FOR GInteractions ====

ginteractions_standard <- c(
    "chr1:1-10|chr2:20-30",
    "chr1:1000-2000|chr2:3000-4000",
    "chrX:1000-2000|chrY:3000-4000"
)

ginteractions_comma <- c(
    "chr1:1,000-2,000|chr2:3,000-4,000",
    "chr1:100,000-200,000|chr2:300,000-400,000"
)

ginteractions_strand <- c(
    "chr1:1000-2000:+|chr2:3000-4000:-",
    "chr1:1000-2000:*|chr2:3000-4000:+"
)

ginteractions_mixed <- c(
    "chr1:1,000-2,000|chr2 3000 4000",  # Mixed formats
    "chr1 1000 2000|chr2:3,000-4,000"
)

# GInteractions with irregular spacing
ginteractions_spacing <- c(
    "chr1: 1-10  |  chr2: 20-30",     # Spaces around pipe and colons
    "chr1  1000   2000 | chr2  3000   4000", # Space-delimited with pipe
    "chr1:1,000-2,000  |  chr2: 3,000-4,000", # Mixed spacing
    "chr1  1000   2000:+ | chr2: 3000-4000:-" # Complex mixed format
)

# Interactions with unusual chromosome names
gint_unusual_chr <- c(
    "chr I:1-10|chr II:20-30",                    # Roman numerals
    "chr_a:1000-2000|chr_b:3000-4000",           # Underscores
    "chr-01:1000-2000|chr-02:3000-4000",         # Dashes
    "scaffold_123:100-200|scaffold_456:300-400", # Scaffolds
    "GL000001.1:1000-2000|GL000002.1:3000-4000", # GenBank
    "2L:1000-2000|3R:3000-4000"                  # Drosophila
)

# ==== TEST CASES FOR IRanges ====

iranges_standard <- c(
    "1000-2000",
    "100-200",
    "1-10"
)

iranges_comma <- c(
    "1,000-2,000", 
    "100,000-200,000",
    "1,234,567-9,876,543"
)

iranges_separators <- c(
    "1000..2000",   # double dot
    "1000 2000",    # space
    "1000_2000"     # underscore
)

iranges_single <- c(
    "1000",
    "50000"
)

# Numbers that could be confused with chromosome names
ambiguous_cases <- c(
    "123-456",        # Pure numbers
    "1 200",          # Space-separated numbers
    "1,000-2,000",    # Comma-separated numbers
    "100",            # Single number
    "1,234"           # Single number with comma
)

# Cases that should NOT be detected as IRanges
not_iranges_cases <- c(
    "chr123:456-789",    # Has chromosome prefix
    "scaffold1:100-200", # Has chromosome prefix
    "1p:100-200"         # Looks like chromosome arm
)


# ==== EDGE CASES AND CORNER CASES ====

edge_cases <- c(
    "chr1:1-1",          # Single base range
    "chr1:0-100",        # Zero start
    "chrMT:1000-2000",   # Mitochondrial
    "scaffold_1:100-200", # Non-standard chromosome names
    "GL000001.1:100-200" # Contig names
)

# Complex formats
complex_cases <- c(
    "chr1:1,234,567",           # Single position with multiple commas
    "chr1:1 000 000-2 000 000", # Spaces in numbers (European format)
    "chr1:1.234.567-2.345.678"  # Dots as thousands separators
)

# Error cases (should fail gracefully)
error_cases <- c(
    "invalid_string",
    "chr1:",
    "chr1:abc-def",
    "chr1:-2000",
    "1000-abc",
    "abc-2000",
    "invalid:format"
)

# Multi-element vectors
multi_element_cases <- list(
    granges = c("chr1:1000-2000", "chr2:3000-4000", "chrX:5000-6000"),
    gpos = c("chr1:1000", "chr2:2000", "chr3:3000"),
    mixed_single = c("chr1:1000", "chr1:2000", "chr1:3000"),  # All single positions
    mixed_ranges = c("chr1:1000-2000", "chr1:3000-4000"),     # All ranges
    iranges = c("1000-2000", "3000-4000", "5000-6000")
)

# ==== UNUSUAL CHROMOSOME NAMES ====

# Chromosome names with spaces (common in some organisms)
unusual_chr_spaces <- c(
    "chr I:1000-2000",           # Roman numerals
    "chr II:1000-2000",
    "chr III:1000-2000", 
    "chr IV:1000-2000"
)

# Chromosome names with underscores
unusual_chr_underscores <- c(
    "chr_1:1000-2000",
    "chr_a:1000-2000",
    "chr_alpha:1000-2000",
    "scaffold_123:1000-2000",
    "contig_456:1000-2000",
    "linkage_group_1:1000-2000"
)

# Chromosome names with dashes
unusual_chr_dashes <- c(
    "chr-01:1000-2000",
    "chr-02:1000-2000", 
    "chr-X:1000-2000",
    "chr-Y:1000-2000",
    "group-I:1000-2000",
    "arm-2L:1000-2000"
)

# Complex chromosome names (real examples from various genomes)
unusual_chr_complex <- c(
    "GL000001.1:1000-2000",              # GenBank accession
    "NC_000001.11:1000-2000",            # RefSeq accession  
    "AC_000001.1:1000-2000",             # Alternative accession
    "NW_003571030.1:1000-2000",          # Whole genome shotgun
    "2L:1000-2000",                      # Drosophila chromosome arms
    "3R:1000-2000", 
    "4:1000-2000",                       # Simple numeric
    "chrUn_GL000220v1:1000-2000",        # Unplaced scaffolds
    "chr1_KI270706v1_random:1000-2000",  # Random contigs
    "chr6_ssto_hap7:1000-2000",          # Haplotype scaffolds
    "HLA-DRB1*15:01:1000-2000",          # HLA nomenclature
    "Pt:1000-2000",                      # Plastid
    "Mt:1000-2000",                      # Mitochondrial (short form)
    "chrM:1000-2000",                    # Mitochondrial (long form)
    "chrUn:1000-2000",                   # Unplaced
    "scaffold123.1:1000-2000",           # Scaffolds with version
    "contig_1_1:1000-2000",              # Multi-underscore
    "super_scaffold_1.1:1000-2000"       # Complex scaffold names
)

# Organism-specific chromosome naming conventions
organism_specific_chr <- c(
    # Drosophila
    "2L:1000-2000", "2R:1000-2000", "3L:1000-2000", "3R:1000-2000",
    "4:1000-2000", "X:1000-2000", "Y:1000-2000",
    
    # C. elegans  
    "I:1000-2000", "II:1000-2000", "III:1000-2000", 
    "IV:1000-2000", "V:1000-2000", "X:1000-2000",
    
    # Yeast
    "chr01:1000-2000", "chr02:1000-2000", "chr03:1000-2000",
    "chr04:1000-2000", "chr05:1000-2000", "chr06:1000-2000",
    "chr07:1000-2000", "chr08:1000-2000", "chr09:1000-2000",
    "chr10:1000-2000", "chr11:1000-2000", "chr12:1000-2000",
    "chr13:1000-2000", "chr14:1000-2000", "chr15:1000-2000",
    "chr16:1000-2000",
    
    # Plant chromosomes (Arabidopsis)
    "Chr1:1000-2000", "Chr2:1000-2000", "Chr3:1000-2000",
    "Chr4:1000-2000", "Chr5:1000-2000", "ChrC:1000-2000", "ChrM:1000-2000"
)

# Edge cases with special characters
unusual_chr_special <- c(
    "chr1.1:1000-2000",                  # Dots
    "chr1_2_3:1000-2000",                # Multiple underscores
    "chr1-2-3:1000-2000",                # Multiple dashes
    "chr1.2_3-4:1000-2000",              # Mixed separators
    "CHROMOSOME_1:1000-2000",            # All caps
    "chromosome_i:1000-2000",            # All lowercase
    "Chr_I:1000-2000",                   # Mixed case
    "scaffold1000001:1000-2000",         # Very long scaffold numbers
    "supercalifragilisticexpialidocious_chromosome_1:1000-2000" # Extremely long name
)

# Unusual chromosome names for single positions (GPos)
unusual_chr_gpos <- c(
    "chr I:1000", "chr_a:50000", "chr-01:100000",
    "GL000001.1:10000", "2L:25000", "scaffold_123:5000",
    "chrUn_GL000220v1:30000", "HLA-DRB1*15:01:75000"
)

# Unusual chromosome names for interactions (GInteractions)  
unusual_chr_ginteractions <- c(
    "chr I:1-10|chr II:20-30",
    "chr_a:1000-2000|chr_b:3000-4000", 
    "chr-01:1000-2000|chr-02:3000-4000",
    "scaffold_123:100-200|scaffold_456:300-400",
    "GL000001.1:1000-2000|GL000002.1:3000-4000",
    "2L:1000-2000|3R:3000-4000",
    "HLA-DRB1*15:01:1000-2000|HLA-DQB1*06:02:3000-4000"
)
