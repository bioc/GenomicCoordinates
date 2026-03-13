# Example usage of GenomicCoordinates package
# This file demonstrates the enhanced parsing capabilities

# Standard examples that should work with the package:

# library(GenomicCoordinates)

# MAIN FUNCTION - Auto-detection examples:
# GenomicCoordinates("chr1:1000-2000")           # Auto-detects as GRanges
# GenomicCoordinates("chr1:1000")                # Auto-detects as GPos
# GenomicCoordinates("chr1:1-10|chr2:4-40")      # Auto-detects as GInteractions
# GenomicCoordinates("1000-2000")               # Auto-detects as IRanges
# GenomicCoordinates("chr1:100,000-200,000")     # Enhanced format support

# Irregular spacing examples:
# GenomicCoordinates("chr1  1000   2000")        # Multiple spaces
# GenomicCoordinates("chr1    1000")             # Single position with spaces
# GenomicCoordinates("chr1  1000   2000:+")      # Mixed format with strand
# GenomicCoordinates("chr1: 1-10  |  chr2: 20-30") # GInteractions with spacing

# Force specific classes:
# GenomicCoordinates("chr1:1000", force_class = "GRanges")
# GenomicCoordinates("1000-2000", force_class = "IRanges")

# Detect class without parsing:
# detect_genomic_class(c("chr1:1000", "chr1:1-10", "1000-2000"))

# Explicit conversion functions:

# Comma-separated coordinates
# as_granges("chr1:1,234,234")        # Single position with commas
# as_granges("chr1:1,234-56,345")     # Range with commas
# as_granges("chr1:100,000-200,000")  # Large numbers with commas
# as_granges("chr1:100,000-200,000:+") # With strand

# Space-delimited format
# as_granges("chr1 1 10")           # chr seqname start end
# as_granges("chr1 100000 200000")  # Larger coordinates

# Explicit GPos conversion
# as_gpos("chr1:1000")
# as_gpos("chr1:1,234,567")

# GInteractions support
# as_ginteractions("chr1:1-10|chr2:4-40")
# as_ginteractions("chr1:100,000-200,000|chr2:300,000-400,000")

# Enhanced IRanges parsing
# as_iranges("1,234..56,345")  # Comma-separated range
# as_iranges("1234 56345")     # Space-separated range
