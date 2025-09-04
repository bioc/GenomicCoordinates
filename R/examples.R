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

# Basic formats (these already work in GenomicRanges)
# as("chr1:4-100", "GRanges")
# as("chr1:4-100:+", "GRanges")

# Enhanced formats supported by GenomicCoordinates:

# Comma-separated coordinates
# as("chr1:1,234,234", "GRanges")        # Single position with commas
# as("chr1:1,234-56,345", "GRanges")     # Range with commas
# as("chr1:100,000-200,000", "GRanges")  # Large numbers with commas
# as("chr1:100,000-200,000:+", "GRanges") # With strand

# Space-delimited format
# as("chr1 1 10", "GRanges")           # chr seqname start end
# as("chr1 100000 200000", "GRanges")  # Larger coordinates

# Auto-detection of single positions (returns GPos when appropriate)
# as("chr1:1000", "GPos")              # Explicit GPos

# GInteractions support
# as("chr1:1-10|chr2:4-40", "GInteractions")
# as("chr1:100,000-200,000|chr2:300,000-400,000", "GInteractions")

# Enhanced IRanges parsing
# IRanges("1,234..56,345")  # Comma-separated range
# IRanges("1234 56345")     # Space-separated range
