# GenomicCoordinates

Enhanced string parsing for genomic coordinates in Bioconductor.

## Overview

`GenomicCoordinates` extends the string parsing capabilities of `GenomicRanges`, `IRanges`, and `InteractionSet` packages to support various genomic coordinate string formats including comma-separated numbers, space-delimited coordinates, and automatic detection of appropriate object types.

## Installation

```r
# Install from Bioconductor (when available)
BiocManager::install("GenomicCoordinates")

# Or install version from github
BiocManager::install("js2264/GenomicCoordinates")
```

## Supported Formats

- **Standard**: `chr1:1000-2000`, `chr1:1000-2000:+` (auto-detects as `GPos`)
- **Single positions**: `chr1:1000` (auto-detects as `GPos`)
- **Interactions**: `chr1:1-10|chr2:4-40` (auto-detects as `GInteractions`)
- **Comma-separated**: `chr1:1,000-2,000`, `chr1:100,000-200,000`
- **Space-delimited**: `chr1 1000 2000`
- **Irregular spacing**: `chr1  1000   2000`, `chr1: 1-10  |  chr2: 20-30`

## Quick Start

Loading the `GenomicCoordinates` package allows you to use enhanced parsing methods directly:

```r
library(GenomicRanges)
GRanges("chr1:100,000-200,000")
## Error in asMethod(object) : 
##   The character vector to convert to a GRanges object must contain strings of the form "chr:start-end" or "chr:start-end:strand", with end >= start - 1, or "chr:pos" or "chr:pos:strand". For example: "chr1:2501-2900", "chr1:2501-2900:+",
##   or "chr1:740". Note that ".." is a valid alternate start/end separator. Strand can be "+", "-", "*", or missing.
  
library(GenomicCoordinates)
GRanges("chr1:100,000-200,000")
## Ranges object with 1 range and 0 metadata columns:
##      seqnames        ranges strand
##         <Rle>     <IRanges>  <Rle>
##  [1]     chr1 100000-200000      *
##  -------
##  seqinfo: 1 sequence from an unspecified genome; no seqlengths
```

```r
# Enhanced GRanges parsing
GRanges("chr1 1000 2000")

# Auto-detection
GenomicCoordinates("chr1:1000")  # Returns GPos

# GInteractions
as("chr1:1-10|chr2:4-40", "GInteractions")

# Enhanced IRanges
IRanges("1,234..56,345")
```

