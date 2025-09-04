#' Coercion methods for genomic coordinates
#'
#' Enhanced coercion methods to convert character strings to GRanges, GPos,
#' and GInteractions objects with support for various string formats.
#' @name GenomicCoordinates-coercion

# Coercion from character to GRanges
setAs("character", "GRanges", function(from) {
    if (length(from) == 0) {
        return(GRanges())
    }
    
    # Handle vector of strings
    parsed_list <- lapply(from, .parse_genomic_string)
    
    # Check if any are single positions (should be GPos)
    has_single <- any(sapply(parsed_list, function(x) isTRUE(x$single)))
    
    if (has_single && length(from) == 1) {
        # Convert single position to GPos, but return as GRanges for compatibility
        parsed <- parsed_list[[1]]
        return(GRanges(
            seqnames = parsed$seqnames,
            ranges = IRanges(start = parsed$start, end = parsed$end),
            strand = parsed$strand
        ))
    }
    
    # Extract components
    seqnames <- sapply(parsed_list, function(x) x$seqnames)
    starts <- sapply(parsed_list, function(x) x$start)
    ends <- sapply(parsed_list, function(x) x$end)
    strands <- sapply(parsed_list, function(x) x$strand)
    
    # Create GRanges
    GRanges(
        seqnames = seqnames,
        ranges = IRanges(start = starts, end = ends),
        strand = strands
    )
})

# Coercion from character to GPos
setAs("character", "GPos", function(from) {
    if (length(from) == 0) {
        return(GPos())
    }
    
    # Parse and check if all are single positions
    parsed_list <- lapply(from, .parse_genomic_string)
    
    # Extract components
    seqnames <- sapply(parsed_list, function(x) x$seqnames)
    positions <- sapply(parsed_list, function(x) x$start)
    strands <- sapply(parsed_list, function(x) x$strand)
    
    # Create GPos
    GPos(
        seqnames = seqnames,
        pos = positions,
        strand = strands
    )
})

# Coercion from character to GInteractions
setAs("character", "GInteractions", function(from) {
    if (length(from) == 0) {
        return(GInteractions())
    }
    
    # Handle vector of strings
    parsed_list <- lapply(from, function(x) {
        # If string contains "|", parse as interaction
        if (grepl("\\|", x)) {
            return(.parse_ginteractions_string(x))
        } else {
            # If no "|", create self-interaction from genomic range
            parsed <- .parse_genomic_string(x)
            return(list(
                anchor1 = parsed,
                anchor2 = parsed
            ))
        }
    })
    
    # Extract anchor1 components
    anchor1_seqnames <- sapply(parsed_list, function(x) x$anchor1$seqnames)
    anchor1_starts <- sapply(parsed_list, function(x) x$anchor1$start)
    anchor1_ends <- sapply(parsed_list, function(x) x$anchor1$end)
    anchor1_strands <- sapply(parsed_list, function(x) x$anchor1$strand)
    
    # Extract anchor2 components
    anchor2_seqnames <- sapply(parsed_list, function(x) x$anchor2$seqnames)
    anchor2_starts <- sapply(parsed_list, function(x) x$anchor2$start)
    anchor2_ends <- sapply(parsed_list, function(x) x$anchor2$end)
    anchor2_strands <- sapply(parsed_list, function(x) x$anchor2$strand)
    
    # Create GRanges for anchors
    anchor1 <- GRanges(
        seqnames = anchor1_seqnames,
        ranges = IRanges(start = anchor1_starts, end = anchor1_ends),
        strand = anchor1_strands
    )
    
    anchor2 <- GRanges(
        seqnames = anchor2_seqnames,
        ranges = IRanges(start = anchor2_starts, end = anchor2_ends),
        strand = anchor2_strands
    )
    
    # Create GInteractions
    suppressWarnings(GInteractions(anchor1, anchor2))
})
