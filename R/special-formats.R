#' Additional helper functions for corner cases
#'
#' Handle various edge cases and special formats in genomic coordinate parsing

#' Detect if a string represents a single genomic position
#'
#' @param x Character string
#' @return Logical indicating if it's a single position
.is_single_position <- function(x) {
    # Normalize whitespace first
    x <- trimws(x)
    x <- gsub("\\s+", " ", x)
    
    # Check various patterns that indicate single position
    patterns <- c(
        "^[^:]+:[0-9,\\s]+$",  # chr1:1000 or chr1:1,000 or chr1: 1000
        "^[^:]+:[0-9,\\s]+\\s*[*+-]\\s*$",
        "^[^:\\s]+\\s+[0-9,\\s]+$"
    )
    
    any(vapply(patterns, function(p) grepl(p, x), logical(1)))
}

#' Handle special genomic string formats
#'
#' @param x Character string
#' @return Parsed genomic information
.handle_special_formats <- function(x) {
    # Normalize whitespace first
    x <- trimws(x)
    x <- gsub("\\s+", " ", x)
    
    # Handle formats like "chr1:1,234,234" (single position with commas)
    if (.is_single_position(x)) {
        
        # Handle space-separated single position: "chr1 1000"
        if (!grepl(":", x)) {
            space_parts <- strsplit(x, "\\s+")[[1]]
            if (length(space_parts) == 2) {
                seqname <- space_parts[1]
                coord_part <- space_parts[2]
                
                # Clean and parse coordinate
                position <- .as_numeric(gsub(",", "", trimws(coord_part)))

                if (!is.na(position)) {
                    return(list(
                        seqnames = seqname,
                        start = position,
                        end = position,
                        strand = "*",
                        single = TRUE
                    ))
                }
            }
        } else {
            # Handle colon-separated formats
            parts <- strsplit(x, ":")[[1]]
            seqname <- trimws(parts[1])
            
            # Extract coordinate and optional strand
            coord_strand <- trimws(parts[2])
            strand_match <- regexpr("[*+-]\\s*$", coord_strand)
            
            if (strand_match > 0) {
                coord_part <- substr(coord_strand, 1, strand_match - 1)
                strand <- trimws(substr(
                    coord_strand,
                    strand_match,
                    nchar(coord_strand)
                ))
            } else {
                coord_part <- coord_strand
                strand <- "*"
            }
            
            # Clean and parse coordinate (remove commas and extra spaces)
            position <- .as_numeric(gsub("[,\\s]", "", trimws(coord_part)))
            
            if (!is.na(position)) {
                return(list(
                    seqnames = seqname,
                    start = position,
                    end = position,
                    strand = strand,
                    single = TRUE
                ))
            }
        }
    }
    
    return(NULL)
}
