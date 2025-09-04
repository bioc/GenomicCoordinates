#' Parse genomic coordinate strings with enhanced format support
#'
#' Internal utility functions to parse various genomic coordinate string formats
#' including comma-separated numbers, space-delimited coordinates, and different
#' separators.
#'
#' @param x A character string representing genomic coordinates
#' @return Parsed components as a list
#' @keywords internal

# Helper function to clean numeric strings (remove commas, spaces)
.clean_numeric_string <- function(x) {
    # Remove commas and extra spaces, but preserve single spaces as separators
    gsub(",", "", x)
}

# Helper function to parse coordinate ranges with various separators
.parse_coordinates <- function(coord_str) {
    # Clean the coordinate string
    clean_str <- .clean_numeric_string(coord_str)
    
    # Input validation
    if (is.null(clean_str) || is.na(clean_str) || nchar(trimws(clean_str)) == 0) {
        stop("Unable to parse coordinates: ", coord_str)
    }
    
    # Try different separators in order of preference
    separators <- c("-", "\\.\\.", " +", "_", ":")
    
    for (sep in separators) {
        if (grepl(sep, clean_str)) {
            parts <- strsplit(clean_str, sep)[[1]]
            parts <- parts[parts != ""]  # Remove empty strings
            if (length(parts) == 2) {
                start_pos <- suppressWarnings(as.numeric(trimws(parts[1])))
                end_pos <- suppressWarnings(as.numeric(trimws(parts[2])))
                
                # Validate that both are numeric
                if (!is.na(start_pos) && !is.na(end_pos)) {
                    # Check for invalid ranges (end before start)
                    if (end_pos < start_pos) {
                        stop("End coordinate cannot be less than start coordinate")
                    }
                    result <- list(start = start_pos, end = end_pos)
                    # Only add single=TRUE if it's actually a single position
                    if (start_pos == end_pos) {
                        result$single <- TRUE
                    }
                    return(result)
                } else {
                    stop("Invalid coordinate values: ", coord_str)
                }
            } else if (length(parts) == 1) {
                # Might be incomplete range like "1000-" or "-2000"
                if (grepl("^-|.*-$", clean_str)) {
                    stop("Incomplete coordinate range: ", coord_str)
                }
            } else {
                stop("Invalid coordinate format: ", coord_str)
            }
        }
    }
    
    # If no separator found, try to parse as single position
    single_pos <- suppressWarnings(as.numeric(trimws(clean_str)))
    if (!is.na(single_pos)) {
        # Check for negative coordinates
        if (single_pos < 0) {
            stop("Genomic coordinates cannot be negative")
        }
        return(list(start = single_pos, end = single_pos, single = TRUE))
    }
    
    # If all parsing fails
    stop("Unable to parse coordinates: ", coord_str)
}

# Enhanced parsing function for genomic strings
.parse_genomic_string <- function(x) {
    # Input validation
    if (is.null(x) || is.na(x) || nchar(trimws(x)) == 0) {
        stop("Invalid genomic string format: ", x)
    }
    
    # Handle GInteractions (contains |)
    if (grepl("\\|", x)) {
        return(.parse_ginteractions_string(x))
    }
    
    # Normalize whitespace first
    x <- trimws(x)
    x <- gsub("\\s+", " ", x)  # Replace multiple spaces with single space
    
    # Early validation for common malformed inputs
    if (x == "" || x == " " || x == ":" || x == "|" || grepl("^:.*|.*:$", x)) {
        stop("Invalid genomic string format: ", x)
    }
    
    # Try special format handling first
    special_result <- .handle_special_formats(x)
    if (!is.null(special_result)) {
        return(special_result)
    }
    
    # Check for mixed space-colon format: "chr1  1      1000:+"
    # This handles cases where there are spaces before coordinates and strand after colon
    if (grepl("^[^:]+\\s+[0-9].*:[*+-]?$", x)) {
        # Split by colon to separate strand
        colon_parts <- strsplit(x, ":")[[1]]
        coord_strand_part <- colon_parts[1]
        strand <- if (length(colon_parts) >= 2 && colon_parts[2] != "") colon_parts[2] else "*"
        
        # Parse the coordinate part which has spaces
        space_parts <- strsplit(trimws(coord_strand_part), "\\s+")[[1]]
        
        if (length(space_parts) >= 3) {
            seqname <- space_parts[1]
            start_coord <- space_parts[2]
            end_coord <- space_parts[3]
            
            coords <- .parse_coordinates(paste(start_coord, end_coord, sep = "-"))
            return(list(
                seqnames = seqname,
                start = coords$start,
                end = coords$end,
                strand = strand,
                single = coords$single  # This will be TRUE or NULL
            ))
        }
    }
    
    # Split by colon for standard format
    parts <- strsplit(x, ":")[[1]]
    
    if (length(parts) < 2) {
        # Try space-delimited format: "chr1 1 10" or "chr1  1     10"
        space_parts <- strsplit(trimws(x), "\\s+")[[1]]
        if (length(space_parts) >= 3) {
            seqname <- space_parts[1]
            start_coord <- space_parts[2]
            end_coord <- space_parts[3]
            
            coords <- .parse_coordinates(paste(start_coord, end_coord, sep = "-"))
            return(list(
                seqnames = seqname,
                start = coords$start,
                end = coords$end,
                strand = "*",
                single = coords$single  # This will be TRUE or NULL
            ))
        } else if (length(space_parts) == 2) {
            # Handle "chr1 1000" format (single position with space)
            seqname <- space_parts[1]
            position <- space_parts[2]
            
            coords <- .parse_coordinates(position)
            return(list(
                seqnames = seqname,
                start = coords$start,
                end = coords$end,
                strand = "*",
                single = TRUE
            ))
        } else {
            stop("Invalid genomic string format: ", x)
        }
    }
    
    # Validate we have chromosome and coordinate parts
    if (length(parts) < 2 || nchar(trimws(parts[1])) == 0 || nchar(trimws(parts[2])) == 0) {
        stop("Invalid genomic string format: ", x)
    }

    seqname <- trimws(parts[1])
    coord_part <- trimws(parts[2])
    strand <- if (length(parts) >= 3) trimws(parts[3]) else "*"
    
    # Validate strand
    if (!strand %in% c("+", "-", "*")) {
        strand <- "*"
    }
    
    # Additional validation for coordinate part
    if (coord_part == "" || grepl("^-.*|.*-$", coord_part)) {
        stop("Invalid genomic string format: ", x)
    }

    # Parse coordinates
    coords <- .parse_coordinates(coord_part)
    
    return(list(
        seqnames = seqname,
        start = coords$start,
        end = coords$end,
        strand = strand,
        single = coords$single  # This will be TRUE or NULL
    ))
}

# Parse GInteractions strings
.parse_ginteractions_string <- function(x) {
    parts <- strsplit(x, "\\|")[[1]]
    if (length(parts) != 2) {
        stop("GInteractions string must contain exactly one '|' separator")
    }
    
    anchor1 <- .parse_genomic_string(parts[1])
    anchor2 <- .parse_genomic_string(parts[2])
    
    return(list(
        anchor1 = anchor1,
        anchor2 = anchor2
    ))
}
