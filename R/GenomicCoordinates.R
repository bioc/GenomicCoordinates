#' GenomicCoordinates: Main parsing function
#'
#' Automatically parse genomic coordinate strings into the most appropriate
#' Bioconductor object type (GRanges, GPos, GInteractions, or IRanges).

#' Parse strings into appropriate genomic objects
#'
#' This is the main function of the GenomicCoordinates package. It automatically
#' detects the most appropriate object type based on the input string format
#' and returns the corresponding Bioconductor object.
#'
#' @param x Character string or vector of genomic coordinates
#' @param force_class Optional class to force
#'   ("GRanges", "GPos", "GInteractions", "IRanges")
#' @return GRanges, GPos, GInteractions, or IRanges object
#' @export
#'
#' @examples
#' # Auto-detection examples
#' GenomicCoordinates("chr1:1000-2000")           # Returns GRanges
#' GenomicCoordinates("chr1:1000")                # Returns GPos  
#' GenomicCoordinates("chr1:1-10|chr2:4-40")      # Returns GInteractions
#' GenomicCoordinates("1000-2000")               # Returns IRanges
#'
#' # Force specific class
#' GenomicCoordinates("chr1:1000", force_class = "GRanges")
#'
#' # Enhanced format support
#' GenomicCoordinates("chr1:100,000-200,000")     # Comma-separated
#' GenomicCoordinates("chr1 1000 2000")           # Space-delimited
GenomicCoordinates <- function(x, force_class = NULL) {
    # Input validation
    if (is.null(x) || (length(x) == 1 && is.na(x))) {
        stop("Input cannot be NULL or NA")
    }
    
    # Check for numeric input (should error)
    if (is.numeric(x)) {
        stop("Numeric input not supported. Please provide character strings.")
    }
    
    # Convert factors to character
    if (is.factor(x)) {
        x <- as.character(x)
    }
    
    # Ensure input is character
    if (!is.character(x)) {
        stop("Input must be a character vector")
    }
    
    if (length(x) == 0) {
        if (!is.null(force_class)) {
            return(switch(force_class,
                "GRanges" = GRanges(),
                "GPos" = GPos(),
                "GInteractions" = GInteractions(),
                "IRanges" = IRanges(),
                stop("Unknown force_class: ", force_class)
            ))
        }
        return(GRanges())  # Default empty object
    }
    
    # If force_class is specified, use it directly
    if (!is.null(force_class)) {
        return(switch(force_class,
            "GRanges" = as_granges(x),
            "GPos" = as_gpos(x),
            "GInteractions" = as_ginteractions(x),
            "IRanges" = as_iranges(x),
            stop("Unknown force_class: ", force_class)
        ))
    }
    
    ## Otherwise, detect the class based on the input format
    # ...... Check for GInteractions pattern (contains |)
    if (any(grepl("\\|", x))) {
        return(as_ginteractions(x))
    }
    
    # ...... Check if any string lacks chromosome information (IRanges only)
    lacks_chr <- any(vapply(x, function(s) {
        # Simple heuristic: if no colon and no
        # space-separated chr, likely IRanges
        s <- trimws(s)
        s <- gsub("\\s+", " ", s)  # Normalize spacing
        
        # Check if it's a simple numeric range without chromosome info
        if (!grepl(":", s)) {
            # If it has space and starts with a number,
            # check if first part looks like coordinates
            if (grepl("\\s", s)) {
                parts <- strsplit(s, "\\s+")[[1]]
                # If first part is numeric (not chr name), it's likely IRanges
                return(grepl("^[0-9,]+$", parts[1]))
            } else {
                # Single number or range without spaces - likely IRanges
                return(grepl("^[0-9,.-]+$", s))
            }
        }
        return(FALSE)
    }, logical(1)))

    if (lacks_chr) {
        return(as_iranges(x))
    }
    
    # ....... Parse strings to determine if they represent single positions
    parsed_list <- lapply(seq_along(x), function(i) {
        tryCatch(
            .parse_genomic_string(x[i]),
            error = function(e) {
                if (is.na(x[i])) {
                    label <- "NA"
                } else if (nchar(x[i]) == 0) {
                    label <- "<empty string>"
                } else {
                    label <- x[i]
                }
                stop(
                    "Unable to parse element ",
                    i, " ('", label, "'): ",
                    e$message, call. = FALSE
                )
            }
        )
    })
    all_single <- all(vapply(
        parsed_list,
        function(p) isTRUE(p$single),
        logical(1)
    ))

    # Return GPos for single positions, GRanges for ranges
    if (all_single) {
        return(as_gpos(x))
    } else {
        return(as_granges(x))
    }
}

# Create aliases
GCoordinates <- GenomicCoordinates

#' Detect the appropriate class for genomic strings
#'
#' Utility function to determine what class a genomic string should be
#' parsed as, without actually performing the parsing.
#'
#' @param x Character string or vector
#' @return Character vector of predicted classes
#' @export
#'
#' @examples
#' detect_genomic_class("chr1:1000-2000")
#' detect_genomic_class("chr1:1000")
#' detect_genomic_class(c("chr1:1-10|chr2:20-30", "1000-2000"))
detect_genomic_class <- function(x) {
    result <- vapply(x, function(s) {
        # Handle edge cases that should return "error"
        if (is.null(s) || is.na(s) || nchar(trimws(s)) == 0 || 
            s == " " || s == ":" || s == "|" || 
            grepl("^:.*|.*:$", s) || grepl("^\\|.*|.*\\|$", s)) {
            return("error")
        }
        
        # GInteractions
        if (grepl("\\|", s)) {
            return("GInteractions")
        }
        
        # IRanges (no chromosome info)
        s <- trimws(s)
        s <- gsub("\\s+", " ", s)  # Normalize spacing
        
        if (!grepl(":", s)) {
            # Check if it's a simple numeric range without chromosome info
            if (grepl("\\s", s)) {
                parts <- strsplit(s, "\\s+")[[1]]
                # If first part is numeric (not chr name), it's likely IRanges
                if (grepl("^[0-9,]+$", parts[1])) {
                    return("IRanges")
                }
            } else {
                # Single number or range without spaces - likely IRanges
                if (grepl("^[0-9,.-]+$", s)) {
                    return("IRanges")
                }
            }
        }
        
        # Try to detect single position vs range
        tryCatch({
            parsed <- .parse_genomic_string(s)
            if (isTRUE(parsed$single)) {
                return("GPos")
            } else {
                return("GRanges")
            }
        }, error = function(e) {
            return("error")  # Return error instead of fallback
        })
    }, character(1))
    
    # Remove names to return unnamed vector
    unname(result)
}
