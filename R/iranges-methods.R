#' Enhanced IRanges parsing
#'
#' Extensions to IRanges parsing to handle comma-separated numbers and
#' space-delimited coordinates.

# Override the existing coercion for enhanced parsing
#' @name GenomicCoordinates-IRanges

setAs("character", "IRanges", function(from) {
    if (length(from) == 0) {
        return(IRanges())
    }
    
    # Handle vector of strings
    ranges_list <- lapply(from, function(x) {
        # If string contains chromosome info, extract just the coordinates
        if (grepl(":", x)) {
            parts <- strsplit(x, ":")[[1]]
            if (length(parts) >= 2) {
                # Take the coordinate part (second element)
                coord_part <- parts[2]
                # Remove strand info if present (third element)
                if (length(parts) >= 3) {
                    coord_part <- parts[2]
                }
                coords <- .parse_coordinates(coord_part)
            } else {
                coords <- .parse_coordinates(x)
            }
        } else {
            coords <- .parse_coordinates(x)
        }
        IRanges(start = coords$start, end = coords$end)
    })
    
    # Combine all ranges
    do.call(c, ranges_list)
})
