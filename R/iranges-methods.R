#' Enhanced IRanges parsing
#'
#' Extensions to IRanges parsing to handle comma-separated numbers and
#' space-delimited coordinates.
#'
#' @param .data A character vector of coordinate strings
#' @param ... Additional arguments (unused)
#' @return An IRanges object
#'
#' @examples
#' as_iranges("1000-2000")
#' as_iranges("1,000-2,000")
#' as_iranges(c("100-200", "300-400"))
#'
#' @rdname coercion
#' @export
setMethod("as_iranges", "character", function(.data, ...) {
    if (length(.data) == 0) {
        return(IRanges())
    }

    # Handle vector of strings
    ranges_list <- lapply(.data, function(x) {
        # If string contains chromosome info, extract just the coordinates
        if (grepl(":", x)) {
            parts <- strsplit(x, ":")[[1]]
            if (length(parts) >= 2) {
                # Take the coordinate part (second element)
                coord_part <- parts[2]
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
