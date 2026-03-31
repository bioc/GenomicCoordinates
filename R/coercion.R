#' Conversion methods for genomic coordinates
#'
#' Methods to convert character strings to GRanges, GPos,
#' and GInteractions objects with support for various string formats
#' including comma-separated numbers and space-delimited coordinates.
#'
#' @param .data A character vector of genomic coordinate strings
#' @param ... Additional arguments (unused)
#' @param keep_mcols Ignored for character input (included for
#'   generic compatibility with plyranges)
#' @param keep.extra.columns Ignored for character input (included for
#'   generic compatibility with plyinteractions)
#' @param starts.in.df.are.0based Ignored for character input (included for
#'   generic compatibility with plyinteractions)
#' @return The appropriate Bioconductor object type
#'
#' @examples
#' # GRanges conversion
#' as_granges("chr1:1000-2000")
#' as_granges("chr1:1,000-2,000:+")
#' as_granges(c("chr1:1000-2000", "chr2:3000-4000"))
#'
#' # GPos conversion
#' as_gpos("chr1:1000")
#' as_gpos(c("chr1:1000", "chr2:2000"))
#'
#' # GInteractions conversion
#' as_ginteractions("chr1:1-10|chr2:20-30")
#'
#' @name coercion
NULL

#' Convert to GPos object
#'
#' Converts character strings representing single genomic positions
#' to GPos objects.
#'
#' @param .data A character vector of genomic position strings
#' @param ... Additional arguments (unused)
#' @return A GPos object
#' @export
#'
#' @examples
#' as_gpos("chr1:1000")
#' as_gpos("chr1:1,000:+")
#' as_gpos(c("chr1:1000", "chr2:2000", "chr3:3000"))
setGeneric("as_gpos", function(.data, ...) {
    standardGeneric("as_gpos")
})

#' @rdname coercion
#' @export
setMethod("as_granges", "character", function(.data, ...) {
    if (length(.data) == 0) {
        return(GRanges())
    }

    # Handle vector of strings
    parsed_list <- lapply(.data, .parse_genomic_string)

    # Check if any are single positions (should be GPos)
    has_single <- any(vapply(
        parsed_list,
        function(x) isTRUE(x$single),
        logical(1)
    ))

    if (has_single && length(.data) == 1) {
        # Convert single position to GRanges for compatibility
        parsed <- parsed_list[[1]]
        return(GRanges(
            seqnames = parsed$seqnames,
            ranges = IRanges(start = parsed$start, end = parsed$end),
            strand = parsed$strand
        ))
    }

    # Extract components
    seqnames <- vapply(parsed_list, function(x) x$seqnames, character(1))
    starts <- vapply(parsed_list, function(x) x$start, numeric(1))
    ends <- vapply(parsed_list, function(x) x$end, numeric(1))
    strands <- vapply(parsed_list, function(x) x$strand, character(1))

    # Create GRanges
    GRanges(
        seqnames = seqnames,
        ranges = IRanges(start = starts, end = ends),
        strand = strands
    )
})

#' @rdname coercion
#' @export
setMethod("as_gpos", "character", function(.data, ...) {
    if (length(.data) == 0) {
        return(GPos())
    }

    # Parse and check if all are single positions
    parsed_list <- lapply(.data, .parse_genomic_string)

    # Extract components
    seqnames <- vapply(parsed_list, function(x) x$seqnames, character(1))
    positions <- vapply(parsed_list, function(x) x$start, numeric(1))
    strands <- vapply(parsed_list, function(x) x$strand, character(1))

    # Create GPos
    GPos(
        seqnames = seqnames,
        pos = positions,
        strand = strands
    )
})

#' @rdname coercion
#' @export
setMethod("as_ginteractions", "character", function(.data, ...) {
    if (length(.data) == 0) {
        return(GInteractions())
    }

    # Handle vector of strings
    parsed_list <- lapply(.data, function(x) {
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
    anchor1_seqnames <- vapply(
        parsed_list, function(x) x$anchor1$seqnames,
        character(1)
    )
    anchor1_starts <- vapply(
        parsed_list, function(x) x$anchor1$start,
        numeric(1)
    )
    anchor1_ends <- vapply(
        parsed_list, function(x) x$anchor1$end,
        numeric(1)
    )
    anchor1_strands <- vapply(
        parsed_list, function(x) x$anchor1$strand,
        character(1)
    )

    # Extract anchor2 components
    anchor2_seqnames <- vapply(
        parsed_list, function(x) x$anchor2$seqnames,
        character(1)
    )
    anchor2_starts <- vapply(
        parsed_list, function(x) x$anchor2$start,
        numeric(1)
    )
    anchor2_ends <- vapply(
        parsed_list, function(x) x$anchor2$end,
        numeric(1)
    )
    anchor2_strands <- vapply(
        parsed_list, function(x) x$anchor2$strand,
        character(1)
    )

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

    # Standardize seqlevels before creating GInteractions
    all_seqlevels <- union(
        seqlevels(anchor1), seqlevels(anchor2)
    )
    seqlevels(anchor1) <- all_seqlevels
    seqlevels(anchor2) <- all_seqlevels

    # Create GInteractions
    GInteractions(anchor1, anchor2)
})
