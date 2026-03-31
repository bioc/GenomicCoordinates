#' GenomicCoordinates: Enhanced string parsing for genomic
#' coordinates
#'
#' The GenomicCoordinates package extends the string parsing
#' capabilities for genomic coordinates in Bioconductor. It
#' supports various string formats including comma-separated
#' numbers, space-delimited coordinates, and automatically
#' detects whether to return GRanges, GPos, or GInteractions
#' objects.
#'
#' @section Supported formats:
#' \itemize{
#'   \item Standard format: "chr1:1000-2000", "chr1:1000-2000:+"
#'   \item Comma-separated: "chr1:1,000-2,000", "chr1:1,000,000-2,000,000"
#'   \item Space-delimited: "chr1 1000 2000"
#'   \item Single positions: "chr1:1000" (returns GPos)
#'   \item Interactions: "chr1:1000-2000|chr2:3000-4000" (returns GInteractions)
#' }
#'
#' @section Main functions:
#' \itemize{
#'   \item \code{GenomicCoordinates(x)}: Main function -
#'     auto-detect and convert to appropriate type
#'   \item \code{detect_genomic_class(x)}: Detect
#'     appropriate class without parsing
#'   \item \code{as_granges(x)}: Convert character to GRanges
#'   \item \code{as_gpos(x)}: Convert character to GPos
#'   \item \code{as_iranges(x)}: Convert character to IRanges
#'   \item \code{as_ginteractions(x)}: Convert character to GInteractions
#' }
#'
#' @keywords internal
"_PACKAGE"

#' @import GenomicRanges IRanges S4Vectors InteractionSet methods
#' @importFrom plyranges as_granges as_iranges
#' @importFrom plyinteractions as_ginteractions
NULL
