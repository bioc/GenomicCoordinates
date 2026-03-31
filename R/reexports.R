#' Re-exported functions from plyranges and plyinteractions
#'
#' These generics are re-exported from plyranges and plyinteractions
#' to provide conversion functions for character strings.
#'
#' @param .data Object to convert
#' @param ... Additional arguments passed to methods
#' @param keep_mcols Logical; whether to keep metadata columns (plyranges)
#' @param keep.extra.columns Logical; whether to keep
#'   extra columns (plyinteractions)
#' @param starts.in.df.are.0based Logical; whether starts
#'   are 0-based (plyinteractions)
#' @return A Bioconductor object
#'
#' @examples
#' as_granges("chr1:1000-2000")
#' as_iranges("1000-2000")
#' as_ginteractions("chr1:1-10|chr2:20-30")
#'
#' @importFrom plyranges as_granges
#' @export
#' @name reexports
plyranges::as_granges

#' @importFrom plyranges as_iranges
#' @export
#' @rdname reexports
plyranges::as_iranges

#' @importFrom plyinteractions as_ginteractions
#' @export
#' @rdname reexports
plyinteractions::as_ginteractions
