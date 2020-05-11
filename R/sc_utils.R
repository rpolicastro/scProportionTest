#' Single-Cell Utilities
#'
#' @slot meta_data Seurat meta-data
#' @slot results Results for various analysis
#'
#' @rdname sc_utils-class
#' @export

setClass(
	"sc_utils",
	representation(
		meta_data = "data.table",
		results = "list"
	),
	prototype(
		meta_data = data.frame(),
		results = list()
	)
)

#' Single-Cell Utilities Constructor
#'
#' @import methods
#' @import Seurat
#' @import data.table
#'
#' @param seurat_object Seurat object with meta-data
#'
#' @rdname sc_utils-class
#' @export

sc_utils <- function(seurat_object) {

	## Pull meta-data out of Seurat object.
	metadata <- as.data.table(
		seurat_object[[]],
		keep.rownames = "cell_id"
	)

	## Create new sc_utils object.
	sc_utils_obj <- new(
		"sc_utils",
		meta_data = metadata
	)

	return(sc_utils_obj)
}
