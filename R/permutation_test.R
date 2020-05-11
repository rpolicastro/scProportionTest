
#' Permutation Test For Proportions
#'
#' @param sc_utils_obj sc_utils object
#' @param cluster_identity Column that has cluster names
#' @param sample_1 First sample to compare (ie. control)
#' @param sample_2 Sample to compare to first sample (ie. treatment)
#' @param sample_identity Column that has sample names
#' @param n_permutations Number of permutations
#'
#' @rdname permutation_test-function
#'
#' @export

permutation_test <- function(
	sc_utils_obj,
	sample_1 = NA,
	sample_2 = NA,
	sample_identity = "orig.ident",
	n_permutations = 1000
) {

	## Prepare data.
	meta_data <- copy(sc_utils_obj@meta_data)

	meta_data <- meta_data[
		get(sample_identity) %in% c(sample_1, sample_2),
		c(..sample_identity, ..cluster_identity)
	]

	setnames(
		meta_data,
		old = c(sample_identity, cluster_identity),
		new = c("samples", "clusters")
	)

	## Get observed differences in fraction.
	obs_diff <- meta_data[, .(count = .N), by = .(samples, clusters)]
	obs_diff[, fraction := count / sum(count), by = samples]
	obs_diff <- dcast(obs_diff, clusters ~ samples, value.var = "fraction")
	obs_diff[, obs_log2FD := log2(get(sample_2)) - log2(get(sample_1))]

	## Permutation test.
	perm_results <- matrix(NA, nrow(obs_diff), n_permutations)

	for (i in seq_len(n_permutations)) {
		permuted <- copy(meta_data)
		permuted[["samples"]] <- sample(permuted[["samples"]])
		permuted <- permuted[, .(count = .N), by = .(samples, clusters)]
		permuted <- permuted[
			CJ(samples = samples, clusters = clusters, unique = TRUE),
			on = .(samples, clusters)
		][
			is.na(count), count := 0
		][]
		permuted[, fraction := count / sum(count), by = samples]
		permuted <- dcast(permuted, clusters ~ samples, value.var = "fraction")
		permuted[, perm_log2FD := log2(get(sample_2)) - log2(get(sample_1))]

		perm_results[, i] <- permuted[["perm_log2FD"]]
	}

	increased <- rowSums(apply(perm_results, 2, function(x) obs_diff[["obs_log2FD"]] > x))
	increased <- (increased + 1) / (n_permutations + 1)

	decreased <- rowSums(apply(perm_results, 2, function(x) obs_diff[["obs_log2FD"]] < x))
	decreased <- (increased + 1) / (n_permutations + 1)

	obs_diff[, pval := ifelse(obs_log2FD > 0, increased[.I], decreased[.I])]
	obs_diff[, FDR := p.adjust(pval, "fdr")]

	## Store results and return object.
	sc_utils_obj@results$permutation <- obs_diff
	return(sc_utils_obj)
}
