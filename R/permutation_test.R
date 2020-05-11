
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
	cluster_identity = NA,
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

	meta_data[, clusters := as.character(clusters)]
	cluster_cases <- unique(meta_data[["clusters"]])

	## Get observed differences in fraction.
	obs_diff <- meta_data[, .(count = .N), by = .(samples, clusters)]
	obs_diff[, fraction := count / sum(count), by = samples]
	obs_diff <- dcast(obs_diff, clusters ~ samples, value.var = "fraction")
	obs_diff[, obs_log2FD := log2(get(sample_2)) - log2(get(sample_1))]

	## Permutation test.
	perm_results <- matrix(NA, nrow(obs_diff), n_permutations)
	rownames(perm_results) <- sort(cluster_cases)

	for (i in seq_len(n_permutations)) {
		permuted <- copy(meta_data)
		permuted[["samples"]] <- sample(permuted[["samples"]])
		permuted <- permuted[, .(count = .N), by = .(samples, clusters)]
		permuted <- permuted[
			CJ(samples = samples, clusters = cluster_cases, unique = TRUE),
			on = .(samples, clusters)
		][
			is.na(count), count := 0
		][]
		permuted[, fraction := count / sum(count), by = samples]
		permuted <- dcast(permuted, clusters ~ samples, value.var = "fraction")
		permuted[, perm_log2FD := log2(get(sample_2)) - log2(get(sample_1))]

		perm_results[, i] <- permuted[["perm_log2FD"]]
	}

	increased <- rowSums(apply(perm_results, 2, function(x) obs_diff[["obs_log2FD"]] < x))
	increased <- (increased + 1) / (n_permutations + 1)

	decreased <- rowSums(apply(perm_results, 2, function(x) obs_diff[["obs_log2FD"]] > x))
	decreased <- (decreased + 1) / (n_permutations + 1)

	obs_diff[, pval := ifelse(obs_log2FD > 0, increased[.I], decreased[.I])]
	obs_diff[, FDR := p.adjust(pval, "fdr")]

	## Boostrap log2FD CI.
	boot_results <- matrix(NA, nrow(obs_diff), n_permutations)
	rownames(boot_results) <- sort(cluster_cases)

	for (i in seq_len(n_permutations)) {
		booted <- copy(meta_data)
		booted[, clusters := sample(clusters, replace = TRUE), by = samples]
		booted <- booted[, .(count = .N), by = .(samples, clusters)]
		booted <- booted[
			CJ(samples = samples, clusters = cluster_cases, unique = TRUE),
			on = .(samples, clusters)
		][
			is.na(count), count := 0
		][]
		booted[, fraction := count / sum(count), by = samples]
		booted <- dcast(booted, clusters ~ samples, value.var = "fraction")
		booted[, boot_log2FD := log2(get(sample_2)) - log2(get(sample_1))]

		boot_results[, i] <- booted[["boot_log2FD"]]
	}

	boot_mean <- rowMeans(boot_results, na.rm = TRUE)
	boot_ci <- t(apply(boot_results, 1, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)))
	boot_ci <- as.data.table(boot_ci)
	setnames(boot_ci, old = c(1, 2), new = c("boot_CI_2.5", "boot_CI_97.5"))

	obs_diff[, boot_mean_log2FD := boot_mean]
	obs_diff <- cbind(obs_diff, boot_ci)

	## Store results and return object.
	sc_utils_obj@results$permutation <- obs_diff
	return(sc_utils_obj)
}
