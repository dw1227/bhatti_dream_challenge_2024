
ora <- function(target_ids, universe_ids, id_sets, set_names = NULL) {
  
  # Remove NAs from target and universe ID lists
  target_ids <- target_ids[!is.na(target_ids)]
  universe_ids <- universe_ids[!is.na(universe_ids)]
  
  # Initialize result vectors
  gns <- NULL   # Genes in the pathway
  OR <- NULL    # Odds ratio
  pv <- NULL    # p-value
  ns <- NULL    # Number of target IDs in the pathway
  Size <- NULL  # Size of the pathway
  
  # Iterate through each ID set in id_sets
  for (gs in 1:length(id_sets)) {
    
    # Calculate intersection of the current set and the universe of tested IDs
    path <- intersect(id_sets[[gs]], universe_ids)
    noMy <- length(intersect(target_ids, path))  # Number of target IDs in this pathway
    
    # Store gene IDs found in the pathway as a single concatenated string
    gns <- c(gns, paste(target_ids[target_ids %in% path], collapse = ";"))
    
    # Perform ORA if there is at least one target ID in the pathway
    if (noMy >= 1) {
      q <- noMy
      m <- length(path)
      n <- length(universe_ids) - length(path)
      k <- length(target_ids)
      
      # Compute hypergeometric p-value
      pv <- c(pv, phyper(q = noMy - 1, m = m, n = n, k = k, lower.tail = FALSE))
      
      # Compute odds ratio using Fisher's exact test
      OR <- c(OR, fisher.test(matrix(c(q, k - q, m - q, n - k + q), 2, 2))$estimate)

      # Store count and size
      ns <- c(ns, noMy)
      Size <- c(Size, m)
    } else {
      # No enrichment found for this pathway
      pv <- c(pv, NA)
      OR <- c(OR, NA)
      ns <- c(ns, 0)
      Size <- c(Size, 0)
    }
  }
  
  # If set names are not provided, use default names
  if (is.null(set_names)) {
    set_names <- names(id_sets)
  }
  
  # Create result dataframe
  res <- data.frame(
    ID = names(id_sets),
    Name = set_names,
    Count = ns,
    Size = Size,
    OddsRatio = round(OR, 1),
    Pvalue = pv,
    Genes = gns
  )
  
  # Remove NA results and filter for pathways with at least 3 target IDs
  res <- na.omit(res)
  res <- res[res$Count >= 3, ]
  
  # Adjust p-values using FDR
  res$q <- p.adjust(res$Pvalue, "fdr")
  
  # Order results by p-value
  res <- res[order(res$Pvalue), ]
  
  # Return selected columns
  return(res[, c("Name", "Count", "Size", "OddsRatio", "q", "Pvalue", "Genes")])
}
