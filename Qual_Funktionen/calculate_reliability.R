calculate_reliability <- function(A_data, B_data) {
  categories <- colnames(A_data)[grepl("HK", colnames(A_data))]

  reliability_results <- sapply(categories, function(cat) {
    A_cat <- A_data[[cat]]
    B_cat <- B_data[[cat]]

    valid_indices <- !is.na(A_cat) & !is.na(B_cat)
    if (sum(valid_indices) == 0) {
      return(NA)
    }

    agreement <- sum(A_cat[valid_indices] == B_cat[valid_indices]) / sum(valid_indices) * 100
    return(agreement)
  })

  results_df <- data.frame(
    Category = categories,
    Agreement = round(reliability_results, 2),
    Valid_Comparisons = sapply(categories, function(cat) {
      sum(!is.na(A_data[[cat]]) & !is.na(B_data[[cat]]))
    })
  )

  print(results_df)
  valid_results <- reliability_results[!is.na(reliability_results)]
  if (length(valid_results) > 0) {
    overall_agreement <- mean(valid_results)
    cat("\nOverall Agreement:", round(overall_agreement, 2), "%\n\n")
    
    if (overall_agreement >= 90) {
      cat("Die Interkoderreliabilität ist erfüllt.\n")
    } else if (overall_agreement < 75) {
      cat("Das Ergebnis erfüllt bislang nicht die Gütekriterien.\n")
    }
    for (i in seq_along(reliability_results)) {
      if (!is.na(reliability_results[i]) && reliability_results[i] < 75) {
        cat(paste0("Die Übereinstimmung für Kategorie ", categories[i], 
                   " erfüllt nicht die Gütekriterien (", 
                   round(reliability_results[i], 2), "%).\n"))
      }
    }
  } else {
    cat("\nKeine gültigen Vergleiche möglich. Bitte überprüfen Sie die Kodierungen.\n")
  }
}