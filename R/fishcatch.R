#'
#' Fish Catch Data
#'
#' This function determines the most common fish, the rarest fish, and the total number of fish
#' @param fish fish catch data
#' @return most common fish, rarest fish, total fish caught
#' 
fish_catch = function(fish) {
# find the most common fish
most_common = names(which.max(summary(as.factor(fish$fish_species))))

# find the rarest fish
rarest_fish = names(which.min(summary(as.factor(fish$fish_species))))
# find the total fish catch
fish_factor = as.factor(fish$fish_species)
fish_total = as.character(fish_factor)
fish_total = as.numeric(fish_total)
total_catch = sum(fish_total)
return(list(most_common = most_common, rarest_fish= rarest_fish, total_catch = total_catch))
}

