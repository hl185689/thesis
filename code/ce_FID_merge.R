full <-read.csv("C:/Users/conor/Desktop/thesis/data/fullfidce.csv")
probs2 <- read.csv("C:/Users/conor/Desktop/thesis/data/probability_comparison_all2.csv")

head(full_probs)

full_probs <- merge(probs2,full, by = "FID")


write.csv(full_probs, file = "C:/Users/conor/Desktop/thesis/data/probability_tiers2.csv", row.names = FALSE)
