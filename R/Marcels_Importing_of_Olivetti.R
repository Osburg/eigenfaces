# Read Olivetti
Olivetti_orig <- read.csv("olivetti_X.csv")

# Convert to 399x64x64 Matrix
Oli_db <- as.double(as.matrix(Olivetti_orig))
Oli_plot <- as.matrix(Oli_db)
dim(Oli_plot) <- c(399,64,64)

# Flip all the images upside down
for (i in 1:399) {
  img <- apply(Oli_plot[i,,],1, rev)
  img <- apply(img,1,rev)
  Oli_plot[i,,] <- img
}

# Plot example in Gray
image(Oli_plot[16,,], col = hcl.colors(12, "Grays", rev = FALSE))




