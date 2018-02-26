
df1 <- data.frame(
  X = sample(1:10),
  Y = sample(c("yes", "no"), 10, replace = TRUE)
)


dput(df1)


x <- structure(list(X = c(4L, 3L, 2L, 5L, 9L, 1L, 8L, 10L, 6L, 7L), 
                    Y = structure(c(2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L), .Label = c("no", 
                "yes"), class = "factor")), .Names = c("X", "Y"), row.names = c(NA, 
                  -10L), class = "data.frame")

x




#dput(df1, "xdf")
# getback <- dget("xdf")