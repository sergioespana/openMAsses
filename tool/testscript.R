vec1 <- c(1,2,3)
vec2 <- c(2,3,4)
vec3 <- c(3,2,1)

mat <- cbind(vec1,vec2,vec3)
print(mat)

df <- data.frame(mat)
ndf <- df[0,]
print(ndf)

rm(vec1, vec2, vec3, df, mat, ndf)
