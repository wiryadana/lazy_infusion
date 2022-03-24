dose <- seq(from=0.1, to=2, by=0.1)
bb <- seq(from=40,to=100,by=5)


A = matrix(NA, nrow = length(dose), ncol = length(bb))
for (i in 1:length(dose)) {
  for (j in 1:length(bb)) {
    A[i,j] = print(dose[i] * 60 * bb[j] *50 / 4000)
  }
}
colnames(A) = bb
rownames(A) = dose

B = as.data.frame(A, row.names = dose)
