library("exams.forge")
n_vals <- c(4, 8, 10, 16, 20, 25, 32)^2
n_vals <- unique(c(seq(100, 1000, by = 100), n_vals))
for (n in n_vals) {
  # compute data
  obj_name <- paste0("sos", n)
  file     <-  file.path(".", paste0(obj_name, ".rda"))
  if (!file.exists(file)) {
    obj <- sumofsquares(n, 10, maxt = Inf, zerosum = TRUE)
    # assign to object with correct name
    assign(obj_name, obj)
    # save into package data directory
    save(list = obj_name,
         file = file.path(".", paste0(obj_name, ".rda")),
         version = 2,
         compress = "bzip2")
    message("Saved ", obj_name)
  }
}