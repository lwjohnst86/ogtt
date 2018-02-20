context("Insulin sensitivity measures")

ds <- data.frame(
    G0 = 3:9,
    G30 = (3:9) + 5,
    G120 = (3:9) + 2,
    I0 = 23:29,
    I30 = (23:29) + 5,
    I120 = (23:29) + 2
)

test_that("Matsuda function takes a different number of blood samples", {
    isi_matsuda(c(ds$G0, ds$G120), c(ds$I0, ds$I120))
})
