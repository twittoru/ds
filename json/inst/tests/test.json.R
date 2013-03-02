test_that("identicalness",{
  p <- new("packer")
  u <- new("unpacker")
  I.msgpack <- function(x) u$unpack(p$pack(x))
  test_identical <- list(
    c(NA,1),
    Filter(function(x) class(x) != "environment",options()),
    lapply(.Machine,signif) # pack-time rounding deprives precision of number
  )
  for(obj in test_identical) {
    expect_equivalent(
      I.msgpack(obj),
      #I(obj)
      #I() returns object of "AsIs" class
      obj
    )
  }
})
if(0) {
test_that("identicalness",{
  jsonfile <- file("~/json2")
  json <- paste(readLines(jsonfile),collapse="\n")
  close(jsonfile)
  p <- new("packer")
  u <- new("unpacker")
  I.msgpack <- function(x) p$pack(u$unpack(x))
  cat(I.msgpack(json),file="~/json3")
  expect_equivalent(
    I.msgpack(json),
    json
  )
})
}
