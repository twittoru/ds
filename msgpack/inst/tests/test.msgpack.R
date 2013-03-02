test_that("identicalness",{
  p <- new("packer")
  u <- new("unpacker",string_mode=T)
  I.msgpack <- function(x) u$unpack(p$pack(x))
  test_identical <- list(
    c(NA,1),
    Filter(function(x) class(x) != "environment",options()),
    .Machine
  )
  for(obj in test_identical) {
    expect_equal(
      I.msgpack(obj),
      #I(obj)
      #I() returns object of "AsIs" class
      obj
    )
  }
})
test_that("connection",{
  p <- new("packer")
  u <- new("unpack_con",string_mode=T)
  I.msgpack <- function(x) u$unpack(p$pack(x))
  test_identical <- list(
    c(NA,1),
    Filter(function(x) class(x) != "environment",options()),
    .Machine
  )
  for(obj in test_identical) {
    expect_equal(
      I.msgpack(obj),
      #I(obj)
      #I() returns object of "AsIs" class
      obj
    )
  }
})
