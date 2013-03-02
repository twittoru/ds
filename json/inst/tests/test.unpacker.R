test_that("number",{
  msg <- new("unpacker")
  expect_equal(
    msg$unpack("1")
    ,1
  )
})
test_that("array",{
  msg <- new("unpacker")
  expect_equal(
    msg$unpack("[1, 2,3,4,5]")
    ,c(1,2,3,4,5)
  )
})
test_that("hash",{
  msg <- new("unpacker")
  expect_equal(
    msg$unpack('{"a" : "1", "b" : false, "c" : 0}')
    ,list(a="1",b=F,c=0)
  )
})
