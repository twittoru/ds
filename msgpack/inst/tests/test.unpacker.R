test_int <- list(
  list(
    name = "makeUInt8",
    max = 255,
    min = 0,
    length = 8,
    prefix = 0xcc
  ),
  list(
    name = "makeUInt16",
    max = 65535,
    min = 0,
    length = 16,
    prefix = 0xcd
  ),
  list(
#TODO: UINT32_MAX is lesser than .Machine$integer.max
    name = "makeUInt32",
    max = 2147483647,
    min = 0,
    length = 32,
    prefix = 0xce
  ),
  list(
    name = "makeInt8",
    max = 127,
    min = -128,
    length = 8,
    prefix = 0xd0
  ),
  list(
    name = "makeInt16",
    max = 32767,
    min = -32768,
    length = 16,
    prefix = 0xd1
  ),
  list(
    name = "makeInt32",
    max = 2147483647,
    min = -2147483648,
    length = 32,
    prefix = 0xd2
  )
)

test_that("makeInt",{
  msg <- new("unpacker")
  for(cond in test_int) {
    objects <- round(runif(10,min=(cond$min), max=(cond$max)))
    expect_equal(
      objects,
      sapply(objects,function(object) msg$unpack(c(as.raw(cond$prefix),rev(packBits(intToBits(object)[1:cond$length])))))
      ,info = cond$name
    )
  }
})
test_that("makeInt64",{
  object <- rep(as.raw(0x01),8)
  msgpack_string <- c(as.raw(0xd3),object)
  msg <- new("unpacker")
  expect_equal(
    msg$unpack(msgpack_string),
    as.numeric(paste(c("0x",object),collapse=""))
  )
})
test_that("makeUInt64",{
  object <- round(runif(8,max=0xff,min=0))
  msgpack_string <- c(as.raw(0xcf),object)
  msg <- new("unpacker")
  expect_equal(
    msg$unpack(msgpack_string),
    as.numeric(paste(c("0x",object),collapse=""))
  )
})
test_that("makeBoolean",{
  bool <- c(TRUE,FALSE,NA)
  object <- as.raw(c(0xc3,0xc2,0xc0))
  msg <- new("unpacker")
  expect_equal(
    sapply(object,msg$unpack),
    bool
  )
})
test_that("makeFixnum",{
  fixnum <- c(0:127,-32:-1)
  object <- sapply(fixnum, function(x) packBits(intToBits(x)[1:8]))
  msg <- new("unpacker")
  expect_equal(
    sapply(object,msg$unpack),
    fixnum
  )
})
if(0) {
test_that("makeArray",{
# tooooooo slow
  msg <- new("unpack_con")
# rep(NA,2^17)
  expect_equal(
  length(msg$unpack(as.raw(c(221,0,2,0,0,rep(192,131072))))),
  131072
  )
# rep(NA,2^16-1)
  expect_equal(
  length(msg$unpack(as.raw(c(220,255,255,rep(192,65535))))),
  65535
  )
})
}
