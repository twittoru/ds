setRefClass("unpacker",
  fields = list(
    .seek.set = "numeric",
    .seek.cur = "numeric",
    .seek.end = "numeric",
    .offset   = "numeric",
    string_mode = "logical"
  ),
  methods = list(
    initialize = function(string_mode=F) {
      .seek.set <<- 0
      .seek.cur <<- 1
      .seek.end <<- 2
      .offset   <<- 1 # 1-origin
      string_mode <<- string_mode
    },
    .seek = function(offset, whence = .seek.set) {
      if (whence == .seek.set) {
        .offset <<- offset
      } else if (whence == .seek.cur) {
        .offset <<- .offset + offset
      } else if (whence == .seek.end) {
        stop("seek.end is not supported")
      } else {
        stop("invalid whence")
      }
    },
    .reset = function() {
      .seek(1, .seek.set)
    },
    unpack = function(binary) {
      if (.offset == 1) {
        if (is.character(binary)) {
          binary <- charToRaw(binary)
        }
        on.exit(.reset())
      }

      currentByte <- .getByte(binary)
      if (currentByte < 0x80) {
        # Positive FixNum (unsigned int 8)
        value <- as.integer(currentByte)
      } else if (currentByte < 0x90) {
        # FixMap
        n <- as.integer(currentByte & as.raw(0x0f))
        value <- .makeMap(binary, n)
      } else if (currentByte < 0xa0) {
        # FixArray
        n <- as.integer(currentByte & as.raw(0x0f))
        value <- .makeArray(binary, n)
      } else if (currentByte < 0xc0) {
        # FixRaw
        n <- as.integer(currentByte & as.raw(0x1f))
        value <- .makeRaw(binary, n)
      } else if (currentByte == 0xc0) {
        # nil
        value <- NA
      } else if (currentByte == 0xc1) {
        # reserved
      } else if (currentByte == 0xc2) {
        # false
        value <- FALSE
      } else if (currentByte == 0xc3) {
        # true
        value <- TRUE
      } else if (currentByte < 0xca) {
        # reserved
      } else if (currentByte == 0xca) {
        # float
        value <- .makeFloat(binary)
      } else if (currentByte == 0xcb) {
        # double
        value <- .makeDouble(binary)
      } else if (currentByte == 0xcc) {
        # uint 8
        value <- .makeUint(binary, 8)
      } else if (currentByte == 0xcd) {
        # uint 16
        value <- .makeUint(binary, 16)
      } else if (currentByte == 0xce) {
        # uint 32
        value <- .makeUint(binary, 32)
      } else if (currentByte == 0xcf) {
        # uint 64
        value <- .makeUint(binary, 64)
      } else if (currentByte == 0xd0) {
        # int 8
        value <- .makeInt(binary, 8)
      } else if (currentByte == 0xd1) {
        # int 16
        value <- .makeInt(binary, 16)
      } else if (currentByte == 0xd2) {
        # int 32
        value <- .makeInt(binary, 32)
      } else if (currentByte == 0xd3) {
        # int 64
        value <- .makeInt(binary, 64)
      } else if (currentByte < 0xda) {
        # reserved
      } else if (currentByte == 0xda) {
        # raw 16
        n <- .getLength(binary, 2)
        value <- .makeRaw(binary, n)
      } else if (currentByte == 0xdb) {
        # raw 32
        n <- .getLength(binary, 4)
        value <- .makeRaw(binary, n)
      } else if (currentByte == 0xdc) {
        # array 16
        n <- .getLength(binary, 2)
        value <- .makeArray(binary, n)
      } else if (currentByte == 0xdd) {
        # array 32
        n <- .getLength(binary, 4)
        value <- .makeArray(binary, n)
      } else if (currentByte == 0xde) {
        # map 16
        n <- .getLength(binary, 2)
        value <- .makeMap(binary, n)
      } else if (currentByte == 0xdf) {
        # map 32
        n <- .getLength(binary, 4)
        value <- .makeMap(binary, n)
      } else {
        # Negative FixNum (int 8)
        value <- packBits(rawToBits(c(currentByte, rep(as.raw(0xff), 3))), type = "integer")
      }
      return(value)
    },
    .getByte = function(binary) {
      byte <- binary[.offset]
      .seek(1, .seek.cur)
      return(byte)
    },
    .getKey = function(binary) {
      key <- unpack(binary)
      if(class(key) == "raw") {
        key <- rawToChar(key)
      }
      len <- length(unlist(key))
      if(len > 1) {
        key <- paste(key,collapse="")
        warning(sprintf("transform invalid key(length:%d) into %s",len,key))
      }
      return(key)
    },
    .checkTypes = function(values) {
      types <- unique(unlist(lapply(values, deepclass)))
      if (length(types) == 1) {
        class(values) <- types
      }
      return(values)
    },
    .getLength = function(binary, read.bytes) {
      n <- packBits(rawToBits(c(binary[(read.bytes - 1):0 + .offset], rep(as.raw(0x00), 4 - read.bytes))), type = "integer")
      .seek(read.bytes, .seek.cur)
      return(n)
    },
    .makeUint = function(binary, bit) {
      readBytes <- bit / 8
      if (bit == 8) {
        value <- binary[.offset]
      } else if (bit %in% c(16, 32, 64)) {
        value <- as.numeric(paste0("0x", paste(binary[0:(readBytes - 1) + .offset], collapse = "")))
      } else {
        stop("invalid type")
      }
      if (value <= .Machine$integer.max) {
        value <- as.integer(value)
      }
      .seek(readBytes, .seek.cur)
      return(value)
    },
    .makeInt = function(binary, bit) {
      readBytes <- bit / 8
      bits_ <- rawToBits(c(binary[(readBytes - 1):0 + .offset]))
      if (bit == 8 || bit == 16 || bit == 32) {
        bits <- c(bits_,rawToBits(rep(as.raw(ifelse(bits_[bit], 0xff, 0x00)), 4 - readBytes)))
        if (bits[length(bits)] == 1 && all(bits[-length(bits)] == 0)) {
          # as.integer(INT32_MIN) will return NA with error
          # 1 + INT32_MIN is the minimum number that can be represent.
          value <- -2^31
        } else {
          value <- packBits(bits, type = "integer")
        }
        value <- packBits(bits, type = "integer")
      } else if (bit == 64) {
        #TODO: use int64 or gmp
        negpos <- ifelse(bits_[bit],0x10000000000000000,0)
        value <- as.numeric(paste0("0x", paste(binary[0:7 + .offset], collapse = ""))) - negpos
      } else {
        stop("invalid type")
      }
      .seek(readBytes, .seek.cur)
      return(value)
    },
    .makeFloat = function(binary) {
      value <- .binToFloat(binary[0:3 + .offset])
      .seek(4, .seek.cur)
      return(value)
    },
    .makeDouble = function(binary) {
      value <- .binToDouble(binary[0:7 + .offset])
      .seek(8, .seek.cur)
      return(value)
    },
    .makeRaw = function(binary, n) {
      if(n > 0) {
        chars <- binary[0:(n - 1) + .offset]
        .seek(n, .seek.cur)
      } else {
        chars <- as.raw(0x00)
      }
      if(string_mode) chars <- rawToChar(chars)
      return(chars)
    },
    .makeArray = function(binary, n) {
      arr <- vector(mode = "list", n)
      for (i in 1:n) {
        arr[[i]] <- unpack(binary)
      }
      arr <- .checkTypes(arr)
      return(arr)
    },
    .makeMap = function(binary, n) {
      map <- vector(mode = "list", n)
      keys <- character(n)
      for (i in 1:n) {
        keys[i] <- .getKey(binary)
        map[[i]] <- unpack(binary)
      }
      names(map) <- keys
      map <- .checkTypes(map)
      return(map)
    },
    .binToDouble = function(binary) {
      return(.binToNumeric(binary, 11, 52, 1023))
    },
    .binToFloat = function(binary) {
      return(.binToNumeric(binary, 8, 23, 127))
    },
    .binToNumeric = function(binary, expSize, fracSize, bias) {
      bits <- rev(rawToBits(rev(binary)))
      sign <- as.integer(bits[1])
      exp <- packBits(c(bits[rev(seq.int(2, length = expSize))], rep(as.raw(0), 32 - expSize)), type = "integer")
      frac <- as.numeric(paste0("0x", paste(binary[-1] & as.raw(c(0x0f, rep(0xff, length(binary) - 2))), collapse = "")))

      # special cases
      if (exp == 0) {
        if (frac == 0) {
          return(0)
        } else {
          # subnormal numbers
          return((-1)^sign * frac * 2^(- fracSize - bias + 1))
        }
      } else if (exp == 2^expSize - 1) {
        if (frac == 0) {
          return((-1)^sign * Inf)
        } else {
          return(NaN)
        }
      }
      return((-1)^sign * (2^fracSize + frac) * 2^(exp - fracSize - bias))
    }
  )
)
