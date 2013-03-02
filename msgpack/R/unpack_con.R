setRefClass("unpack_con",
  fields = c(
    "string_mode",
    "stream"
  ),
  methods = list(
    initialize = function(string_mode=F) {
      string_mode <<- string_mode
    },
    unpack = function(binary, remainder=F) {
      if(!remainder) {
        flag <- tryCatch(!isOpen(stream),error = function(e) NULL)
        if (is.null(flag) || flag) {
          if (is.character(binary)) {
            binary <- charToRaw(binary)
          }
          stream <<- rawConnection(binary)
          on.exit({close(stream)})
        }
      }

      currentByte <- readBin(stream, "raw")
      if (currentByte < 0x80) {
        # Positive FixNum (unsigned int 8)
        value <- as.integer(currentByte)
      } else if (currentByte < 0x90) {
        # FixMap
        n <- as.integer(currentByte & as.raw(0x0f))
        value <- .makeMap(n)
      } else if (currentByte < 0xa0) {
        # FixArray
        n <- as.integer(currentByte & as.raw(0x0f))
        value <- .makeArray(n)
      } else if (currentByte < 0xc0) {
        # FixRaw
        n <- as.integer(currentByte & as.raw(0x1f))
        value <- .makeRaw(n)
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
        value <- .makeFloat()
      } else if (currentByte == 0xcb) {
        # double
        value <- .makeDouble()
      } else if (currentByte == 0xcc) {
        # uint 8
        value <- .makeUint(8)
      } else if (currentByte == 0xcd) {
        # uint 16
        value <- .makeUint(16)
      } else if (currentByte == 0xce) {
        # uint 32
        value <- .makeUint(32)
      } else if (currentByte == 0xcf) {
        # uint 64
        value <- .makeUint(64)
      } else if (currentByte == 0xd0) {
        # int 8
        value <- .makeInt(8)
      } else if (currentByte == 0xd1) {
        # int 16
        value <- .makeInt(16)
      } else if (currentByte == 0xd2) {
        # int 32
        value <- .makeInt(32)
      } else if (currentByte == 0xd3) {
        # int 64
        value <- .makeInt(64)
      } else if (currentByte < 0xda) {
        # reserved
      } else if (currentByte == 0xda) {
        # raw 16
        n <- .makeUint(16)
        value <- .makeRaw(n)
      } else if (currentByte == 0xdb) {
        # raw 32
        n <- .makeUint(32)
        value <- .makeRaw(n)
      } else if (currentByte == 0xdc) {
        # array 16
        n <- .makeUint(16)
        value <- .makeArray(n)
      } else if (currentByte == 0xdd) {
        # array 32
        n <- .makeUint(32)
        value <- .makeArray(n)
      } else if (currentByte == 0xde) {
        # map 16
        n <- .makeUint(16)
        value <- .makeMap(n)
      } else if (currentByte == 0xdf) {
        # map 32
        n <- .makeUint(32)
        value <- .makeMap(n)
      } else {
        # Negative FixNum (int 8)
        value <- packBits(rawToBits(c(currentByte, rep(as.raw(0xff), 3))), type = "integer")
      }
      if(!remainder && isIncomplete(stream)) warning("extra bytes exists")
      return(value)
    },
    .getKey = function() {
      key <- unpack(remainder=T)
      if(is.raw(key)) {
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
    .makeUint = function(bit) {
      readBytes <- bit / 8
      if (bit == 8) {
        value <- readBin(stream, "raw")
      } else if (bit %in% c(16, 32, 64)) {
        value <- as.numeric(paste0("0x", paste(readBin(stream, "raw", readBytes), collapse = "")))
      } else {
        stop("invalid type")
      }
      if (value <= .Machine$integer.max) {
        value <- as.integer(value)
      }
      return(value)
    },
    .makeInt = function(bit) {
      readBytes <- bit / 8
      if (bit == 8 || bit == 16 || bit == 32) {
        bits_ <- rawToBits(rev(readBin(stream, "raw", readBytes)))
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
        bits_ <- rawToBits(readBin(stream, "raw", readBytes))
        #TODO: use int64 or gmp
        negpos <- ifelse(bits_[bit], 0x10000000000000000, 0)
        value <- as.numeric(paste0("0x", paste(bits_, collapse = ""))) - negpos
      } else {
        stop("invalid type")
      }
      return(value)
    },
    .makeFloat = function() {
      value <- .binToNumeric(readBin(stream, "raw", 4), 8, 23, 127)
      return(value)
    },
    .makeDouble = function() {
      value <- .binToNumeric(readBin(stream,"raw", 8), 11, 52, 1023)
      return(value)
    },
    .makeRaw = function(n) {
      if(n > 0) {
        chars <- readBin(stream, "raw", n)
      } else {
        chars <- as.raw(0x00)
      }
      if(string_mode) chars <- rawToChar(chars)
      return(chars)
    },
    .makeArray = function(n) {
      arr <- vector(mode = "list", n)
      for (i in 1:n) {
        arr[[i]] <- unpack(remainder=T)
      }
      arr <- .checkTypes(arr)
      return(arr)
    },
    .makeMap = function(n) {
      map <- vector(mode = "list", n)
      keys <- character(n)
      for (i in 1:n) {
        keys[i] <- .getKey()
        map[[i]] <- unpack(remainder=T)
      }
      names(map) <- keys
      map <- .checkTypes(map)
      return(map)
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
