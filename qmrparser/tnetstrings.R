library(qmrparser)

size <- numberInteger()
colon <- charParser(":")
left <- function(a,b) concatenation(a,b, action=function(s) s[[1]])

as.logical_ <- function(a) {
  ret <- as.logical(a)
  if(is.na(ret)) stop()
  ret
}

tndict <- function(from,to) {
  tnl <- list()
  name <- character()
  idx <- 1
  while(from$pos < to$pos - 1) {
    cstream <- tnetstring(from)
    name[idx] <- cstream$node
    cstream <- tnetstring(cstream$stream)
    tnl[idx] <- cstream$node
    idx <- idx + 1
    from <- cstream$stream
  }
  names(tnl) <- name
  tnl
}
tnlist <- function(from,to) {
  tnl <- list()
  idx <- 1
  while(from$pos < to$pos - 1) {
    cstream <- tnetstring(from)
    tnl[idx] <- cstream$node
    idx <- idx + 1
    from <- cstream$stream
  }
  tnl
}

type <- charInSetParser(function(ch) ch %in% c(',', '#', '^', '!', '~', ']', '}'),action=I)
taken <- function (n, action = function(s) list(type = "n", value = s), error = function(p) list(type = "n", pos = p)) {
  function(stream) {
    n <- n
    s <- NULL
    repeat {
      if(n == 0) break
      n <- n -1
      cstream <- streamParserNextCharSeq(stream)
      if (cstream$status == "eof") 
        return(list(status = "fail", node = error(streamParserPosition(stream)), stream = stream))
      s <- c(s, cstream$char)
      stream <- cstream$stream
    }
    return(list(status = "ok", node = action(paste(s, 
      collapse = "")), stream = stream)
    )
  }
}
tnetstrings <- function(stream) {
  cstream <- left(size,colon)(stream)
  stream <- cstream$stream
  len <- cstream$node$value
  cstream <- taken(as.integer(len))(stream)
  val <- cstream$node$value
  cstream <- type(cstream$stream)
  val <- switch(
    cstream$node,
    ',' = val,
    '#' = as.integer(val),
    '^' = as.numeric(val),
    '!' = as.logical_(toupper(val)),
    '~' = NA,
    ']' = tnlist(stream,cstream$stream),
    '}' = tndict(stream,cstream$stream)
  )
  return(list(status = "ok", node=val, stream=cstream$stream))
}

tnetstrings.unpack <- function(string) {
  ret <- tnetstrings(streamParserFromString(string))
  if(ret$status == "ok") {
    translate(ret$node)
  } else {
    stop(ret)
  }
}
tnetstrings.unpack.file <- function(filename) {
  ret <- tnetstrings(streamParserFromFileName(filename))
  if(ret$status == "ok") {
    translate(ret$node)
  } else {
    stop(ret)
  }
}
