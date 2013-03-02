library(qmrparser)

second <- function(...) concatenation(..., action=function(s) s[[2]])
first  <- function(...) concatenation(..., action=function(s) s[[1]])
alt    <- function(...) alternation(..., action=I)
star   <- function(...) option(plus(...), action=I)
plus <- function(...) repetition1N(..., action=emptyToNull)
opt    <- function(...) option(..., action=I)
listAppend <- function(s) {
  if(length(s) == 2) {
    append(s[1], s[[2]])
  } else {
    list(s)
  }
}
emptyToList <- function(s) {
  if(!is.null(s$type) && s$type == "empty") {
    list(s)
  } else {
    s
  }
}
cutEmpty <- function(s) {
  if(!is.null(s[[2]]$type) && s[[2]]$type == "empty") {
    s[1]
  } else {
    listAppend(s)
  }
}
emptyToNull <- function(s) {
  if(!is.null(s$type) && s$type == "empty") {
    NULL
  } else {
    s
  }
}
parsePaste <- function(typename="", postfunction=I) {
  function(s) {
    list(
      type=typename,
      value=postfunction(paste(sapply(s, function(ss) ss$value), collapse=""))
    )
  }
}
parsePaste0 <- function(...) {
  function(s) {
    if(s$type == "empty") {
      s
    } else {
      parsePaste(...)(s$value)
    }
  }
}

nl <- concatenation(opt(charParser("\r")),charParser("\n"))
tab <- charParser("\t")
colon <- charParser(":")
lbyte <- charInSetParser(lbyte_range)
fbyte <- charInSetParser(fbyte_range)

lbyte_range <- function(ch) {
  ch %in% c(LETTERS,letters,0:9)
}
fbyte_range <- function(ch) {
  charToRaw(ch) %in% as.raw(c(1:8,0x0B,0x0C,0x0E:0xFF))
}

field_value <- repetition0N(fbyte,action=parsePaste0("fval"))
label <- repetition1N(lbyte,action=parsePaste("label"))
field <- concatenation(label, colon, field_value, action=function(s) list(name=s[[1]]$value, value=s[[3]]$value))
record <- option(concatenation(field, star(second(tab, field)), action=listAppend),action=function(s) list(type="record",value=s))
ltsv <- concatenation(record,star(second(nl,record)),action=cutEmpty)


translate <- function(node) {
  lapply(node,function(n) {
    switch(n$type,
      `record` = trans_recode(n$value)
    )
  })
}
trans_recode <- function(node) {
  if(!is.null(node$type) && node$type == "empty") {
    list()
  } else {
    structure(
      lapply(node,function(n) n$value),
      names=lapply(node,function(n) n$name)
    )
  }
}

ltsv.unpack <- function(string) {
  ret <- ltsv(streamParserFromString(string))
  if(ret$status == "ok") {
    translate(ret$node)
  } else {
    stop(ret)
  }
}
ltsv.unpack.file <- function(filename) {
  ret <- ltsv(streamParserFromFileName(filename))
  if(ret$status == "ok") {
    translate(ret$node)
  } else {
    stop(ret)
  }
}
ltsv.unpack("host:127.0.0.1\tident:-\tuser:frank\ttime:[10/Oct/2000:13:55:36 -0700]\treq:GET /apache_pb.gif HTTP/1.0\tstatus:200\tsize:2326\treferer:http://www.example.com/start.html\tua:Mozilla/4.08 [en] (Win98; I ;Nav)")[[1]]
