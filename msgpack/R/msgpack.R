Msgpack <- setRefClass("Msgpack",
  fields = c("packer", "unpacker"),
  methods = list(
    initialize = function(string_mode=F) {
      packer <<- new("packer")
      unpacker <<- new("unpacker",string_mode=string_mode)
    },
    pack = function(object) {
      packer$pack(object)
    },
    unpack = function(msg) {
      unpacker$unpack(msg)
    }
  )
)
