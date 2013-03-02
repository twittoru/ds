data-interchange format in R
=========

WHAT IS THIS
------------

いくつかのデータシリアライゼーションフォーマットを R で実装したもの.

2 種類に大きく分けることができて，

 * JSON/LTSV/TNetstrings をパーサコンビネータ qmrparser でパースする関数 (./qmrparser)
 * JSON/MessagePack への/からの pack/unpack メソッドを供えたクラスライブラリ (./json,./msgpack)

前者は qmrparser の習作ですので qmrparser をインストールする必要があります．

後者は R5 のクラスオブジェクトの習作として書いてます. pure-R なのでパッケージの別途インストールは必要ありません．

HOW TO USE
----------

	> source("qmrparser/ltsv.R")
	> ltsv.unpack("host:127.0.0.1\tident:-\tuser:frank\ttime:[10/Oct/2000:13:55:36 -0700]\treq:GET /apache_pb.gif HTTP/1.0\tstatus:200\tsize:2326\treferer:http://www.example.com/start.html\tua:Mozilla/4.08 [en] (Win98; I ;Nav)")

	$host
	[1] "127.0.0.1"
	
	$ident
	[1] "-"
	
	$user
	[1] "frank"
	
	$time
	[1] "[10/Oct/2000:13:55:36 -0700]"
	
	$req
	[1] "GET /apache_pb.gif HTTP/1.0"
	
	$status
	[1] "200"
	
	$size
	[1] "2326"
	
	$referer
	[1] "http://www.example.com/start.html"
	
	$ua
	[1] "Mozilla/4.08 [en] (Win98; I ;Nav)"


WARNINGS
--------

JSON and MessagePack no-qmrparser version packages support passable function.

The others qmrparser version don't tested; these are writing hot codes.

Some numeric type is (un)packed inexactly, because the number in JSON and 32/64bit (unsigned) integer in MessagePack are too large as numeric in R.

MessagePack unpacker is derivative of a_bicky's unpacker.

gist: https://gist.github.com/4433343

blog: http://d.hatena.ne.jp/a_bicky/20130102/1357123909


SEE ALSO
--------

qmrparser: http://cran.r-project.org/web/packages/qmrparser/

messagepack: http://msgpack.org/

json: http://www.json.org/

tnetstrings: http://tnetstrings.org/

ltsv: http://ltsv.org/
