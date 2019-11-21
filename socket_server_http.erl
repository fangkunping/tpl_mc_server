%	版本 g2-1.0.7
%	版本更新说明：
%	【2019-11-21】g2-1.0.7版本修改
%	取消resume函数，及自身模块的超时。 
%	如果 MessagePid 传入 nil 则不发送提示信息
%	增加 header 数据获取
%	【2013-12-13】g2-1.0.6版本修改
%	增加resume函数，仅仅给自身模块使用，用于网页访问超时, 30秒超时。 
%	【2011-04-20】g2-1.0.5版本修改
%	增加预先开启进程数模式。 
%	【2011-03-31】g2-1.0.4版本修改
%	改用http做头标识, 去掉start/4方法, 
%	返回 {get|post, Path, Value}, 
%	【2010-10-14】g-1.0.3版本修改
%	使用头长度进行tcp/ip数据获取判断依据，即{packet,HeadLength}。 HeadLength = 1|2|4 byte
%	【2010-03-27】1.0.2版本修改
%	cut_zero/1 使用 string:tokens 函数。
%	【2010-03-27】1.0.2版本修改
%	cut_zero/1 使用 string:tokens 函数。
%	增加stop方法
%	【2008-03-27】1.0.1版本修改
%	DataPid 替换为程序 ，不使用线程
%	增加stop方法
%	【2008-03-27】1.0.0版本建立
%
%    实现：建立一个tcp服务。
%	
%	start() 启动一个默认端口为8296的tcp/ip服务
%	
%	start(Port) 启动一个指定端口的tcp/ip服务
%	
%	start(Port, DataPid) 启动一个指定端口的tcp/ip服务
%	Port -> 整数，端口号 
%	DataPid -> 接收数据的程序(v1.0.1) （接收数据的线程id v1.0.0）
%	线程接收数据的格式是 {Socket, H} -> {Socket, 收到的讯息[]已经去除结束号0}
%	
%	start(Port, DataPid, MessagePid) 启动一个指定端口的tcp/ip服务
%	Port -> 整数，端口号 
%	DataPid -> 接收数据的程序(v1.0.1) （接收数据的线程id v1.0.0）
%	MessagePid -> 接收服务器信息的线程id
%	线程接收数据的格式是 {Socket, H} -> {Socket, 收到的讯息[]已经去除结束号0}
%	
%	start(Port, DataPid, MessagePid, Length) 启动一个指定端口的tcp/ip服务, 并且定义包头的大小 1|2|4 个bytes
%	Port -> 整数，端口号 
%	DataPid -> 接收数据的程序(v1.0.1) （接收数据的线程id v1.0.0）
%	MessagePid -> 接收服务器信息的线程id
%	线程接收数据的格式是 {Socket, H} -> {Socket, 收到的讯息[]已经去除结束号0}
%	
%	stop() 结束当前服务，默认端口 8296。 (v1.0.1)
%	stop(Port) 结束当前服务， 
%	
%	delete_socket(Socket, Port) 断开某一socket 

-module(socket_server_http).
-export([start/0, start/1, start/2, start/3, start/4]).
-export([resume/1]).
-export([ver/0]).

% 连接自动断开时间, 毫秒
%%[2019-11-21]%% -define(disconnect_time, 5000).

% 接收数据超时时间, 毫秒
-define(TCP_RECV_TIMEOUT, 5000).

ver() ->
    'g2-1.0.6'
.

start() ->
	start(8296)
.
start(Port) ->
	start(Port, fun undef_f/4)
.
start(Port, DataPid) ->
	start(Port, DataPid, 0)
.
start(Port, DataPid, PNum) ->
	start(Port, DataPid, spawn(fun() -> undef_output() end), PNum)
.

start(Port, DataPid, MessagePid, PNum) ->
	case gen_tcp:listen(Port, [{active, false},
            list,
            {backlog, 256},
            {packet, http},
            {raw,6,9,<<1:32/native>>}, %% defer accept
            %%{delay_send,true},
            %%{nodelay,true},
            {reuseaddr, true}]) of
		{error, Reason} ->
			send_info_message(MessagePid, {error, "Listen error", Reason});
		{ok, Listen} -> 
			case PNum of
				0 ->
					spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end);
				_ ->
					Spawn = fun(_) -> spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end) end,
					lists:foreach(Spawn, lists:seq(1, PNum))
			end
	end
.

send_info_message(nil, _) ->
	ok
;
send_info_message(MessagePid, V) ->
	MessagePid ! V
.

par_connect(Listen, DataPid, MessagePid, Port) ->
	{Any, Socket} = gen_tcp:accept(Listen),
	case {Any, Socket} of
		{ok, Socket} ->
			send_info_message(MessagePid, {msg, "Socket connected", Socket, self()}),
			spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end),
			loop(Socket, DataPid, MessagePid, Port) ;
		{error, Reason} ->
			spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end),
			send_info_message(MessagePid, {error, "Socket connect error", Reason})
	end
.

loop(Socket, DataPid, MessagePid, Port) ->
	case gen_tcp:recv(Socket, 0, ?TCP_RECV_TIMEOUT) of
		{ok, Packet} ->
			%% io:format("HttpPacket:~p ~n",[Packet] ),
			handle_request(Socket, Port, Packet, DataPid);
		{error, closed} ->
			send_info_message(MessagePid, {msg, "socket closed", Socket});
		X -> io:format("unknown_message:~p~n",[X])
	end
.

%% 解码 http_request
%% HttpPacket = HttpRequest | HttpResponse | HttpHeader | http_eoh | HttpError
%% HttpRequest = {http_request, HttpMethod, HttpUri, HttpVersion}
%% HttpResponse = {http_response, HttpVersion, integer(), HttpString}
%% HttpHeader = {http_header, int(), HttpField, Reserved=term(), Value=HttpString}
%% HttpError = {http_error, HttpString}
%% HttpMethod = HttpMethodAtom | HttpString
%% HttpMethodAtom = 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' | 'DELETE' | 'TRACE'
%% HttpUri = '*' | {absoluteURI, http|https, Host=HttpString, Port=int()|undefined, Path=HttpString} | {scheme, Scheme=HttpString, HttpString} | {abs_path, HttpString} | HttpString
%% HttpVersion = {Major=int(), Minor=int()}
%% HttpString = string() | binary()
%% HttpField = HttpFieldAtom | HttpString
%% HttpFieldAtom = 'Cache-Control' | 'Connection' | 'Date' | 'Pragma' | 'Transfer-Encoding' | 'Upgrade' | 'Via' | 'Accept' | 'Accept-Charset' | 'Accept-Encoding' | 'Accept-Language' | 'Authorization' | 'From' | 'Host' | 'If-Modified-Since' | 'If-Match' | 'If-None-Match' | 'If-Range' | 'If-Unmodified-Since' | 'Max-Forwards' | 'Proxy-Authorization' | 'Range' | 'Referer' | 'User-Agent' | 'Age' | 'Location' | 'Proxy-Authenticate' | 'Public' | 'Retry-After' | 'Server' | 'Vary' | 'Warning' | 'Www-Authenticate' | 'Allow' | 'Content-Base' | 'Content-Encoding' | 'Content-Language' | 'Content-Length' | 'Content-Location' | 'Content-Md5' | 'Content-Range' | 'Content-Type' | 'Etag' | 'Expires' | 'Last-Modified' | 'Accept-Ranges' | 'Set-Cookie' | 'Set-Cookie2' | 'X-Forwarded-For' | 'Cookie' | 'Keep-Alive' | 'Proxy-Connection'
%% 判断类型
handle_request (Socket, Port, HttpPacket, DataPid)->
	case HttpPacket of
		{http_request,HttpMethod, HttpUri, _} ->
			case HttpMethod of
				'GET' ->
					%%io:format("in GET: ~p ~n",[HttpUri] ),
					case HttpUri of
						{_, Value} ->
							case sp_string:unjoin(Value, "?") of
								[Path, Value2]->
									handle_get(Socket, Port, DataPid, {get, Path, Value2, []});
								_ ->
									handle_get(Socket, Port, DataPid, {get, Value, [], []})
							end;
							
						_ ->

							io:format("unknown GET HttpUri: ~p ~n",[HttpMethod] ),
							send_unsupported_error(Socket)
					end;
				'POST' ->
					%% io:format("in POST: ~p ~n",[HttpUri] ),
					case HttpUri of
						{_, Value} ->
							handle_post(Socket, Port, DataPid, Value);
						_ ->
							io:format("unknown POST HttpUri: ~p ~n",[HttpMethod] ),
							send_unsupported_error(Socket)
					end;
				_ ->
					io:format("unknown HttpMethod: ~p ~n",[HttpMethod] ),
					send_unsupported_error(Socket)					
			end;
		_ ->
			io:format("unknown HttpPacket:~p~n",[HttpPacket]),
			send_unsupported_error(Socket)
	end
.

get_content_length(Sock, HeaderList) ->
	case gen_tcp:recv(Sock, 0, ?TCP_RECV_TIMEOUT) of
		{ok, {http_header, _, 'Content-Length', _, Length}} -> {list_to_integer(Length), HeaderList};
		{ok, {http_header, _, Header, _, HeaderValue}} -> get_content_length(Sock, [ {Header, HeaderValue} | HeaderList]);
		{error, Reason} -> io:format("http_server get_content_length error: ~p ~n",[Reason] );
		_  ->
			%% io:format("http_server get_content_length x: ~p ~n",[X] ),
			get_content_length(Sock, HeaderList)
	end
.

get_body(Sock, Length) ->
	case gen_tcp:recv(Sock, 0, ?TCP_RECV_TIMEOUT) of
		{ok, http_eoh} -> 
			inet:setopts(Sock, [{packet, raw}]),
			case gen_tcp:recv(Sock, Length, ?TCP_RECV_TIMEOUT) of
				{ok,Body} ->
					Body;
				{error, Reason} -> io:format("http_server get_body error_sub: ~p ~n",[Reason] )
			end;
			%% inet:setopts(Sock, [{packet, raw}]),{ok,Body}=gen_tcp:recv(Sock, Length, 60000),	Body;
		{error, Reason} -> io:format("http_server get_body error: ~p ~n",[Reason] );
		_ -> get_body(Sock, Length)
	end
.
get_end(Sock)->
	case gen_tcp:recv(Sock, 0, ?TCP_RECV_TIMEOUT) of
		{ok, http_eoh} -> done;
		{error, Reason} -> io:format("http_server get_end error: ~p ~n",[Reason] );
		_ -> get_end(Sock)
	end
.

handle_get(Sock, _Port, DataPid, Value) ->
	get_end(Sock),
	DataPid(data_in, Sock, self(), Value)
	%5秒后自动断开链接
	%%[2019-11-21]%% erlang:send_after(?disconnect_time, self(), "honk honk"),
	%%[2019-11-21]%% proc_lib:hibernate(?MODULE, resume, [Sock])
.
resume(Socket) ->
	receive
		_Msg ->
			case inet:getstat(Socket) of
				{ok, _} ->
					gen_tcp:close(Socket);
				_ ->
					nothing
			end
			%io:format("wake up message: ~p~n", [Msg])
	end
.
handle_post(Sock, _Port, DataPid, Path) ->
	{Length, HeaderList}=get_content_length(Sock, []),
	PostBody=case Length of
		0 -> "";
		_ -> get_body(Sock, Length)
	end,
	%%io:format("PostBody :~p~n",[PostBody]),
	%%io:format("Path :~p~n",[Path]),
	DataPid(data_in, Sock, self(), {post, Path, PostBody, HeaderList})
	%5秒后自动断开链接
	%%[2019-11-21]%% erlang:send_after(?disconnect_time, self(), "honk honk"),
	%%[2019-11-21]%% proc_lib:hibernate(?MODULE, resume, [Sock])
	%% spawn(fun() -> DataPid(Sock, Port, {post, Path, PostBody}) end)
	%% send_accept(Sock)
.


send_accept(Sock) ->
	%% gen_tcp:send(Sock, "HTTP/1.1 202 Accepted\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
	gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nhello world!"),
	gen_tcp:close(Sock)
.
 
send_unsupported_error(Sock) ->
	gen_tcp:send(Sock, "HTTP/1.1 405 Method Not Allowed\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
	gen_tcp:close(Sock)
.	

undef_f(data_in, Socket, SocketPid, Bin) ->
	io:format("Coming: Socket:~p~n Port:~p~n Bin:~p~n",[Socket, SocketPid, Bin] ),
	send_accept(Socket)
.

undef_output() ->
	receive
		{error, "Listen error", Reason} ->
			io:format("Listen error: ~p~n",[Reason]),
			undef_output();
		{msg, "Socket connected", Socket, Pid} ->
			io:format("Socket connected: ~p ~p~n",[Socket, Pid]),
			undef_output();
		{msg, "socket closed", Socket} ->
			io:format("socket closed: ~p~n",[Socket]),
			undef_output();
		X -> io:format("unknown_message:~p~n",[X]),
			undef_output()
	end
.

