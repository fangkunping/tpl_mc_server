%	版本 g-1.0.6
%	版本更新说明：
%	【2013-12-11】g-1.0.8版本修改
%	ets创建的名称修改，可以多开socket_server_bit模块了了。
%	【2013-01-30】g-1.0.7版本修改
%	修改高并发导致发包顺序颠倒问题。
%	【2011-04-27】g-1.0.6版本修改
%	修复一个重要的BUG，即socket关闭没有检测到。
%	【2011-04-20】g-1.0.5版本修改
%	增加预先开启进程数模式。
%	【2011-04-08】g-1.0.4b版本修改
%	改成 {active, once}, 模式。
%	【2011-03-14】g-1.0.4版本修改
%	增加 delete_socket_by_string 通过socket字符删除socket
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

-module(socket_server_bit).
-export([start/0, start/1, start/2, start/3, start/4, start/5, stop/0, stop/1, delete_socket/2, delete_socket_by_string/2]).
-export([ver/0]).

ver() ->
    'g-1.0.8'
.

start() ->
	start(8296)
.
start(Port) ->
	start(Port, fun undef_f/3)
.
start(Port, DataPid) ->
	start(Port, DataPid, spawn(fun() -> undef_output() end))
.
start(Port, DataPid, MessagePid) ->
	start(Port, DataPid, MessagePid, 1)
.
start(Port, DataPid, MessagePid, HeadLength) ->
	start(Port, DataPid, MessagePid, HeadLength, 0)
.
start(Port, DataPid, MessagePid, HeadLength, PNum) ->
	case gen_tcp:listen(Port, [binary, {packet, HeadLength},{reuseaddr, true},{active, once},{nodelay, true},{send_timeout, 5000}]) of
		{error, Reason} ->
			MessagePid ! {error, "Listen error", Reason};
		{ok, Listen} -> 
			kvs:start(),
			ETSid = ets:new( list_to_atom(atom_to_list(?MODULE)++integer_to_list(Port)) , [set, public]),
			kvs:store({d0, ?MODULE, Port}, [ETSid, Listen]),
			case PNum of
				0 ->
					spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end);
				_ ->
					Spawn = fun(_) -> spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end) end,
					lists:foreach(Spawn, lists:seq(1, PNum))
			end
	end
.

stop() ->
	stop(8296)
.

stop(Port) ->
	{ok, [ETSid, Listen]} = kvs:delete({d0, ?MODULE, Port}),
	gen_tcp:close(Listen),
	SockList = ets:tab2list(ETSid),
	delete_all_socket(SockList)
.

delete_all_socket([H|T]) ->
	{Socket} = H,
	gen_tcp:close(Socket),
	delete_all_socket(T)
;
delete_all_socket([]) ->
	ok
.

delete_socket(Socket, Port)->
	{ok, [ETSid, _]} = kvs:lookup({d0, ?MODULE, Port}),
	ets:delete(ETSid, Socket),
	gen_tcp:close(Socket)
.

delete_socket_by_string(SocketString, Port)->
	{ok, [ETSid, _]} = kvs:lookup({d0, ?MODULE, Port}),
	delete_socket_by_list2(ets:tab2list(ETSid), SocketString, ETSid)
.
delete_socket_by_list2([H|T], SocketString, ETSid) ->
	{Socket} = H,
	case erlang:port_to_list(Socket) of
		SocketString ->
			ets:delete(ETSid, Socket),
			gen_tcp:close(Socket);
		_ ->
			delete_socket_by_list2(T, SocketString, ETSid)
	end
;
delete_socket_by_list2([], _, _) ->
	done
.

par_connect(Listen, DataPid, MessagePid, Port) ->
	{Any, Socket} = gen_tcp:accept(Listen),
	case {Any, Socket} of
		{ok, Socket} ->
			{ok, [ETSid, Listen]} = kvs:lookup({d0, ?MODULE, Port}),
			ets:insert(ETSid, {Socket}),
			MessagePid ! {msg, "Socket connected", Socket},
			spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end),
			loop(Socket, DataPid, MessagePid, Port);
		{error, Reason} ->
			spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end),
			MessagePid ! {error, "Socket connect error", Reason}
	end
.

loop(Socket, DataPid, MessagePid, Port) ->
	receive
		{tcp, Socket, Bin} ->
			%% public hide io:format("Server Bin:~p ~n",[Bin] ),
			%spawn(fun() -> DataPid(Socket, Port, Bin) end),
			DataPid(Socket, Port, Bin),
			loop(Socket, DataPid, MessagePid, Port);
		{tcp_closed, Socket} ->
			{ok, [ETSid, _]} = kvs:lookup({d0, ?MODULE, Port}),
			MessagePid ! {msg, "socket closed", Socket},
			ets:delete(ETSid, Socket);
		{tcp_error, Socket, Reason} ->
			{ok, [ETSid, _]} = kvs:lookup({d0, ?MODULE, Port}),
			MessagePid ! {msg, "socket closed", Socket},
			io:format("Sockt error:~p~n",[Reason]),
			ets:delete(ETSid, Socket);
		X -> 
			{ok, [ETSid, _]} = kvs:lookup({d0, ?MODULE, Port}),
			MessagePid ! {msg, "socket closed", Socket},
			ets:delete(ETSid, Socket),
			io:format("[socket_server_bit] unknown_message:~p~n",[X])
	after 600000 -> %% 十分钟没有数据自动关闭链接
		{ok, [ETSid, _]} = kvs:lookup({d0, ?MODULE, Port}),
		MessagePid ! {msg, "socket closed", Socket},
		ets:delete(ETSid, Socket),	
		io:format("[socket_server_bit] socket no data time out!:~p~n",[Socket])
	end
.


undef_f(Socket, Port, Bin) ->
	io:format("Coming: Socket:~p~n Port:~p~n Bin:~p~n",[Socket, Port, Bin] ),
	inet:setopts(Socket,[{active,once}])
.

undef_output() ->
	receive
		{error, "Listen error", Reason} ->
			io:format("Listen error: ~p~n",[Reason]),
			undef_output();
		{msg, "Socket connected", Socket} ->
			io:format("Socket connected: ~p~n",[Socket]),
			undef_output();
		{msg, "socket closed", Socket} ->
			io:format("socket closed: ~p~n",[Socket]),
			undef_output();
		X -> io:format("unknown_message:~p~n",[X]),
			undef_output()
	end
.

