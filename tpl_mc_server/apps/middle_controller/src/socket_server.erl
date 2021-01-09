%	版本 1.2.0
%	版本更新说明：
%	【2013-10-28】1.2.0版本修改
%	切分0方式修改
%	【2013-08-08】1.1.1版本修改
%	加入握手信息判断
%	优化socket出错，关闭部分的代码
%	【2013-07-25】1.1.0版本修改
%	加入可以接收内部事件的 {self, Value} ->
%	【2013-07-23】1.0.9版本修改
%	加入gen_tcp标识：延迟发送，发送高低水位，发送缓冲大小，接收缓冲大小
%	【2013-06-05】1.0.8版本修改
%	发给 socket_process_bin 的时候 不在 binary_to_list, 因为那边又要解一次，太麻烦了
%	【2013-01-30】1.0.7版本修改
%	修改高并发导致发包顺序颠倒问题。
%	【2012-07-09】1.0.6版本修改
%	修正一次传输太多数据（没有到0结尾），{active, once}需要再次激活的bug。
%	【2011-11-09】1.0.5版本修改
%	去掉flash_policy检测。
%	【2011-04-27】1.0.4版本修改
%	修复一个重要的BUG，即socket关闭没有检测到。
%	【2011-04-20】1.0.3版本修改
%	增加预先开启进程数模式。
%	【2011-04-08】1.0.2b版本修改
%	改成 {active, once}, 模式。
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
%	stop() 结束当前服务，默认端口 8296。 (v1.0.1)
%	stop(Port) 结束当前服务， 
%	
%	delete_socket(Socket, Port) 断开某一socket 

-module(socket_server).
-export([start/0, start/1, start/2, start/3, start/4]).
-export([ver/0]).

ver() ->
    "1.20"
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
	start(Port, DataPid, MessagePid, 0)
.
start(Port, DataPid, MessagePid, PNum) ->
	case gen_tcp:listen(Port, [binary, {packet, 0}, {packet_size, 80000000}, {reuseaddr, true},{active, once},{delay_send, true},{send_timeout, 5000},{sndbuf, 16 * 1024},{recbuf, 16 * 1024},{high_watermark, 128 * 1024}, {low_watermark, 64 * 1024}]) of
	% case gen_tcp:listen(Port, [binary, {packet, 0}, {packet_size, 80000000}, {reuseaddr, true},{active, once},{nodelay, true},{send_timeout, 5000}]) of
		{error, Reason} ->
			MessagePid ! {error, "Listen error", Reason};
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



par_connect(Listen, DataPid, MessagePid, Port) ->
	try
		{Any, Socket} = gen_tcp:accept(Listen),
		case {Any, Socket} of
			{ok, Socket} ->
				MessagePid ! {msg, "Socket connected", Socket},
				spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end),
				loop(Socket, DataPid, MessagePid, Port, [], false);
			{error, Reason} ->
				spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end),
				MessagePid ! {error, "Socket connect error", Reason}
		end
	catch
        Type:CrashReason ->
			io:format("bit gen_tcp catch error -> ~p:~p~n",[Type, CrashReason]),
			spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end)
	end
.



loop(Socket, DataPid, MessagePid, Port, Lave, IsHandshaked) ->
	receive
		{tcp, Socket, Bin} ->
			R = cut_zero(Lave, Bin),
			[Last|T] = lists:reverse(R),
			case T of
				[<<>>] -> 
					inet:setopts(Socket,[{active,once}]);
				[] -> 
					inet:setopts(Socket,[{active,once}]);
				_ -> 
					%spawn(fun() -> send_to_func(lists:reverse(T),Socket, DataPid, Port) end)
					send_to_func(lists:reverse(T),Socket, DataPid, Port)
			end,
			loop(Socket, DataPid, MessagePid, Port, Last, IsHandshaked);
		{self, Value} ->
			send_to_func([Value], Socket, DataPid, Port),
			loop(Socket, DataPid, MessagePid, Port, Lave, IsHandshaked);
		{tcp_closed, Socket} ->
		 	MessagePid ! {msg, "socket closed", Socket};
		{tcp_error, Socket, Reason} ->
		 	io:format("Sockt error:~p~n",[Reason]),
		 	MessagePid ! {msg, "socket closed", Socket};
		X -> 
		 	MessagePid ! {msg, "socket closed", Socket},
			io:format("[socket_server] unknown_message:~p~n",[X])
	after 600000 -> %% 十分钟没有数据自动关闭链接
		MessagePid ! {msg, "socket closed", Socket},
		io:format("[socket_server] socket no data time out!:~p~n",[Socket])
			
	end
.

cut_zero(DataLeave, Data) ->
    [H|T] = re:split(Data,  <<"\\0">>),
    [list_to_binary([DataLeave, H]) | T]
.

send_to_func([H|T],Socket, DataPid, Port) ->
	%[_, _, _|B] = H,
	%io:format("send_to_func ~p ~p ~p ~p ~n" ,[Socket, Port, H , T]),
	%Pid ! {self(), Socket, haxe:decode(B)},
	%DataPid ! {Socket, Port, H},
	%spawn(fun() -> DataPid(Socket, Port, list_to_binary(H)) end),
	DataPid(data_in, Socket, self(), H),
	send_to_func(T,Socket, DataPid, Port)
;
send_to_func([], _, _, _) ->
	true
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

