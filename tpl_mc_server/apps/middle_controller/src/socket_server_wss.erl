%	版本 0.0.1
%	版本更新说明：
%	【2013-08-08】0.0.1版本建立
%	 在socket_server 1.1.0 的基础上修改获得
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

-module(socket_server_wss).
-export([start/0, start/1, start/2, start/3, start/6]).
-export([encode/1, encode_with_mask/1]).
-export([ver/0]).

ver() ->
    "0.01"
.

start() ->
	start(8296)
.
start(Port) ->
	start(Port, fun undef_f/4)
.
start(Port, DataPid) ->
	start(Port, DataPid, spawn(fun() -> undef_output() end))
.
start(Port, DataPid, MessagePid) ->
	start(Port, DataPid, MessagePid, 0, nil, nil)
.
start(Port, DataPid, MessagePid, PNum, Certfile, Keyfile) ->
	ssl:start(),
	case ssl:listen(Port, [
		{certfile, Certfile}, 
		{keyfile, Keyfile}, 
		binary, {packet, 0}, {packet_size, 0}, {reuseaddr, true},{active, once},
		{delay_send, true},{send_timeout, 5000},{sndbuf, 16 * 1024},{recbuf, 16 * 1024},
		{high_watermark, 128 * 1024}, {low_watermark, 64 * 1024}]) of
	% case gen_tcp:listen(Port, [binary, {packet, 0}, {packet_size, 80000000}, {reuseaddr, true},{active, once},{nodelay, true},{send_timeout, 5000}]) of
		{error, Reason} ->
			MessagePid ! {error, "Listen error", Reason};

		{ok, Listen} -> 
			MessagePid ! {ok, "Listen ok", Port, Listen},
			case PNum of
				0 ->
					spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end);
				_ ->
					Spawn = fun(_) -> spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end) end,
					lists:foreach(Spawn, lists:seq(1, PNum))
			end
	end
.


%delete_all_socket([H|T]) ->
%	{Socket} = H,
%	gen_tcp:close(Socket),
%	delete_all_socket(T)
%;
%delete_all_socket([]) ->
%	ok
%.
%
%delete_socket(Socket, Port)->
%	ets:delete(ETSid, Socket),
%	gen_tcp:close(Socket)
%.

par_connect(Listen, DataPid, MessagePid, Port) ->
	SuccessFn = fun(Socket) ->
		MessagePid ! {msg, "Socket connected", Socket, self()},
		loop(Socket, DataPid, MessagePid, Port, <<>>, false)
	end,
	FailFn = fun(Reason) ->
		MessagePid ! {error, "Socket connect error", Reason}
	end,
	{Any, TLSTransportSocket} = ssl:transport_accept(Listen),
	spawn(fun() -> par_connect(Listen, DataPid, MessagePid, Port) end),
	ssl_handshake(SuccessFn, FailFn, {Any, TLSTransportSocket})
.

ssl_handshake(SuccessFn, FailFn, {ok, TLSTransportSocket}) ->
	case ssl:handshake(TLSTransportSocket, 5000) of
		{ok, SSLSocket} -> 
			SuccessFn(SSLSocket);
		{ok, _SSLSocket, _Ext} ->
			FailFn("not support hello ssl handshake!");
		{error, Reason} ->
			FailFn(Reason)
	end
;
ssl_handshake(_SuccessFn, FailFn, {error, Reason}) ->
	FailFn(Reason)
.


loop(Socket, DataPid, MessagePid, Port, Lave, IsHandshaked) ->
	receive
		{ssl, Socket, Bin } ->
			%io:format("Bin:~p~n", [Bin]),
			case IsHandshaked of
				true ->					
					case get_all(list_to_binary([Lave, Bin])) of
						{ok, DataLeave} ->
							inet:setopts(Socket,[{active,once}]),
							loop(Socket, DataPid, MessagePid, Port, DataLeave, true);
						close ->
						 	MessagePid ! {msg, "socket closed", Socket};
						{err, Resion} ->
							io:format("Decode error:~p~n",[Resion]),
						 	MessagePid ! {msg, "socket closed", Socket}
				 	end;
				_ ->
					case erlang:decode_packet(http, Bin, []) of
						{ok, {http_request, Method, Uri, Version}, Rest} -> %websocket 握手信息
							case DataPid(hand_shake, Socket, self(), {Method, Uri, Version}) of
								true ->
									Headers = headers(Rest, []),
									% _Request = {request, [{method, Method}, {uri, Uri}, {version, Version}], headers, Headers},
									BinaryClientKey = list_to_binary(get_header_value("sec-websocket-key", Headers)),
									HandShake = [
										"HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
										"Upgrade: WebSocket\r\n",
										"Connection: Upgrade\r\n",
										"Sec-WebSocket-Accept: ",
										base64:encode_to_string(crypto:hash(sha, <<BinaryClientKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
										"\r\n\r\n"
									],
									ssl:send(Socket, HandShake),
									ssl:setopts(Socket,[{active,once}]),
									loop(Socket, DataPid, MessagePid, Port, Lave, true);
								false ->
									MessagePid ! {msg, "socket closed", Socket}
							end;
						_ -> %直接抛出错误
				   		 	io:format("Handshak error:~p~n",[{tcp, Socket, Bin}]),
						 	MessagePid ! {msg, "socket closed", Socket}
					end
			end;
		{self, Value} ->
			send_to_func([Value], Socket, DataPid, Port),
			loop(Socket, DataPid, MessagePid, Port, Lave, IsHandshaked);
		{ssl_closed, Socket} ->
		 	MessagePid ! {msg, "socket closed", Socket};
		{ssl_error, Socket, Reason} ->
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

headers(Packet, Acc) ->
  F = fun(S) when is_atom(S)-> atom_to_list(S);
        (S)-> S
      end,
  case erlang:decode_packet(httph, Packet, [])of
    {ok, {http_header, _, Key, _, Value}, Rest} ->
      headers(Rest, [{string:to_lower(F(Key)), Value} | Acc]);
    {ok, http_eoh, _} ->
      Acc
  end.

get_header_value(Key, Headers) ->
  proplists:get_value(Key, Headers).

% get_request_resource_uri(Request) ->
%   {abs_path, Path} = get_request_value(uri, Request),
%   Path.

% get_request_version(Request) ->
%   {X, Y} = get_request_value(version, Request),
%   integer_to_list(X) ++ "." ++ integer_to_list(Y).

% get_request_value(Key, Request) ->
%   proplists:get_value(Key, Request).


unmask(<<Data:32, Rest/bits>>, MaskKey, Acc) ->
  T = Data bxor MaskKey,
  unmask(Rest, MaskKey, <<Acc/binary, T:32>>);

unmask(<<Data:24>>, MaskKey, Acc) ->
  <<MaskKey2:24, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:24>>;

unmask(<<Data:16>>, MaskKey, Acc) ->
  <<MaskKey2:16, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:16>>;

unmask(<<Data:8>>, MaskKey, Acc) ->
  <<MaskKey2:8, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:8>>;

unmask(<<>>, _, Acc) ->
  Acc.


%只支持文本接收 Opcode = 1
%FIN:1, RSV1:1, RSV2:1, RSV3:1, Opcode:4, (Len:7|126:7, Len:16|127:7, Len:64) , (Mask:0 | Mask:4), Payload
get_data(Data) -> 
	case Data of
		<< FIN:1, _RSV1:1, _RSV2:1, _RSV3:1, Opcode:4, Mask:1, Bin/bits >> when Opcode == 1 orelse Opcode == 9 ->
			case FIN of
				1 ->
					case Mask of
						1 ->
							get_data2(true, Bin, Data);
						_ ->
							{err, "Not Support Mask /= 1"}
					end;
				0 ->
					{err, "Not Support FIN == 0"}
			end;
		<< _:4, 8:4, _/bits>> -> %关闭连接
			close;

		<< FIN:1, _RSV1:1, _RSV2:1, _RSV3:1, Opcode:4, Mask:1, _Bin/bits >> ->
			io:format("FIN:~p~n", [FIN]),
			io:format("RSV1:~p~n", [_RSV1]),
			io:format("RSV2:~p~n", [_RSV2]),
			io:format("RSV3:~p~n", [_RSV3]),
			io:format("Opcode:~p~n", [Opcode]),
			io:format("Mask:~p~n", [Mask]),
			%io:format("Bin:~p~n", [Bin]),
			{err, "Not Support Opcode /= 1"}
	end
.

encode(Payload) ->
    Opcode = 1,
    Len = iolist_size(Payload),
    BinLen = payload_length_to_binary(Len),
    %MaskingKeyBin = crypto:strong_rand_bytes(4),
    % << MaskingKey:32 >> = MaskingKeyBin,
    Header = << 1:1, 0:3, Opcode:4, 0:1, BinLen/bits >>,
    %MaskedPayload = mask_payload(MaskingKey, Payload),
    << Header/binary, Payload/binary >>
.

encode_with_mask(Payload) ->
    Opcode = 1,
    Len = iolist_size(Payload),
    BinLen = payload_length_to_binary(Len),
    MaskingKeyBin = crypto:strong_rand_bytes(4),
    << MaskingKey:32 >> = MaskingKeyBin,
    Header = << 1:1, 0:3, Opcode:4, 1:1, BinLen/bits, MaskingKeyBin/bits >>,
    MaskedPayload = mask_payload(MaskingKey, Payload),
    << Header/binary, MaskedPayload/binary >>
.
%% @doc The payload is masked using a masking key byte by byte.
%% Can do it in 4 byte chunks to save time until there is left than 4 bytes left
mask_payload(MaskingKey, Payload) ->
    mask_payload(MaskingKey, Payload, <<>>).
mask_payload(_, <<>>, Acc) ->
    Acc;
mask_payload(MaskingKey, << D:32, Rest/bits >>, Acc) ->
    T = D bxor MaskingKey,
    mask_payload(MaskingKey, Rest, << Acc/binary, T:32 >>);
mask_payload(MaskingKey, << D:24 >>, Acc) ->
    << MaskingKeyPart:24, _:8 >> = << MaskingKey:32 >>,
    T = D bxor MaskingKeyPart,
    << Acc/binary, T:24 >>;
mask_payload(MaskingKey, << D:16 >>, Acc) ->
    << MaskingKeyPart:16, _:16 >> = << MaskingKey:32 >>,
    T = D bxor MaskingKeyPart,
    << Acc/binary, T:16 >>;
mask_payload(MaskingKey, << D:8 >>, Acc) ->
    << MaskingKeyPart:8, _:24 >> = << MaskingKey:32 >>,
    T = D bxor MaskingKeyPart,
    << Acc/binary, T:8 >>.

%% @doc Encode the payload length as binary in a variable number of bits.
%% See RFC Doc for more details
payload_length_to_binary(Len) when Len =<125 ->
    << Len:7 >>;
payload_length_to_binary(Len) when Len =< 16#ffff ->
    << 126:7, Len:16 >>;
payload_length_to_binary(Len) when Len =< 16#7fffffffffffffff ->
    << 127:7, Len:64 >>.


get_all(Data) ->
	case get_data(Data) of
		L when is_list(L) ->
			{ok, send_data_all(L)};
		Other ->
			Other
	end
.



send_data_all([H|T]) ->
	case H of
		{wait_binary, Data} ->
			Data;
		Data ->
			%io:format("InData:~p~n", [Data]),
			% io:format("SizeOfInData:~p~n", [size(Data)]),
			self() ! {self, Data},
			send_data_all(T)
	end
	
;
send_data_all([]) -> 
	<<>>
.

get_data2(true,<<Len:7, MaskKey:32, MuskRest/bits>>, OrData) when Len < 126 ->
	get_data3(MuskRest, MaskKey, Len, OrData)
;
get_data2(true,<<126:7, Len:16, MaskKey:32, MuskRest/bits>>, OrData) when Len > 125 ->
	get_data3(MuskRest, MaskKey, Len, OrData)
;
get_data2(true,<<127:7, 0:1, Len:63, MaskKey:32, MuskRest/bits>>, OrData) when Len > 16#ffff ->
	get_data3(MuskRest, MaskKey, Len, OrData)
.

get_data3(MuskRest, MaskKey, Len, OrData) ->
	MuskRestSize = size(MuskRest),
	if 
		MuskRestSize < Len ->
			[{wait_binary, OrData}] ;
		true ->
			{UnMuskData, DataLeave} = erlang:split_binary(MuskRest, Len),
			Data = unmask(UnMuskData, MaskKey, <<>>),
			get_data4(Data, DataLeave)
	end
.

get_data4(R, <<>>) ->
	[R]
;
get_data4(R, DataLeave) ->
	[ R | get_data(DataLeave)]
.


%获得掩码数据

%获得非掩码数据

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

undef_f(http_request, _Method, Uri, _Version) ->
	io:format("Uri:~p~n",[Uri]),
	true
;

undef_f(data_in, Socket, Port, Bin) ->
	io:format("Coming: Socket:~p~n Port:~p~n Bin:~p~n",[Socket, Port, Bin] ),
	io:format("Outing Message: ~p~n", [encode(<<"I got your message">>)] ),
	ssl:send(Socket, encode(<<"I got your message">>)),
	ssl:setopts(Socket,[{active,once}])
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

