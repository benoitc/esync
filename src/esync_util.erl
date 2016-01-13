-module(esync_util).

-export([make_dir/1]).
-export([abort/2]).
-export([md5_file/1]).
-export([partition/2]).
-export([relpath/2]).
-export([timestamp/0]).

-define(BLOCK_SIZE, 32768).

make_dir(Dir) ->
    ok = filelib:ensure_dir(Dir),

    ok = case filelib:is_dir(Dir) of
             true -> ok;
             false ->
                 case file:make_dir(Dir) of
                     ok -> ok;
                     {error, eexist} -> ok; %% race condition
                     Error ->
                         io:format("Error while creating ~s: ~p~n",
                                   [Dir, Error]),
                         Error
                 end
         end.

abort(String, Args) ->
    io:format(String, Args),
    halt(1).

md5_file(File) ->
    case file:open(File, [binary, raw, read]) of
        {ok, Fd} -> md5_loop(Fd, crypto:hash_init(md5));
        Error -> throw(Error)
    end.

md5_loop(Fd, Ctx) ->
    case file:read(Fd, ?BLOCK_SIZE) of
        {ok, Bin} ->
            md5_loop(Fd, crypto:hash_update(Ctx, Bin));
        eof ->
            file:close(Fd),
            {ok, crypto:hash_final(Ctx)}
    end.

partition(String, Sep) ->
    case partition(String, Sep, []) of
        undefined ->
            {String, "", ""};
        Result ->
            Result
    end.

partition("", _Sep, _Acc) ->
    undefined;
partition(S, Sep, Acc) ->
    case partition2(S, Sep) of
        undefined ->
            [C | Rest] = S,
            partition(Rest, Sep, [C | Acc]);
        Rest ->
            {lists:reverse(Acc), Sep, Rest}
    end.

partition2(Rest, "") ->
    Rest;
partition2([C | R1], [C | R2]) ->
    partition2(R1, R2);
partition2(_S, _Sep) ->
    undefined.

relpath(Path, Root) ->
    {_, _, RelPath} = partition(Path, Root),
    case string:left(RelPath, 1) of
        " " ->
            "";
        "/" ->
            "/" ++ RelPath1 = RelPath,
            RelPath1;
        "\\" ->
            "\\\\" ++ RelPath1 = RelPath,
            RelPath1
    end.


timestamp() ->
    {A, B, _} = os:timestamp(),
    (A * 1000000) + B.
