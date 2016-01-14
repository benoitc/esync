-module(esync).
-export([main/1]).

-export([copy_worker/2]).
-export([process_path/3]).

-define(STATS, stats).
-define(FILES, files).
-define(CONFLICTS, conflicts).


-include_lib("kernel/include/file.hrl").

init_app() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(esync),
    error_logger:tty(true),
    ?FILES = ets:new(?FILES, [ordered_set, public, named_table]),
    ?STATS = ets:new(?STATS, [ordered_set, public, named_table]),
    ?CONFLICTS = ets:new(?CONFLICTS, [ordered_set, public, named_table]),

    init_pool(),
    init_stats(),
    ok.

init_pool() ->
    wpool:start_pool(esync_pool).

init_stats() ->
    ets:insert(?STATS, [{conflicts, 0},
                        {dirs, 0},
                        {ncopy, 0},
                        {size, 0}]).


maybe_copy(Source, Target) ->
    IsRegular = filelib:is_regular(Source),

    case IsRegular of
        true ->
            {ok, SourceHash} = esync_util:md5_file(Source),
            case filelib:is_file(Target) of
                true ->
                    {ok, TargetHash} = esync_util:md5_file(Target),
                    if
                        TargetHash == SourceHash -> false;
                        true ->
                            {ok, FileInfo} = file:read_file_info(Source, [{time, posix}]),
                            #file_info{mtime=MTime} = FileInfo,
                            NTarget = find_name(Target ++ ".conflict-" ++ integer_to_list(MTime), 0),
                            ets:update_counter(?STATS, conflicts, {2, 1}),
                            ets:insert(?CONFLICTS, {NTarget, Source}),
                            NTarget
                    end;
                false -> Target
            end;
        false ->
            case filelib:is_file(Target) of
                true ->
                    {ok, FileInfo} = file:read_file_info(Source, [{time, posix}]),
                    #file_info{mtime=MTime} = FileInfo,
                    find_name(Target ++ "."++ integer_to_list(MTime), 0);
                false ->
                    Target
            end
    end.

find_name(Target, Inc) ->
    NewTarget = Target ++ "." ++ integer_to_list(Inc),
    case filelib:is_file(NewTarget) of
        true -> find_name(Target, Inc + 1);
        false -> NewTarget
    end.

update_stats(Source) ->
    {ok, FileInfo} = file:read_file_info(Source, [{time, posix}]),
    #file_info{size=Sz} = FileInfo,
    ets:update_counter(?STATS, size, {2, Sz}),
    ok.


copy_worker(Source, Target) ->
    case filelib:is_file(Source) of
        true ->
            case maybe_copy(Source, Target) of
                false -> ok;
                Target2 ->
                    catch do_copy(Source, Target2)
            end;
        false ->
            ok
    end,
    ets:update_counter(?STATS, ncopy, {2, 1}).


do_copy(Source, Target) ->
    TargetDir = filename:dirname(Target),
    esync_util:make_dir(TargetDir),
    filelib:ensure_dir(Target),
    {ok, _} = file:copy(Source, Target),
    update_stats(Source).



build_list(Source, Target) ->
    Files = filelib:wildcard("*", Source),
    process_path(Files, Source, #{ source => Source, target => Target}),
    wait_for(dirs, -1).


process_path([], _Dir, _State) ->
    ets:update_counter(?STATS, dirs, {2, -1}),
    ok;
process_path(["." | Rest], Dir, State) ->
    process_path(Rest, Dir, State);
process_path([".." | Rest], Dir, State) ->
    process_path(Rest, Dir, State);
process_path([".DS_Store" | Rest], Dir, State) ->
    process_path(Rest, Dir, State);
process_path([File | Rest], Dir, State) ->
    #{ source := Source,
       target := Target} = State,
    SourceFile = filename:join(Dir, File),
    RelPath = esync_util:relpath(SourceFile, Source),
    case filelib:is_dir(SourceFile) of
             true ->
                Files = filelib:wildcard("*", SourceFile),
                Args = [Files, SourceFile, State],
                wpool:cast(esync_pool, {?MODULE, process_path, Args}),
                ets:update_counter(?STATS, dirs, {2, 1});
             false ->
                 TargetFile = filename:join(Target, RelPath),
                 ets:insert(?FILES, {SourceFile, TargetFile})
         end,
    process_path(Rest, Dir, State).

wait_for(Key, N) ->
    case ets:update_counter(?STATS, Key, {2, 0}) of
        N ->
            ok;
        _ ->
            timer:sleep(500),
            wait_for(Key, N)
    end.

process_files('$end_of_table', N) ->
    wait_for(ncopy, N),
    io:format("done~n", []);
process_files({[{Source, Target}], Cont}, N) ->
    Args =  [Source, Target],
    wpool:cast(esync_pool, {?MODULE, copy_worker, Args}),
    process_files(ets:select(Cont), N+1).


display_stats() ->
    Conflicts = ets:update_counter(?STATS, conflicts, {2, 0}),
    NCopy = ets:update_counter(?STATS, ncopy, {2, 0}),
    Size = ets:update_counter(?STATS, size, {2, 0}),
    io:format("~nsize: ~p copied: ~p conflicts: ~p~n", [Size, NCopy, Conflicts]).


store_conflicts('$end_of_table', _Fd) ->
    ok;
store_conflicts({[{Conflict, _Source}], Cont}, Fd) ->
    ok = file:write(Fd, list_to_binary(io_lib:format("~p~n", [Conflict]))),
    store_conflicts(ets:select(Cont), Fd).

save_log() ->
    Conflicts = ets:update_counter(?STATS, conflicts, {2, 0}),
    NCopy = ets:update_counter(?STATS, ncopy, {2, 0}),
    Size = ets:update_counter(?STATS, size, {2, 0}),
    LogLine = io_lib:format("~nsize: ~p copied: ~p conflicts: ~p~n",
                            [Size, NCopy, Conflicts]),

    LogFile = "esync-" ++ integer_to_list(esync_util:timestamp()) ++ ".log",
    {ok, Fd} = file:open(LogFile, [raw, append]),
    file:write(Fd, list_to_binary(LogLine)),
    file:write(Fd, <<"\nconflicts:\n">>),


    Res = ets:select(?CONFLICTS, [{{'$1','$2'},[],[{{'$1','$2'}}]}], 1),
    ok = store_conflicts(Res, Fd),
    file:sync(Fd),
    file:close(Fd).

main([Source, Target]) ->
    ok = init_app(),
    io:format("building file list ...", []),
    esync_util:make_dir(Target),
    build_list(Source, Target),
    Size = ets:info(?FILES, size),
    io:format("~n~ndone (~p files found)~n", [Size]),
    io:format("copy files ... ", []),
    Res = ets:select(?FILES, [{{'$1','$2'},[],[{{'$1','$2'}}]}], 1),
    process_files(Res, 0),
    save_log(),
    display_stats();
main(_) ->
    esync_util:abort("usage: esync SOURCE TARGET~n", []).
