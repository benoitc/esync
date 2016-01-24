-module(esync).
-export([main/1]).

-export([copy_worker/2]).
-export([process_path/3]).

-define(STATS, stats).
-define(FILES, files).
-define(CONFLICTS, conflicts).


-include_lib("kernel/include/file.hrl").

-define(MIN_WORKERS, 10).

init_app() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(esync),
    error_logger:tty(true),
    ?FILES = ets:new(?FILES, [ordered_set, public, named_table, compressed]),
    ?STATS = ets:new(?STATS, [ordered_set, public, named_table]),
    ?CONFLICTS = ets:new(?CONFLICTS, [ordered_set, public, named_table]),

    init_pool(),
    init_stats(),
    ok.

init_pool() ->
    NumWorkers = case erlang:system_info(threads) of
                     true ->
                         erlang:system_info(thread_pool_size) * 2 + 1;
                     false ->
                         ?MIN_WORKERS
                 end,

    wpool:start_pool(esync_pool, [{workers, NumWorkers}]).

init_stats() ->
    ets:insert(?STATS, [{conflicts, 0},
                        {dirs, 0},
                        {ncopy, 0},
                        {progress, 0},
                        {size, 0}]).


maybe_copy(Source, Target) ->
    IsRegular = filelib:is_regular(Source),

    case IsRegular of
        true ->
            {ok, SourceHash} = esync_util:md5_file(Source),
            case {filelib:is_file(Target), filelib:is_regular(Target)} of
                {true, true} ->
                    {ok, TargetHash} = esync_util:md5_file(Target),
                    if
                        TargetHash == SourceHash -> false;
                        true ->
                            case has_conflict(Target, TargetHash) of
                                true -> false;
                                false -> create_conflict(Source, Target)
                            end
                    end;
                {true, false} ->
                    create_conflict(Source, Target);
                _ ->
                    Target
            end;
        false ->
            case filelib:is_file(Target) of
                true ->
                    create_conflict(Source, Target);
                false ->
                    Target
            end
    end.

create_conflict(Source, Target) ->
    {ok, FileInfo} = file:read_file_info(Source, [{time, posix}]),
    #file_info{mtime=MTime} = FileInfo,
    NTarget = find_name(Target ++ ".conflict-" ++ integer_to_list(MTime), 0),
    ets:update_counter(?STATS, conflicts, {2, 1}),
    ets:insert(?CONFLICTS, {NTarget, Source}),
    NTarget.


has_conflict(TargetFile, Hash) ->
    TargetDir = filename:dirname(TargetFile),
    Files = filelib:wildcard("*", TargetDir),
    find_conflict(Files, TargetDir, Hash).

find_conflict([], _TargetDir, _Hash) ->
    false;
find_conflict(["." | Rest], TargetDir, Hash) ->
    find_conflict(Rest, TargetDir, Hash);
find_conflict([".." | Rest], TargetDir, Hash) ->
    find_conflict(Rest, TargetDir, Hash);
find_conflict([RelFile | Rest], TargetDir, Hash) ->
    File = filename:join(TargetDir, RelFile),
    case filelib:is_dir(File) of
        true ->
            find_conflict(Rest, TargetDir, Hash);
        false ->
            {ok, FileHash} = esync_util:md5_file(File),
            if
                FileHash == Hash -> true;
                true -> find_conflict(Rest, TargetDir, Hash)
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
                false ->
                    ok;
                Target2 ->
                    ets:update_counter(?STATS, ncopy, {2, 1}),
                    catch do_copy(Source, Target2)
            end;
        false ->
            ok
    end,
    ets:update_counter(?STATS, progress, {2, 1}).


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
    wait_for(progress, N),
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
