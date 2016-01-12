-module(esync).
-export([main/1]).

-define(STATS, stats).
-define(FILES, files).

-define(CQ, copy_queue).
-define(CJ, copy_jobs).
-define(BQ, build_queue).
-define(BJ, build_jobs).


-include_lib("kernel/include/file.hrl").

init_app() ->
    {ok, _} = application:ensure_all_started(esync),
    ?FILES = ets:new(?FILES, [ordered_set, public, named_table]),

    jobs:add_queue(?CQ,[passive]),
    jobs:add_queue(?CJ, [{standard_counter, 20},
                         {producer, fun copy_job/0}]),

    jobs:add_queue(?BQ,[passive]),
    jobs:add_queue(?BJ, [{standard_counter, 500},
                         {producer, fun build_job/0}]),

    ok.


maybe_copy(Source, Target) ->
    IsRegular = filelib:is_regular(Source),

    case IsRegular of
        true ->
            {ok, SourceHash} = esync_util:md5_file(Source),
            case filelib:is_file(Target) of
                true ->
                    {ok, TargetHash} = esync_util:md5_file(Target),
                    if
                        TargetHash == SourceHash -> ok;
                        true ->
                            {ok, FileInfo} = file:read_file_info(Source, [{time, posix}]),
                            #file_info{mtime=MTime} = FileInfo,
                            NTarget = find_name(Target ++ "."++ integer_to_list(MTime), 0),
                            ets:insert(?FILES, {Source, NTarget})
                    end;
                false -> ets:insert(?FILES, {Source, Target})
            end;
        false ->
            Target2 = case filelib:is_file(Target) of
                          true ->
                              {ok, FileInfo} = file:read_file_info(Source, [{time, posix}]),
                              #file_info{mtime=MTime} = FileInfo,
                              find_name(Target ++ "."++ integer_to_list(MTime), 0);
                          false ->
                              Target
                      end,
            ets:insert(?FILES, {Source, Target2})
    end,
    ok.

build_job() ->
    [{_, {Source, Target, Server}}] = jobs:dequeue(?BQ, 1),
    try
        maybe_copy(Source, Target),
        Server ! ack
    catch
        Error ->
            esync_util:abort("~n~s: ~p~n", [Source, Error])
    end.


find_name(Target, Inc) ->
    NewTarget = Target ++ "." ++ integer_to_list(Inc),
    case filelib:is_file(NewTarget) of
        true -> find_name(Target, Inc + 1);
        false -> NewTarget
    end.

copy_job() ->
    [{_, {Source, Target, Server}}] = jobs:dequeue(?CQ, 1),
    case filelib:is_file(Source) of
        true ->
            TargetDir = filename:dirname(Target),
            esync_util:make_dir(TargetDir),
            filelib:ensure_dir(Target),
            {ok, _} = file:copy(Source, Target);
        false ->
            ok
    end,
    Server ! ack.


build_list(Source, Target) ->
    Files = filelib:wildcard("*", Source),
    N = process_path(Files, Source, #{ source => Source, target => Target}, 0),
    wait_for_jobs(N).


process_path([], _Dir, _State, N) ->
    N;
process_path([".DS_Store" | Rest], Dir, State, N) ->
    process_path(Rest, Dir, State, N);
process_path([File | Rest], Dir, State, N) ->
    #{ source := Source,
       target := Target} = State,

    SourceFile = filename:join(Dir, File),
    RelPath = esync_util:relpath(SourceFile, Source),
    N2 = case filelib:is_dir(SourceFile) of
             true ->
                 Files = filelib:wildcard("*", SourceFile),
                 process_path(Files, SourceFile, State, N);
             false ->
                 TargetFile = filename:join(Target, RelPath),
                 ok = jobs:enqueue(?BQ, {SourceFile, TargetFile, self()}),
                 N+1
         end,
    process_path(Rest, Dir, State, N2).

wait_for_jobs(0) ->
    ok;
wait_for_jobs(N) ->
    receive
        ack -> wait_for_jobs(N-1)
    end.

process_files('$end_of_table', N) ->
    wait_for_jobs(N),
    io:format("done~n", []);
process_files({[{Source, Target}], Cont}, N) ->
    ok = jobs:enqueue(?CQ, {Source, Target, self()}),
    process_files(ets:select(Cont), N+1).

main([Source, Target]) ->
    io:format("building file list ... ", []),
    ok = init_app(),
    esync_util:make_dir(Target),
    build_list(Source, Target),
    io:format("done~n", []),
    io:format("copy files ... ", []),
    Res = ets:select(?FILES, [{{'$1','$2'},[],[{{'$1','$2'}}]}], 1),
    process_files(Res, 0);
main(_) ->
    esync_util:abort("usage: esync SOURCE TARGET~n", []).
