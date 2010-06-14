%% ---------------------------------------------------------------------
%% File: svn2git.erl
%%
%% @author Torbjorn Tornkvist <tobbe@klarna.com>
%% @copyright 2010 Torbjorn Tornkvist
%% @doc Replay svn dumpfile in order to create a git repos.

-module(svn2git).

-export([run/2
        ]).


-include_lib("eunit/include/eunit.hrl").

-include("svndump.hrl").

-record(s, {dir
            , rev
         }).

%% @doc
run(SvnDumpFile, GitDir) ->
    {ok, Bin} = file:read_file(SvnDumpFile),
    mkdir(GitDir),
    ok = file:set_cwd(GitDir),
    feed(#s{dir=GitDir},
         [fun r/2
          , fun c/2
         ],
         Bin).

    
feed(_, _, <<>>) -> ok;
feed(S, Fs, Bin) ->
    case svndump:scan_record(Bin) of
	{{[], [], <<>>}, <<>>} ->
	    ok;  % ignore trailing empty lines
        {R, Rest} ->
            io:format("+ ~p.\n", [R]),
            io:get_line('press enter'),
            feed(until(S, Fs, R), Fs, Rest)
    end.

until(S, [], _R)    -> S;
until(S, [F|Fs], R) ->
    try {S1, true} = F(S,R), S1
    catch _:_ -> until(S, Fs, R) end.
        


r(S, #revision{number=N, properties=_Ps, changes=_Cs}) -> {S#s{rev=N}, true};
r(_, _)                                                -> false.


% c(S, #change{path=Path, kind=dir, action=add, headers=Hs, properties=Ps, data=_}) ->

%%
%% FIXME:  check for trunk/branches/tags and setup the git repo accordingly
%%

c(S, #change{path=Path, kind=dir, action=add}) ->
    mkdir(b2l(Path)),
    {S, true};
c(S, #change{path=Path, kind=file, action=add, data=Data}) ->
    ok = file:write_file(b2l(Path), Data),
    {S, true};
c(_, _) ->
    false.


mkdir(Dir) ->
    os:cmd("mkdir -p "++Dir).
    
b2l(B) when is_binary(B) -> binary_to_list(B);
b2l(L) when is_list(L)   -> L.

    
