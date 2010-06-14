%% ---------------------------------------------------------------------
%% File: svn2git.erl
%%
%% @author Torbjorn Tornkvist <tobbe@klarna.com>
%% @copyright 2010 Torbjorn Tornkvist
%% @doc Replay svn dumpfile in order to create a git repos.

-module(svn2git).

-export([run/2]).


-include_lib("eunit/include/eunit.hrl").

-include("svndump.hrl").

-record(s, {dir
            , rev
            , log
            , date
            , author
            , changed=false
            , current=trunk
            , branches=[]
            , preds=default_preds()
         }).


dbg(R) ->
    io:format("+ ~p.\n", [R]),
    io:get_line('press enter').


%% @doc Default predicates to identify trunk/branch data
default_preds() ->
    [fun(Path) -> 
             {match, X} = re:run(Path, 
                                 <<"^.*trunk/(.*)">>,
                                 [{capture,[1],binary}]),
             {trunk, X}
     end,
     fun(Path) -> 
             {match, B, X} = re:run(Path, 
                                 <<"^.*branches/(.*)/(.*)">>,
                                 [{capture,[1,2],binary}]),
             {branch, B, X}
     end
    ].
             

%% @doc Replay the commits in the Svn dump as Git commits.
run(SvnDumpFile, GitDir) ->
    {ok, Bin} = file:read_file(SvnDumpFile),
    mkdir(GitDir),
    ok = file:set_cwd(GitDir),
    init_git(),
    feed(#s{dir=GitDir},
         [fun g/2
          , fun r/2
          , fun c/2
         ],
         Bin).

%% @doc Consume the svn dump.
feed(_, _, <<>>) -> ok;
feed(S, Fs, Bin) ->
    case svndump:scan_record(Bin) of
	{{[], [], <<>>}, <<>>} ->
	    ok;  % ignore trailing empty lines
        {R, Rest} ->
            dbg(R),
            feed(until(S, Fs, R), Fs, Rest)
    end.

%% @doc Apply the actions for each entry until success.
until(S, [], _R)    -> S;
until(S, [F|Fs], R) ->
    case F(S,R) of
        {break, S1} -> S1;
        {cont, S1}  -> until(S1, Fs, R) 
    end.


%% @doc Setup the proper branch.
g(#s{current = Current, preds=Ps} = S, #change{path=Path}) ->
    case get_branch(Ps, Path) of
        Current -> {cont, S}; 
        Branch  -> {cont, co_git(S#s{current=Branch})}
    end;
g(S, _) ->
    {cont, S}.

%% @doc Apply the predicates to find out what branch we are on.
get_branch([P|Ps], Path) ->
    try P(Path) of
        {trunk,_}         -> trunk;
        {branch,Branch,_} -> Branch
    catch 
        _:_ -> get_branch(Ps, Path)
    end;
get_branch([], Path) ->
    throw({get_branch, Path}).

%% @doc Checkout the (new) current branch.
co_git(#s{current=trunk} = S) ->
    os:cmd("git checkout master"),
    S;
co_git(#s{current=Branch, branches=Bs} = S) ->
    case lists:member(Branch, Bs) of
        true ->
            os:cmd("git checkout "++b2l(Branch)),
            S;
        false ->
            os:cmd("git checkout -b "++b2l(Branch)),
            S#s{branches=[Branch|Bs]}
    end.
            

%% @doc The start of a svn revision, extract the revision info.
r(S, #revision{number=N, properties=Ps, changes=_Cs}) -> 
    {break, props(S#s{rev=N}, Ps)};
r(S, _) -> 
    {cont, S}.

props(S, [{<<"svn:author">>, Author} | Ps]) -> props(S#s{author = Author}, Ps);
props(S, [{<<"svn:date">>,   Date} | Ps])   -> props(S#s{date = Date}, Ps);
props(S, [{<<"svn:log">>,    Log} | Ps])    -> props(S#s{log = Log}, Ps);
props(S, [_|Ps])                            -> props(S, Ps);
props(S, [])                                -> S.


%% @doc Perform the repository changes.
c(S, #change{path=Path, kind=dir, action=add}) ->
    mkdir(b2l(Path)),
    {break, S};
c(S, #change{path=Path, kind=file, action=add, data=Data}) ->
    ok = file:write_file(b2l(Path), Data),
    {break, S};
c(S, _) ->
    {cont, S}.


init_git() ->
    os:cmd("git init").

mkdir(Dir) ->
    os:cmd("mkdir -p "++Dir).
    
b2l(B) when is_binary(B) -> binary_to_list(B);
b2l(L) when is_list(L)   -> L.

    
