%% ---------------------------------------------------------------------
%% File: svn2git.erl
%%
%% @author Torbjorn Tornkvist <tobbe@klarna.com>
%% @copyright 2010 Torbjorn Tornkvist
%% @doc Replay svn dumpfile in order to create a git repos.

-module(svn2git).

-export([run/2]).
%%-compile(export_all).


-include_lib("eunit/include/eunit.hrl").

-include("svndump.hrl").

-record(s, {dir
            , rev
            , log
            , date
            , author
            , changed=false
            , ignore=false
            , current=trunk
            , branches=[]
            , preds=default_preds()
         }).

-define(is_bool(B), ((B == true) orelse (B == false))).

-define(print(Fmt,Var), io:format(Fmt++" ~s=~p~n",[??Var,Var])).


dbg(R) ->
    case get(dbg) of
        true ->
            io:format("+ ~p.\n", [no_data(R)]),
            io:get_line('press enter');
        _ ->
            ok
    end.

%no_data(X)           -> X;
no_data(#change{}=C) -> C#change{data="..."};
no_data(X)           -> X.
    

%% @doc Default predicates to identify trunk/branch data
default_preds() ->
    [fun(Path) -> 
             {match, [X]} = re:run(Path, 
                                   <<"^.*trunk/(.*)">>,
                                   [{capture,[1],binary}]),
             {trunk, X}
     end,
     fun(Path) -> 
             {match, [B,X]} = re:run(Path, 
                                     <<"^.*branches/([^/]*)/(.*)">>,
                                     [{capture,[1,2],binary}]),
             {branch, B, X}
     end,
     fun(Path) -> 
             throw({ignore,Path})
     end
    ].
             

%% @doc Replay the commits in the Svn dump as Git commits.
run(SvnDumpFile, GitDir) when is_list(SvnDumpFile) andalso is_list(GitDir) ->
    case filelib:is_dir(GitDir) of
        false ->
            {ok, Bin} = file:read_file(SvnDumpFile),
            mkdir(GitDir),
            ok = file:set_cwd(GitDir),
            init_git(),
            feed(#s{dir=GitDir},
                 [fun i/2,
                  fun g/2,
                  fun r/2,
                  fun c/2
                 ],
                 Bin);
        _ ->
            {error, "Git directory already exist!"}
    end.

%% @doc Consume the svn dump.
feed(_, _, <<>>) -> ok;
feed(S, Fs, Bin) ->
    case svndump:scan_record(Bin) of
	{{[], [], <<>>}, <<>>} ->
	    ok;  % ignore trailing empty lines
        {R, Rest} ->
            dbg(R),
            feed(until(ignore(S, false), Fs, R), Fs, Rest)
    end.

%% @doc Apply the actions for each entry until success.
until(S, [], _R)    -> S;
until(S, [F|Fs], R) ->
    try F(S,R) of
        {break, S1} -> S1;
        {cont, S1}  -> until(S1, Fs, R)
    catch 
        throw:{ignore,_What} -> ignore(S,true)
    end.

    
%% ---------------------------------------------------------------------
%% @doc Commit the outstanding changes.
%%
%% Before a new revision is handled, we need to commit the outstanding
%% changes that we have made to the git repository. 
%%
i(#s{changed=true} = S, #revision{}) ->
    ci_git(S),
    {cont, changed(S,false)};
i(S, _) ->
    {cont, S}.

%% ---------------------------------------------------------------------
%% @doc Setup the proper branch.
%%
g(#s{current = Current} = S, #change{path=Path}) ->
    case get_branch(preds(S), Path) of
        Current -> {cont, S}; 
        Branch  -> {cont, co_git(S#s{current=Branch})}
    end;
g(S, _) ->
    {cont, S}.

preds(#s{preds=Ps}) -> Ps.
    

%% @doc Apply the predicates to find out what branch we are on.
get_branch([P|Ps], Path) ->
    try P(Path) of
        {trunk,_}         -> trunk;
        {branch,Branch,_} -> Branch
    catch 
        _:_ -> get_branch(Ps, Path)
    end;
get_branch([], Path) ->
    ?print("get_branch/2: ignoring path ", Path),
    throw({ignore, Path}).

get_path([P|Ps], Path) ->
    try P(Path) of
        {trunk, X}           -> X;
        {branch, _Branch, X} -> X
    catch 
        _:_ -> get_path(Ps, Path)
    end;
get_path([], Path) ->
    ?print("get_path/2: ignoring path ", Path),
    throw({ignore, Path}).


%% ---------------------------------------------------------------------
%% @doc The start of a svn revision, extract the revision info.
%%
r(S, #revision{number=N, properties=Ps, changes=_Cs}) -> 
    {break, props(S#s{rev=N}, Ps)};
r(S, _) -> 
    {cont, S}.

props(S, [{<<"svn:author">>, Author} | Ps]) -> props(S#s{author = Author}, Ps);
props(S, [{<<"svn:date">>,   Date} | Ps])   -> props(S#s{date = Date}, Ps);
props(S, [{<<"svn:log">>,    Log} | Ps])    -> props(S#s{log = Log}, Ps);
props(S, [_|Ps])                            -> props(S, Ps);
props(S, [])                                -> S.


%% ---------------------------------------------------------------------
%% @doc Perform the repository changes.
%%
c(S, #change{path=Path, kind=dir, action=add}) ->
    mkdir(b2l(get_path(preds(S), Path))),
    {break, changed(S, true)};
c(S, #change{path=Path, kind=file, action=Action, data=Data}) 
  when ((Action == add) orelse (Action == change)) ->
    Fname = b2l(get_path(preds(S), Path)),
    %?print("c: ", Fname),
    %?print("c: ", Data),
    ok = file:write_file(Fname, data(Data)),
    add_git(Fname),
    {break, changed(S, true)};
c(S, _) ->
    {cont, S}.

data(none) -> "";
data(Data) -> Data.
    

ignore(S, Bool)  when ?is_bool(Bool) -> S#s{ignore=Bool}.
changed(S, Bool) when ?is_bool(Bool) -> S#s{changed=Bool}.

mkdir(Dir) ->
    os:cmd("mkdir -p "++Dir).
    
b2l(B) when is_binary(B) -> binary_to_list(B);
b2l(L) when is_list(L)   -> L.

%%% --------------
%%% GIT operations    
%%% --------------

init_git() ->
    os:cmd("git init").

add_git(Fname) ->
    os:cmd("git add "++b2l(Fname)).

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
            
%% NB: Using '--date' seem to require vsn: 1.7.0.5
ci_git(#s{author=Author, date=Date, log=Log}) ->
    ok = file:write_file("/tmp/cmsg", no_empty_msg(Log)),
    Cmd = "git commit -a --allow-empty "
        "--author='"++author_git(Author)++"' "++
        "--date='"++date_git(Date)++"' "++
        "--file /tmp/cmsg",
    ?print("=== Commit: ", Cmd),
    Res = os:cmd(Cmd),
    ?print("--- Commit: ", Res).

no_empty_msg(<<>>)                    -> <<"_empty_">>;
no_empty_msg(Msg) when is_binary(Msg) -> Msg.
    

%% FIXME should map author against the users.txt file !!
author_git(Author) ->
    "Torbjorn Tornkvist <tobbe@klarna.com>".

date_git(SvnDate) ->
    [Date|_] = string:tokens(b2l(SvnDate), "."),
    Date.


-ifdef(EUNIT).

default_preds_trunk_test() ->
    Path = <<"tobbe/trunk/home/flundra/.gnus">>,
    F = hd(default_preds()), % FIXME use separate pred functions
    ?assertMatch({trunk,<<"home/flundra/.gnus">>}, F(Path)).

default_preds_branch_test() ->
    Path = <<"tobbe/branches/home/flundra/.gnus">>,
    F = hd(tl(default_preds())), % FIXME use separate pred functions
    ?assertMatch({branch,<<"home">>,<<"flundra/.gnus">>}, F(Path)).


%% FIXME setup the use of a proper users.txt file.
author_git_test() ->
    SvnAuthor = <<"tobbe">>,
    GitAuthor = "Torbjorn Tornkvist <tobbe@klarna.com>",
    ?assertMatch(GitAuthor, author_git(SvnAuthor)).

date_git_test() ->
    SvnDate = <<"2008-10-06T13:30:06.551146Z">>,
    GitDate = "2008-10-06T13:30:06",
    ?assertMatch(GitDate, date_git(SvnDate)).

-endif.

%% + {change,<<"tobbe/branches/home/flundra">>,dir,add,
%%           [{node_copyfrom_rev,41},{node_copyfrom_path,<<"tobbe/trunk">>}],
%%          [],none}.

            
