%% ---------------------------------------------------------------------
%% File: svn2git.erl
%%
%% @author Torbjorn Tornkvist <tobbe@klarna.com>
%% @copyright 2010 Torbjorn Tornkvist
%% @doc Replay svn dumpfile in order to create a git repos.
%% ---------------------------------------------------------------------
-module(svn2git).

%%-compile(export_all).
-export([run/2
         , run/3
         , run/4
         , parse_users_txt/1
        ]).


-include_lib("eunit/include/eunit.hrl").

-include("svndump.hrl").

-record(s, {dir
            , rev
            , log
            , date
            , author
            , users=[]
            , changed=false
            , ignore=false
            , current=trunk
            , merge_p=false
            , merge_from
            , merge_to
            , bp
            , branches=[]
            , preds=default_preds()
         }).

-define(is_bool(B), ((B == true) orelse (B == false))).

-define(print(Fmt,Var), io:format(Fmt++" ~s=~p~n",[??Var,Var])).


%% -------------------------------------------------------
%% @doc Replay the commits in the Svn dump as Git commits.
run(SvnDumpFile, GitDir) ->
    run(SvnDumpFile, GitDir, "").

%% -------------------------------------------------------
%% @doc As run/2 + a users.txt file path.
run(SvnDumpFile, GitDir, UserTxtFile) ->
    run(SvnDumpFile, GitDir, UserTxtFile, []).

%% -------------------------------------------------------
%% @doc As run/3 + a list of options
%%
%%   {bp, Revision::int() }  --  Enter 'debugger' at Revision
%%
run(SvnDumpFile, GitDir, UserTxtFile, Opts) 
  when is_list(SvnDumpFile) andalso 
       is_list(UserTxtFile) andalso
       is_list(GitDir) andalso
       is_list(Opts) ->
    case filelib:is_dir(GitDir) of
        false ->
            {ok, Bin} = file:read_file(SvnDumpFile),
            Users = maybe_parse_users_txt(UserTxtFile),
            mkdir(GitDir),
            ok = file:set_cwd(GitDir),
            init_git(),
            feed(opts(#s{dir=GitDir,users=Users}, Opts),
                 % The actions 
                 [fun ci/2,     % commit outstanding changes
                  fun br/2,     % setup proper branch
                  fun rv/2,     % check for a new svn revision
                  fun ch/2      % perform any repository changes
                 ],
                 Bin);
        _ ->
            {error, "Git directory already exist!"}
    end.


maybe_parse_users_txt(Fname) ->
    try parse_users_txt(Fname)
    catch 
        _:_ -> 
            io:format("WARNING: No valid users.txt file!~n",[]),
            [] 
    end.
            
parse_users_txt(Fname) ->
    {ok,Bin} = file:read_file(Fname),
    F = fun(Line) ->
                {match, [Uid,Name,Email]} = 
                    re:run(Line,
                           "([^ ]*)[ =]*"
                           "([^<]*)"
                           "(.*)",
                           [{capture,[1,2,3],list}]),
                {Uid,string:strip(Name),Email}
        end,
    [F(Line) || Line <- string:tokens(b2l(Bin),"\n")].




%% Parse and save options
opts(S, [{bp,N}|Opts]) when is_integer(N) -> opts(S#s{bp=N}, Opts);
opts(S, [_|Opts])                         -> opts(S, Opts);
opts(S, [])                               -> S.
    

debugger(R) ->
    case get(dbg) of
        true ->
            io:format("+ ~p.\n", [no_data(R)]),
            io:get_line('press enter');
        _ ->
            ok
    end.

%% Set breakpoint
bp(true) -> put(dbg,true);
bp(_)    -> false.

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
             {match, [X]} = re:run(Path, 
                                   <<"^.*trunk">>,
                                   [{capture,[1],binary}]),
             {trunk, X}
     end,
     fun(Path) -> 
             {match, [B,X]} = re:run(Path, 
                                     <<"^.*branches/([^/:]*)/(.*)">>,
                                     [{capture,[1,2],binary}]),
             {branch, B, X}
     end,
     fun(Path) ->
             {match, [B]} = re:run(Path, 
                                   <<"^.*branches/([^ :]*).*">>,
                                   [{capture,[1],binary}]),
             {branch, B, <<>>}
     end,
     fun(Path) -> 
             {match, [B]} = re:run(Path, 
                                   <<"^.*branches/([^/ ]*).*">>,
                                   [{capture,[1],binary}]),
             {branch, B, <<>>}
     end,
     fun(Path) -> 
             throw({ignore,Path})
     end
    ].
             

%% @doc Consume the svn dump.
feed(_, _, <<>>) -> ok;
feed(S, Fs, Bin) ->
    case svndump:scan_record(Bin) of
	none ->
            % Commit any final outstanding data.
            S#s.changed == true andalso ci_git(S),
	    ok; 
        {R, Rest} ->
            debugger(R),
            feed(until(S, Fs, R), Fs, Rest)
    end.

%% @doc Apply the actions for each entry until success.
until(S, [], _R)    -> S;
until(S, [F|Fs], R) ->
    try F(S,R) of
        {break, S1} -> S1;
        {cont, S1}  -> until(S1, Fs, R)
    catch 
        throw:{ignore,_What} -> S
    end.

    
%% ---------------------------------------------------------------------
%% @doc Commit the outstanding changes.
%%
%% Before a new revision is handled, we need to commit the outstanding
%% changes that we have made to the git repository. 
%%
ci(#s{changed=true} = S, #revision{}) ->
    ci_git(S),
    {cont, changed(S,false)};
ci(S, _) ->
    {cont, S}.

%% ---------------------------------------------------------------------
%% @doc Setup the proper branch.
%%
br(#s{current = Current} = S, #change{path=Path}) ->
    case get_branch(preds(S), Path) of
        Current -> {cont, S}; 
        Branch  -> {cont, co_git(S#s{current=Branch})}
    end;
br(S, _) ->
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
rv(#s{bp=BP} = S, #revision{number=N, properties=Ps, changes=_Cs}) -> 
    bp(N == BP),
    {break, props(ignore(S#s{rev=N},false), Ps)};
rv(S, _) -> 
    {cont, S}.

props(S, [{<<"svn:author">>, Author} | Ps]) -> props(S#s{author = Author}, Ps);
props(S, [{<<"svn:date">>,   Date} | Ps])   -> props(S#s{date = Date}, Ps);
props(S, [{<<"svn:log">>,    Log} | Ps])    -> props(merge_p(S#s{log = Log}), Ps);
props(S, [_|Ps])                            -> props(S, Ps);
props(S, [])                                -> S.

merge_p(#s{log=Log, current=Current} = S) ->
    ?print("merge_p: ", Log),
    ?print("merge_p: ", Current),
    case re:run(Log, <<".*([mM][eE][rR][gG][eE]).*">>) of
        nomatch -> S;
        _ ->
            % Try to guess From/To of a possible merge.
            try get_branch(preds(S), Log) of

                Current ->
                    S#s{merge_p    = true, 
                        merge_from = Current};

                trunk ->
                    S#s{merge_p    = true, 
                        merge_from = Current, 
                        merge_to   = <<"master">>};

                Branch ->
                    S#s{merge_p    = true, 
                        merge_from = Current, 
                        merge_to   = Branch}

            catch throw:{ignore,_What} -> S#s{merge_p = true} end

    end.
                               


%% ---------------------------------------------------------------------
%% @doc Perform the repository changes.
%%
ch(#s{merge_p=true} = S, #change{kind=dir, action=change} = X) ->
    {break, ignore(maybe_merge(S, X), true)};
ch(#s{ignore=false} = S, #change{path=Path, kind=dir, action=Action})
  when ((Action == add) orelse (Action == change)) ->
    mkdir(b2l(get_path(preds(S), Path))),
    {break, changed(S, true)};
ch(#s{ignore=false} = S, #change{path=Path, kind=file, action=Action, data=Data}) 
  when ((Action == add) orelse (Action == change)) ->
    Fname = b2l(get_path(preds(S), Path)),
    %?print("c: ", Fname),
    %?print("c: ", Data),
    ok = file:write_file(Fname, data(Data)),
    add_git(Fname),
    {break, changed(S, true)};
ch(S, _) ->
    {cont, S}.

maybe_merge(#s{} = S, #change{path=Path} = X) ->
    {From, To} = check_for_merge_info(S, X),
    try trunk2master(get_branch(preds(S), Path)) of
        To ->
            ?print("maybe_merge: ", From),
            ?print("maybe_merge: ", To),
            merge_git(S, From, To),
            S#s{merge_p = false};
        ToBranch ->
            ?print("maybe_merge: ", ToBranch),
            ?print("maybe_merge: ", To),
            S#s{merge_p = false}
    catch 
        throw:{ignore,_What} ->
            ?print("maybe_merge: ", Path),
            merge_git(S, From, To),
            S#s{merge_p = false}
    end.

check_for_merge_info(#s{merge_from=From0, merge_to=To0} = S, #change{path=Path} = X) ->
    Ps = properties(X),
    case lists:keysearch(<<"svn:mergeinfo">>,1,Ps) of
        {value, {_,ToPath}} ->
            % Ex.Path: <<"/branches/acceptor-11334:162-163">>
            Preds = preds(S),
            try 
                FromBranch = get_branch(Preds, ToPath),
                ToBranch   = get_branch(Preds, Path),
                ?print("check_for_merge_info: ", FromBranch),
                ?print("check_for_merge_info: ", ToBranch),
                {trunk2master(FromBranch),
                 trunk2master(ToBranch)}
            catch
                throw:{ignore,_What} ->
                    {From0, To0}
            end;
        _ ->
            {From0, To0}
    end.


properties(#change{properties=Ps})   -> Ps;
properties(#revision{properties=Ps}) -> Ps.
    
trunk2master(trunk)  -> "master";
trunk2master(Branch) -> Branch.
    

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
ci_git(#s{author=Author, date=Date, log=Log, users=Users}) ->
    ok = file:write_file("/tmp/cmsg", no_empty_msg(Log)),
    Cmd = "git commit -a --allow-empty "
        "--author='"++author_git(Author,Users)++"' "++
        "--date='"++date_git(Date)++"' "++
        "--file /tmp/cmsg",
    ?print("=== Commit: ", Cmd),
    Res = os:cmd(Cmd),
    ?print("--- Commit: ", Res).

no_empty_msg(<<>>)                    -> <<"_empty_">>;
no_empty_msg(Msg) when is_binary(Msg) -> Msg.

merge_git(S, From, To) ->            
    Res = os:cmd("(git checkout "++b2l(To)++
                 "; git merge --no-ff "++b2l(From)++")"),
    ?print("merge_git: ", Res),
    ci_git(S).


%% @doc Get Name and Email from user list or fallback to USER@HOST.
author_git(Author, Users) ->
    try 
        {value, {_,Name,Email}} = lists:keysearch(b2l(Author), 1, Users),
        Name++" "++Email
    catch
        _:_ ->
            {ok,Host} = inet:gethostname(),
            User = os:getenv("USER"),
            User++" <"++User++"@"++Host++">"
    end.

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
    F = hd(tl(tl(default_preds()))), % FIXME use separate pred functions
    ?assertMatch({branch,<<"home">>,<<"flundra/.gnus">>}, F(Path)).

author_git_test() ->
    SvnAuthor = <<"tobbe">>,
    GitAuthor = "Torbjorn Tornkvist <tobbe@klarna.com>",
    Users = [{"tobbe","Torbjorn Tornkvist","<tobbe@klarna.com>"}],
    ?assertMatch(GitAuthor, author_git(SvnAuthor,Users)).

date_git_test() ->
    SvnDate = <<"2008-10-06T13:30:06.551146Z">>,
    GitDate = "2008-10-06T13:30:06",
    ?assertMatch(GitDate, date_git(SvnDate)).

-endif.

%% + {change,<<"tobbe/branches/home/flundra">>,dir,add,
%%           [{node_copyfrom_rev,41},{node_copyfrom_path,<<"tobbe/trunk">>}],
%%          [],none}.

            
