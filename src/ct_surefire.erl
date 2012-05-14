%%%-------------------------------------------------------------------
%%% @private
%%% @copyright 2012 Ericsson AB.
%%% @doc       
%%% @end
%%%-------------------------------------------------------------------
-module(ct_surefire).

-behaviour(gen_event).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([add_handler/1]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([file_writer/1]).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("common_test/include/ct_event.hrl").
%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {file, axis=[], properties=[], package, hostname,
                curr_suite, curr_suite_ts, curr_group = [], curr_tc,
                curr_log_dir,
                timer, tc_log,
                test_cases = [],
                test_suites = [],
                url_base="",
                base_log_dir="",
                dest_dir}).

-record(testcase, { log, group, classname, name, time, failure, timestamp }).
-record(testsuite, { errors, failures, hostname, name, tests,
                     time, timestamp, id, package,
                     properties, testcases }).


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec add_handler(EventMgr) -> ok | ErrorRet
%% @doc Add this module as an event handler.
%% @end
%%--------------------------------------------------------------------
add_handler(EventMgr) ->
    gen_event:add_handler(EventMgr, ?MODULE, []).

%%====================================================================
%% Handler callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(ArgList) -> Return
%%         Return = {ok, State} |
%%                  Other
%% @doc Initialize the event handler.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok,
     #state{hostname=string:strip(os:cmd("hostname"), right,$\n)}};
init(UrlBase) ->
    case init:get_argument(surefire_dest_dir) of
        {ok, DestDir} ->
            {ok,
             #state{url_base=UrlBase,
                    hostname=string:strip(os:cmd("hostname"), right,$\n),
                    dest_dir=DestDir}};
        error ->
            {ok,
             #state{url_base=UrlBase,
                    hostname=string:strip(os:cmd("hostname"), right,$\n)}}
    end.

%%--------------------------------------------------------------------
%% @spec handle_event(Event, State) -> Return
%%         Return =  {ok, State}                                |
%%                   {swap_handler, Args1, State1, Mod2, Args2} |
%%                   remove_handler
%% @doc Handle an event.
%% @end
%%--------------------------------------------------------------------
handle_event(#event{name=Name}, State)
  when Name == start_make;
       Name == finished_make;
       Name == start_write_file;
       Name == finished_write_file ->
    {ok, State};
handle_event(#event{name=tc_start,
                    data={_Suite, {init_per_group, Group, _GroupProp}}},
             State) ->
    {ok, State#state{curr_group=[Group | State#state.curr_group],
                     curr_tc=init_per_group, timer=now()}};
handle_event(#event{name=tc_start, data={Suite, init_per_suite}}, State) ->
    Now= now(),
    {ok, State#state{curr_tc=init_per_suite, timer=Now,
                     curr_suite=Suite, curr_suite_ts=Now}};
handle_event(#event{name=tc_start, data={_Suite, Tc}}, State)
  when is_atom(Tc) ->
    {ok, State#state{curr_tc=Tc, timer=now()}};
handle_event(#event{name=tc_logfile, data={_SuiteFunc, TcLogFile}}, State) ->
    {ok, State#state{tc_log=TcLogFile}};
handle_event(#event{name=tc_done, data={Suite, end_per_suite, Result}},
             #state{file=FileWriter, timer=Ts, tc_log = Log,
                    curr_group=Groups}=State) ->
    LogLink = get_log_link(Log, State),
    Tc = end_tc(Suite, end_per_suite, Ts, Groups, Result, LogLink),
    Tcs = [Tc | State#state.test_cases],
    Total = length(Tcs),
    Succ = length(lists:filter(fun(#testcase{ failure = F }) ->
                                       F == passed
                               end,Tcs)),
    Fail = Total - Succ,
    TimeTakes = io_lib:format(
                  "~f",
                  [timer:now_diff(now(),State#state.curr_suite_ts) / 1000000]),
    TestSuite =
        #testsuite{name = atom_to_list(State#state.curr_suite),
                   package = State#state.package,
                   time = TimeTakes,
                   timestamp = now_to_string(State#state.curr_suite_ts),
                   errors = Fail, tests = Total, testcases = Tcs},
    Xml = to_xml(TestSuite),
    FileWriter ! {write, Xml},
    {ok, State#state{test_cases=[]}};
handle_event(#event{name=tc_done, data={Suite, FuncOrGroup, Result}},
             #state{timer=Ts, tc_log=Log, curr_group=Groups}=State) ->
    LogLink = get_log_link(Log, State),
    Tc = end_tc(Suite, FuncOrGroup, Ts, Groups, Result, LogLink),
    NewCurrGroup = get_new_group(FuncOrGroup, Groups),
    {ok, State#state{test_cases=[Tc | State#state.test_cases],
                     curr_group=NewCurrGroup}};
handle_event(#event{name=TcSkip, data={Suite, end_per_suite, Reason}},
             #state{file=FileWriter, timer=Ts, curr_group=Groups}=State)
  when TcSkip == tc_auto_skip; TcSkip == tc_user_skip ->
    Tc = end_tc(Suite, end_per_suite, Ts, Groups, {skipped, Reason}, ""),
    Tcs = [Tc | State#state.test_cases],
    Total = length(Tcs),
    Succ = length(lists:filter(fun(#testcase{ failure = F }) ->
                                       F == passed
                               end,Tcs)),
    Fail = Total - Succ,
    TimeTakes = io_lib:format(
                  "~f",
                  [timer:now_diff(now(),State#state.curr_suite_ts) / 1000000]),
    TestSuite =
        #testsuite{name = atom_to_list(State#state.curr_suite),
                   package = State#state.package,
                   time = TimeTakes,
                   timestamp = now_to_string(State#state.curr_suite_ts),
                   errors = Fail, tests = Total, testcases = Tcs},
    Xml = to_xml(TestSuite),
    FileWriter ! {write, Xml},
    {ok, State#state{test_cases=[]}};
handle_event(#event{name=TcSkip, data={Suite, FuncOrGroup, Reason}},
             #state{timer=Ts, curr_group=Groups}=State)
  when TcSkip == tc_auto_skip; TcSkip == tc_user_skip ->
    Tc = end_tc(Suite, FuncOrGroup, Ts, Groups, {skipped, Reason}, ""),
    NewCurrGroup = get_new_group(FuncOrGroup, Groups),
    {ok, State#state{test_cases=[Tc | State#state.test_cases],
                     curr_group=NewCurrGroup}};
handle_event(#event{name=test_start, data={_Time, LogDir}}, State) ->
    SelectedLogDir = create_log_dir_filename(LogDir, State),
    LogFile = filename:join(SelectedLogDir, "junit_report.xml"),
    Pid = proc_lib:spawn(?MODULE, file_writer, [LogFile]),
    Pid ! {write, "<?xml version=\"1.0\" encoding= \"UTF-8\" ?>"},
    Pid ! {write, "<testsuites>"},
    {ok, State#state{file=Pid}};
handle_event(#event{name=test_done}, #state{file=FileWriter}=State) ->
    FileWriter ! {write, "</testsuites>"},
    FileWriter ! {self(), close},
    receive {FileWriter, closed} -> ok end,
    {ok, State};
handle_event(#event{name=start_logging, data=LogDir}, State) ->
    RealRootDir = filename:dirname(LogDir),
    io:format(user, "RealRootLogDir: ~s~n", [RealRootDir]),
    {ok, State#state{base_log_dir=RealRootDir}};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, State) -> Return
%%         Return = {ok, Reply, State}                                |
%%                  {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                  {remove_handler, Reply}
%% @doc Handle a call.
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> Return
%%         Return = {ok, State}                                |
%%                  {swap_handler, Args1, State1, Mod2, Args2} |
%%                  remove_handler
%% @doc Handle all non event/call messages.
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Arg, State) -> void()
%%         Arg = Args |
%%               {stop, Reason} |
%%               stop |
%%               remove_handler |
%%               {error, {'EXIT', Reason}} |
%%               {error, Term}
%% @doc Terminate the handler.
%% @end
%%--------------------------------------------------------------------
terminate(_Arg, #state{}) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
end_tc(Suite, FuncOrGroup, Ts, Groups, Result, Log) ->
    ClassName = atom_to_list(Suite),
    Name = get_tc_name(FuncOrGroup),
    TimeTakes = io_lib:format("~f", [timer:now_diff(now(),Ts) / 1000000]),
    PGroup = string:join([atom_to_list(Group) ||
                             Group <- lists:reverse(Groups)],"."),
    Res = get_result(Result),

    #testcase{log = Log,
              timestamp = now_to_string(Ts),
              classname = ClassName,
              group = PGroup,
              name = Name,
              time = TimeTakes,
              failure = Res}.

get_tc_name({Conf, _Group, _GroupProp}) ->
    Conf;
get_tc_name(Func) when is_atom(Func) ->
    Func.

get_result(ok) ->
    passed;
get_result({skipped, SkipReason}) ->
    {skipped, lists:flatten(io_lib:format("~p",[SkipReason]))};
get_result({failed, FailReason}) ->
    {fail, lists:flatten(io_lib:format("~p",[FailReason]))}.

get_new_group({end_per_group, _Group, _GroupProp}, Groups) ->
    tl(Groups);
get_new_group(_FuncOrGroup, Groups) ->
    Groups.

now_to_string(Now) ->
    {{YY,MM,DD},{HH,Mi,SS}} = calendar:now_to_local_time(Now),
    io_lib:format("~p-~s-~sT~s:~s:~s",[YY,adj(MM),adj(DD),adj(HH),adj(Mi),
                                       adj(SS)]).

adj(Int) ->
    string:right(integer_to_list(Int), 2, $0).


to_xml(#testcase{group = Group, classname = CL, log = L, name = Name,
                 time = T, timestamp = TS, failure = F}) ->
    N = atom_to_list(Name),
    ["<testcase ",
     [["group=\"",Group,"\""]||Group /= ""]," "
     "name=\"",N,"\" "
     "time=\"",T,"\" "
     "timestamp=\"",TS,"\" ",
     [["log=\"",L,"\""]||L /= ""], ">",
     case F of
         passed ->
             [];
         {skipped,Reason} ->
             case L of
                 "" ->
                     ["<skipped type=\"skip\" message=\"Test ",N," in ",CL,
                      " skipped!\">", sanitize(Reason),"</skipped>"];
                 _Else ->
                     ["<skipped type=\"skip\" message=\"Test ",N," in ",CL,
                      " skipped!\">", "Test log link: ", L, "\n",
                      sanitize(Reason),"</skipped>"]
                 end;
         {fail,Reason} ->
             case L of
                 "" ->
                     ["<failure message=\"Test ",N," in ",CL,
                      " failed!\" type=\"crash\">",
                      sanitize(Reason), "</failure>"];
                 _Else ->
                     ["<failure message=\"Test ",N," in ",CL,
                      " failed!\" type=\"crash\">", "Test log link: ", L, "\n",
                      sanitize(Reason), "</failure>"]
             end
     end,"</testcase>"];
to_xml(#testsuite{ package = P, hostname = H, errors = E, time = Time, timestamp = TS,
                   tests = T, name = N, testcases = Cases }) ->
    ["<testsuite ",
     [["package=\"",P,"\" "]||P /= undefined],
     [["hostname=\"",P,"\" "]||H /= undefined],
     [["name=\"",N,"\" "]||N /= undefined],
     [["time=\"",Time,"\" "]||Time /= undefined],
     [["timestamp=\"",TS,"\" "]||TS /= undefined],
     "errors=\"",integer_to_list(E),"\" "
     "tests=\"",integer_to_list(T),"\">",
     [to_xml(Case) || Case <- lists:reverse(Cases)],
     "</testsuite>"];
to_xml(#state{ test_suites = TestSuites, axis = Axis, properties = Props }) ->
    ["<testsuites>",properties_to_xml(Axis,Props),[to_xml(TestSuite) || TestSuite <- TestSuites],"</testsuites>"].

properties_to_xml(Axis,Props) ->
    ["<properties>",
     [["<property name=\"",Name,"\" axis=\"yes\" value=\"",Value,"\" />"] || {Name,Value} <- Axis],
     [["<property name=\"",Name,"\" value=\"",Value,"\" />"] || {Name,Value} <- Props],
     "</properties>"
    ].

sanitize([$>|T]) ->
    "&gt;" ++ sanitize(T);
sanitize([$<|T]) ->
    "&lt;" ++ sanitize(T);
sanitize([$"|T]) ->
    "&quot;" ++ sanitize(T);
sanitize([$'|T]) ->
    "&apos;" ++ sanitize(T);
sanitize([$&|T]) ->
    "&amp;" ++ sanitize(T);
sanitize([H|T]) ->
    [H|sanitize(T)];
sanitize([]) ->
    [].

get_log_link(Log, State) ->
    RelativeLink = lists:nthtail(length(State#state.base_log_dir), Log),
    State#state.url_base ++ RelativeLink.

file_writer(File) ->
    {ok, D} = file:open(File, [write]),
    loop(D).

loop(File) ->
    receive
        {write, Xml} ->
            ok = io:format(File, "~s", [Xml]),
            loop(File);
        {Pid, close} ->
            catch file:sync(File),
            catch file:close(File),
            Pid ! {self(), closed}
    end.

create_log_dir_filename(LogDir, State) ->
    case State#state.dest_dir of
        undefined ->
            LogDir;
        DestDir ->
            DestDir
    end.

example_xml() ->
    "<testsuites>"
        "<testsuite errors=\"1\" failures=\"1\" hostname=\"host\" name=\"suitename\" "
        "tests=\"3\" time=\"0.002\" timestamp=\"2010-09-21T14:21:05\" "
        "id=\"1\" package=\"pkg\">"
        "<properties>"
        "<property name=\"otp_ver\" value=\"R14B\"/>"
        "</properties>"
        "<testcase classname=\"test_SUITE\" name=\"test\" time=\"0.0001\"/>"
        "<testcase classname=\"test_SUITE\" name=\"test1\" time=\"0.0001\">"
        "<failure message=\"test_SUITE:test1 failed!\" type=\"crash\">"
        "Test info"
        "</failure>"
        "</testcase>"
        "<testcase classname=\"test_SUITE\" name=\"test2\" time=\"0.0001\">"
        "<error type=\"crash\">"
        "Error message!"
        "</error></testcase>"
        "<system-out/>"
        "<system-err/>"
        "</testsuite>"
        "<testsuite errors=\"1\" failures=\"1\" hostname=\"host\" name=\"suitename2\" "
        "tests=\"3\" time=\"0.002\" timestamp=\"2010-09-21T14:21:05\" "
        "id=\"2\" package=\"akjdgaskjhg\">"
        "<properties>"
        "<property name=\"otp_ver\" value=\"R14B01\"/>"
        "</properties>"
        "<testcase classname=\"test1_SUITE\" name=\"test\" time=\"0.0001\"/>"
        "<testcase classname=\"test1_SUITE\" name=\"test1\" time=\"0.0001\">"
        "<failure message=\"test_SUITE1:test1 failed!\" type=\"crash\">"
        "Test info"
        "</failure>"
        "</testcase>"
        "<testcase classname=\"test1_SUITE\" name=\"test2\" time=\"0.0001\">"
        "<error type=\"crash\">"
        "Error message!"
        "</error></testcase>"
        "<system-out/>"
        "<system-err/>"
        "</testsuite>"
        "</testsuites>".

%%% XML spec of junit report format
%% start = testsuites

%% property = element property {
%% attribute name {text},
%% attribute value {text}
%% }

%% properties = element properties {
%% property*
%% }

%% failure = element failure {
%% attribute message {text},
%% attribute type {text},
%% text
%% }

%% testcase = element testcase {
%% attribute classname {text},
%% attribute name {text},
%% attribute time {text},
%% failure?
%% }

%% testsuite = element testsuite {
%% attribute errors {xsd:integer},
%% attribute failures {xsd:integer},
%% attribute hostname {text},
%% attribute name {text},
%% attribute tests {xsd:integer},
%% attribute time {xsd:double},
%% attribute timestamp {xsd:dateTime},
%% attribute id {text},
%% attribute package {text},
%% properties,
%% testcase*,
%% element system-out {text},
%% element system-err {text}
%% }
%% }

%% testsuites = element testsuites {
%% testsuite*
%% }
