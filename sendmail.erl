%% File   : sendmail.erl
%% Author : Klacke <klacke@hyber.org>,
%%          Johan Bevemyr <jb@son.bevemyr.com>,
%%          Håkan Stenholm <hokan@klarna.com>,
%%          Richard Carlsson <richardc@klarna.com>
%%
%% Description : send mail using local sendmail; based on sendmail.erl
%% by Klacke and smtp.erl by Johan Bevemyr, with code for RFC1522 by
%% Håkan Stenholm. Major cleanup and rewrites by Richard Carlsson.
%%
%% Copyright (C) Johan Bevemyr 2004, Klacke <klacke@hyber.org> 2005,
%% Håkan Stenholm 2009, Richard Carlsson 2009.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.

%% TODO: allow list of recipients?

-module(sendmail).

-export([ create/4
        , create/5
        , send/4
        , send/5
        , send_data/3
        , send_data/4
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(NL, "\n").    % unix sendmail expects LF-terminated lines

%% API

create(To, From, Subject, Message) ->
    create(To, From, Subject, Message, []).

create(To, From, Subject, Message, Opts) ->
    data(To, From, Subject, Message, Opts).

send(To, From, Subject, Message) ->
    send(To, From, Subject, Message, []).

send(To, From, Subject, Message, Opts) ->
    send_data(To, From, create(From, To, Subject, Message, Opts), Opts).

%% returns {ExitCode, CmdOutput}
send_data(To, From, Data, _Opts) ->
    %% should perhaps support other methods as well, such as direct SMTP
    %% (in that case, this module should probably be renamed)
    sendmail(From, To, Data).

%% returns {ExitCode, CmdOutput}
send_data(From, Data, _Opts) ->
    sendmail(From, Data).

%% ------------------------------------------------------------------------
%% The rest is internal functionality

sendmail(From, To, Data) ->
    PortCmd = port_cmd(From, shell_quote(To)),
    sendmail_1(PortCmd, Data).

%% Extract recipients from the message headers instead of manually
%% specifying them.
sendmail(From, Data) ->
    %% sendmail options used:
    %%   -t  : extract recipients from message headers
    PortCmd = port_cmd(From, "-t"),
    sendmail_1(PortCmd, Data).

port_cmd(From, ExtraOpts) ->
    %% sendmail options used:
    %%   -f  : set envelope sender (can only be done by trusted user)
    %%   -bm : message on stdin
    "/usr/sbin/sendmail -bm -f " ++ From ++ " " ++ ExtraOpts.

sendmail_1(PortCmd, Data) ->
    %% TODO: use spawn_executable to avoid need for shell quote
    P = open_port({spawn, PortCmd}, [stderr_to_stdout, exit_status, eof]),
    %% sendmail reads its standard input up to a line consisting only of a
    %% single dot
    P ! {self(), {command, [Data, "\n.\n"]}},
    sendmail_wait(P, undefined, false, []).

sendmail_wait(P, Status, true = _Eof, Ds) when Status =/= undefined ->
    erlang:port_close(P),
    {Status, lists:flatten(lists:reverse(Ds))};
sendmail_wait(P, Status, Eof, Ds) ->
    receive
        {P, eof} ->
            sendmail_wait(P, Status, true, Ds);
        {P, {data, D}} ->
            sendmail_wait(P, Status, Eof, [D|Ds]);
        {P, {exit_status, S}} ->
            sendmail_wait(P, S, Eof, Ds)
    after 15000 ->
            erlang:port_close(P),
            {undefined, "sendmail command timed out\n" ++
                lists:flatten(lists:reverse(Ds))}
    end.

data(From, To, Subject, Message, Opts0) ->
    %% TODO: should accept additional headers as options
    Opts = proplists:expand([{text, [{content_type,"text/plain"}]},
                             {html, [{content_type,"text/html"}]}],
                            Opts0),
    ContentType = proplists:get_value(content_type, Opts, "text/plain"),
    Attached = proplists:get_value(attached, Opts, []),
    [
     mk_text_header("Subject", Subject) ++ ?NL,
     mk_header("From", From),
     mk_header("To", To),
     case Attached of
         [] ->
             [mk_header("Content-Type", ContentType),
              mk_header("Content-Transfer-Encoding", "8bit"),
              ?NL,
              Message
             ];
         _ ->
             Boundary = mk_boundary(),
             [
              mk_header("Mime-Version", "1.0"),
              mk_header("Content-Type",
                        ("Multipart/Mixed; boundary=\""
                         ++ Boundary ++ "\"")),
              mk_header("Content-Transfer-Encoding", "8bit"),
              ?NL,
              "--", Boundary,
              ?NL,
              mk_header("Content-Type",
                        ContentType ++ "; charset=us-ascii"),
              mk_header("Content-Transfer-Encoding", "8bit"),
              ?NL,
              Message,
              attachments(Boundary, Attached)
             ]
     end].

attachments(Boundary, []) ->
    [?NL, "--", Boundary, "--", ?NL];
attachments(Boundary, [{FileName,ContentType,Data}|Rest]) ->
    [?NL, "--", Boundary, ?NL,
     mk_header("Content-Type", ContentType),
     mk_header("Content-Transfer-Encoding", "base64"),
     mk_header("Content-Disposition",
               "attachment; filename=\"" ++ FileName ++ "\""),
     ?NL,
     base64:encode(Data),
     attachments(Boundary, Rest)
    ];
attachments(Boundary, [FileName|Rest]) when is_list(FileName) ->
    case file:read_file(FileName) of
        {ok, Data} ->
            ContentType = "application/octet-stream",  % safe default
            attachments(Boundary,
                        [{filename:basename(FileName),
                          ContentType,
                          Data} | Rest]);
        {error, Reason} ->
            throw({attachment_error, FileName, Reason})
    end.

mk_boundary() ->
    {N1, N2, N3} = now(),
    lists:flatten(io_lib:format("[~w:~w:~w]", [N1, N2, N3])).

%% Make an arbitrary (IO-) string safe to pass into a shell command.
%% Note that single quotes in the string are dropped.
%% (Perhaps they should be translated to '' ?) 
shell_quote(String) ->
    %% 1. Put single quotes around the string.
    "'" ++
	%% 2. Remove any single quote
	[C || C <- lists:flatten(String), C =/= $'  % ' emacs
	   ] 
	++ "'". 


%% * See RFC1522 for detail about encoding non-us-ascii in mail headers.
%% * RFC822 specifies the header layout in greater detail.

mk_header(_Key, []) -> [];
mk_header(Key, Val) -> Key ++ ": " ++ Val ++ ?NL.

-define(CONT, (?NL ++ " ")). % continues field on new line
-define(MAX_LENGTH, 76).     % RFC1522 - max length of line in multiline field

%% @spec mk_text_header(Title::string(),
%%                      Content::deep_string()) -> string()
%%
%% @doc Title: US-ASCII, e.g. "Subject" (no control chars, SP or ':').
%% Content: Latin-1, the text after Title. Output is Q-encoded Latin-1.
%% Will split the header over multiple lines if needed.
%%
%% This is only intended for unstructured `<text>' fields like "Subject" or
%% "Comments" where all of Content should be Q-encoded. Don't use this for
%% "From" or "To" fields!
%%
%% An empty field becomes "xxx: " rather than "xxx: =?ISO-8859-1?Q??=" for
%% the sake of clarity and to avoid possible mail header parsing issues.

mk_text_header(Title, []) ->
    Title ++ ": ";
mk_text_header(Title, Content) ->
    %% Note: folding of text (split over lines) should generally be done at
    %% LWSP or other structural item (e.g. address line) according to RFC822
    %% but here we simply split when the line gets too long.
    Charset = "ISO-8859-1",

    Head = "=?" ++ Charset ++ "?Q?",
    %% ":" would be ok according to RFC822, but ": " seams more common
    %% when looking at email examples and eml files
    FirstHead = Title ++ ": " ++ Head, 
    Tail = "?=",
    Text = q_encode_latin1(Content),

    %% Size of fixed elements on each line, ?CONT and ?NL are somewhat
    %% conservativly added to line length.
    %% 1 is added for LWSP from ?CONT on line no. 2+.
    %% Counting NL on final line ensures that lines don't get too long
    %% between fields
    HTLen = length(Head) + 1 + length(Tail) + length(?NL), 
    FTLen = length(FirstHead) + length(Tail) + length(?NL), 

    FirstHead ++ mk_text_header(FirstHead, Head, Tail, Text, 
                                          HTLen, FTLen, FTLen).

mk_text_header(_FirstHead, _Head, Tail, [] = _Text, 
                         _HTLen, _FTLen, _Len) ->
    %% no more text
    Tail;
mk_text_header(done = FirstHead, Head, Tail, [C|R] = Text, 
                         HTLen, FTLen, Len) ->
    %% 2:nd+ line
    %% can we fit another (encode) letter on this line?
    NewLen = Len + length(C),
    case NewLen > ?MAX_LENGTH of
	false -> C ++ 
		     mk_text_header(FirstHead, Head, Tail, R,
					      HTLen, FTLen, NewLen);
	%% C must be placed on new line
	true -> Tail ++ ?CONT ++ Head ++ 
		    mk_text_header(FirstHead, Head, Tail, Text, 
					     HTLen, FTLen, HTLen)
    end;
mk_text_header(FirstHead, Head, Tail, [C|R] = Text, 
                         HTLen, FTLen, Len) ->
    %% 1:st line
    %% can we fit another (encode) letter on this line?
    NewLen = Len + length(C),
    case NewLen > ?MAX_LENGTH of
	false -> C ++ 
		     mk_text_header(FirstHead, Head, Tail, R,
					      HTLen, FTLen, NewLen);
	%% C must be placed on new line
	true -> Tail ++ ?CONT ++ Head ++
		    mk_text_header(done, Head, Tail, Text,
					     HTLen, FTLen, HTLen)
    end.

%% Str = deep_string(), latin-1
%% return: [string()], each entry matches a letter in Str
q_encode_latin1(Str) ->
    F = fun(C) ->
		case C of
		    %% SP characters must be encoded as "_" (or "=20")
		    $\s -> "_";

		    %% '=', '?', and '_' are used as special control
		    %% characters, so these must always be qhex encoded
		    $= -> to_qhex(C);
		    $? -> to_qhex(C);
		    $_ -> to_qhex(C);

		    %% NOTE: this list may be unnecessarily restrictive

		    %% don't qhex-encode "standard us-ascii" letters
		    C when 
		    ((C >= $a) and (C =< $z)) or
		    ((C >= $A) and (C =< $Z)) or
		    ((C >= $0) and (C =< $9)) -> [C];

		    %% qhex-encode all other characters
		    _ -> to_qhex(C)
		end
	end,
    lists:map(F, lists:flatten(Str)).

%% return Q-encoded hex version of char C e.g. $= -> "=3D"
to_qhex(C) when C >= 0, C =< 255 ->
    First = C bsr 4,
    Last = C band 16#F,
    [$=, to_hex_char(First), to_hex_char(Last)].

to_hex_char(N) when N >=  0, N =<  9 -> N + $0;
to_hex_char(N) when N >= 10, N =< 15 -> N + $A - 10.


%% ------------------------------------------------------------------------
%% eunit test cases

mk_text_header_test_() ->
    [
     %% based on Thunderbird output
     ?_assertEqual("Subject: =?ISO-8859-1?Q?=E5=E4=F6?=",
                   mk_text_header("Subject", "åäö")),

     ?_assertEqual(
        "Subject: =?ISO-8859-1?Q?=E5=E4=F6twequiiiirrrweyqruyqitrrqw"
        "eruitwqeeerwqe?=\n"
        " =?ISO-8859-1?Q?urtwuietrriqweeeeeqeiu"
        "urrrrrrrweuiqtruiwetriweeeeyiirrrrr?=\n"
        " =?ISO-8859-1?Q?rrrrrrrruiweqtrweertwe"
        "uitr?=",
        mk_text_header(
          "Subject",
          "åäötwequiiiirrrweyqruyqitrrqw"
          "eruitwqeeerwqeurtwuietrriqweeeeeqeiuurrrrrrrweuiqtruiwetriwee"
          "eeyiirrrrrrrrrrrrruiweqtrweertweuitr")),
     
     %% based on RFC 1522
     %%   =  S?  ?  _  =  =  S?  S_
     ?_assertEqual("XXX: =?ISO-8859-1?Q?=3D_=3F=3F=5F=3D=3D_=3F_=5F?=",
                   mk_text_header("XXX", "= ??_== ? _")),
     
     ?_assertEqual("XXX: ",
                   mk_text_header("XXX", "")),
     
     %% 1 char on new line
     ?_assertEqual("Subject: =?ISO-8859-1?Q?=E5=E4=F6twequ"
                   "iiiirrrweyqruyqitrrqweruitwqeeerwqe?=\n"
                   " =?ISO-8859-1?Q?u?=",
                   mk_text_header(
                     "Subject",
                     "åäötwequiiiirrrweyqruyqitrrqweruitwqeeerwqeu")),
     
     %% fits on 1 line
     ?_assertEqual("Subject: =?ISO-8859-1?Q?=E5=E4=F6twequ"
                   "iiiirrrweyqruyqitrrqweruitwqeeerwqe?=",
                   mk_text_header(
                     "Subject",
                     "åäötwequiiiirrrweyqruyqitrrqweruitwqeeerwqe"))
    ].
