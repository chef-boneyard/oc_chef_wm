%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Serdar Sutay <serdar@opscode.com>
%% @copyright 2011-2012 Opscode, Inc.
-module(oc_chef_wm_routes_core).

-export([bulk_route_fun/2,
         route_organization_rest_object/3]).

-include_lib("webmachine/include/webmachine.hrl").

%% @doc Generate a function that produces URLs.  Use this when multiple URLs of the same
%% type must be produced at once to prevent the needless recomputation of static URL
%% information.
%% @end
%% Create a function that needs just the Role name to generate a URL.
bulk_route_fun(Req, Template) ->
    {BaseURI, Org} = extract_from_req(Req),
    fun(Args) ->
            render_template(Template, BaseURI, [Org | Args])
    end.

%% @doc Extract various bits of information from a Webmachine request.
%%
%% Returns a tuple with the base URI and Chef Organization name.
-spec extract_from_req(Req :: #wm_reqdata{}) -> {nonempty_string(), binary()}.
extract_from_req(Req) ->
    Org = org_name(Req),
    BaseURI = chef_wm_util_core:base_uri(Req),
    {BaseURI, Org}.

route_organization_rest_object(ParentName, Req, Args) ->
    Org = org_name(Req),
    {name, Name} = lists:keyfind(name, 1, Args),
    Template = "/organizations/~s/~s/~s",
    TemplateArgs = [Org, ParentName, Name],
    render_template(Template, Req, TemplateArgs).

%% @doc utility method for generating a binary from a template and arguments.  The protocol
%% and host are derived from the Webmachine request via our own magic in `chef_wm_util',
%% since Webmachine doesn't do this for us.  As a result, the `template' should be for just
%% the path of the desired URL (including the leading "/" character!).  Thus, a "good"
%% template might be
%%
%%  "/organizations/~s/search/~s"
%%
render_template(Template, BaseURI, Args) when is_list(BaseURI) ->
    iolist_to_binary(BaseURI ++ io_lib:format(Template, Args));
render_template(Template, Req, Args) ->
    render_template(Template, chef_wm_util_core:base_uri(Req), Args).

%% @doc Extract the organization name from the Request's path.  Depends on us always using
%% the atom `organization_id' in our dispatch rules for the organization name.
org_name(Req) ->
    list_to_binary(wrq:path_info(organization_id, Req)).
