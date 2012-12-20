%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc Helper functions that tie together operations across chef_db and chef_index
-module(oc_chef_object_db).

-export([delete/3]).

-include_lib("chef_objects/include/chef_types.hrl").

%% @doc Deletes an object from the database, removes the object's authz record (if needed),
%% and queues a delete of the object's data in the search index (Solr). Throws an error if
%% the database delete operation fails. Crashing on db error and not performing the authz
%% and solr deletes is the right thing because: if the data is still in the db, then the
%% entire operation can be retried. The object will still be searchable and accessible; if
%% the data was actually deleted despite the error return, then the orphaned authz and solr
%% data are of little consequence, data returned from searches is pulled out of the db so it
%% will be as if the data was correctly deleted. If we delted the authz and solr data when a
%% db error was encountered, we could have data in the db that could not be accessed nor be
%% findable via search.
-spec delete( chef_db:db_context(),
    chef_object() | #chef_cookbook_version{checksums::'undefined' | [binary()]},
    object_id() ) -> ok.

delete(DbContext,#chef_data_bag{org_id = OrgId,
                                name = DataBagName}=DataBag,
                                RequestorId) ->
    %% This is a special case, because of the hierarchical relationship between Data Bag
    %% Items and Data Bags.  We need to get the ids of all the data bag's items so that we
    %% can remove them from Solr as well; a cascade-on-delete foreign key takes care of the
    %% data bag item deletion from the database
    DataBagItemIds = case chef_db:fetch_data_bag_item_ids(DbContext, {id, OrgId}, DataBagName) of
                         Items when is_list(Items)->
                             Items;
                         _Other ->
                             []
                     end,

    %% Remove data bag from database; no need to remove from Solr, since they're not indexed
    %% anyway (what's there to index, after all?)
    delete_from_db(DbContext, RequestorId, DataBag), % throws on error
    %% Remove data bag items from Solr now; directly calling chef_index_queue:delete since
    %% we've just got ids, and not proper data bag item records required for
    %% chef_object_db:delete_from_solr
    [ chef_index_queue:delete(data_bag_item, Id, OrgId) || Id <- DataBagItemIds ],
    ok;
delete(DbContext, Object, RequestorId) ->
    %% All other object deletion is relatively sane :)
    %% Note that this will throw if an error is encountered
    delete_from_db(DbContext, RequestorId, Object),
    %% This is fire and forget as well. If we're here, we've already deleted the db record
    %% and won't be able to get back here for a retry.
    chef_object_db:delete_from_solr(Object),
    ok.

-spec delete_from_db(chef_db:db_context(),
                     RequestorId :: object_id(),
                     chef_object() | #chef_cookbook_version{}) -> ok |
                                                                  not_found.
%% @doc Delete an object from the database.  Provides pattern-matching sugar over chef_db
%% delete functions, making the `delete` function in this module very simple. Also deletes
%% the corresponding authz object (if there is one and it should be deleted). That is, it
%% does nothing extra for data_bag_items, it deletes the authz id for the cookbook if the
%% last cookbook_version of a given name is being deleted, and otherwise deletes the authz
%% object as desired. If an error is returned by the `chef_db:delete_*` call, this function
%% throws a `{delete_from_db, {error, Why}}' tuple.
delete_from_db(DbContext, RequestorId, #chef_client{}=Client) ->
    maybe_delete_authz_id_or_error(chef_db:delete_client(DbContext, Client),
                                   Client, RequestorId);
delete_from_db(DbContext, RequestorId, #chef_node{}=Node) ->
    maybe_delete_authz_id_or_error(chef_db:delete_node(DbContext, Node), Node, RequestorId);
delete_from_db(DbContext, RequestorId, #chef_role{}=Role) ->
    maybe_delete_authz_id_or_error(chef_db:delete_role(DbContext, Role), Role, RequestorId);
delete_from_db(DbContext, RequestorId, #chef_environment{}=Environment) ->
    maybe_delete_authz_id_or_error(chef_db:delete_environment(DbContext, Environment),
                                   Environment, RequestorId);
delete_from_db(DbContext, RequestorId, #chef_data_bag{}=DataBag) ->
    maybe_delete_authz_id_or_error(chef_db:delete_data_bag(DbContext, DataBag),
                                   DataBag, RequestorId);
delete_from_db(DbContext, RequestorId, #chef_data_bag_item{}=DataBagItem) ->
    maybe_delete_authz_id_or_error(chef_db:delete_data_bag_item(DbContext, DataBagItem),
                                   DataBagItem, RequestorId);
delete_from_db(DbContext, RequestorId, #chef_cookbook_version{}=CBVersion) ->
    maybe_delete_authz_id_or_error(chef_db:delete_cookbook_version(DbContext, CBVersion),
                                   CBVersion, RequestorId).

-spec maybe_delete_authz_id_or_error(Status, Object, RequestorId) -> ok | not_found when
      Status :: {ok, 1 | 2} | not_found | {error, _},
      Object :: chef_object() | #chef_cookbook_version{},
      RequestorId :: object_id().
%% Some Chef objects do not have an authz id (data_bag_items) and some (cookbook_versions)
%% share an authz id (cookbook). This code determins when the parent authz id should be
%% deleted. For data_bag_items, never as data_bags are deleted directly by the API. For
%% cookbook_version objects, we delete the associated cookbook authz id when the the
%% cookbook_version is the last of the named cookbook.
maybe_delete_authz_id_or_error(Error = {error, _}, _Object, _RequestorId) ->
    %% db error, no further action, but throw!
    throw({delete_from_db, Error});
maybe_delete_authz_id_or_error(not_found, _Object, _RequestorId) ->
    %% if the object wasn't found, we take no further action
    not_found;
maybe_delete_authz_id_or_error({ok, 1}, #chef_data_bag_item{}, _RequestorId) ->
    %% no authz_id for data_bag_items so just return
    ok;
maybe_delete_authz_id_or_error({ok, 1}, #chef_cookbook_version{}, _RequestorId) ->
    %% With status {ok, 1} we've deleted _only_ the cbv, not the cb so we leave the authz_id
    ok;
maybe_delete_authz_id_or_error({ok, 2}, #chef_cookbook_version{} = CBV, RequestorId) ->
    %% With status {ok, 2} we've deleted the cbv _and_ the cb so we delete the authz_id
    oc_chef_authz:delete_resource(RequestorId, object, authz_id(CBV)),
    ok;
maybe_delete_authz_id_or_error({ok, 1}, Object, RequestorId) ->
    %% for all other object types, successful delete of 1 record means we should delete authz id
    oc_chef_authz:delete_resource(RequestorId, object, authz_id(Object)),
    ok.

-spec authz_id(chef_object() | #chef_cookbook_version{} | #chef_client{}  ) -> object_id().

authz_id(#chef_client{authz_id=AuthzId}) -> AuthzId;
authz_id(#chef_node{authz_id=AuthzId}) -> AuthzId;
authz_id(#chef_role{authz_id=AuthzId}) -> AuthzId;
authz_id(#chef_environment{authz_id=AuthzId}) -> AuthzId;
authz_id(#chef_data_bag{authz_id=AuthzId}) -> AuthzId;
authz_id(#chef_cookbook_version{authz_id=AuthzId}) -> AuthzId.
