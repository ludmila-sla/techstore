-module(techstore).
-behaviour(gen_server).
-export([start/0, add_product/5, find_product/2, init/1, list_products/1, delete_product/2]).
-export([get_env/2]).
-export([mdbcc/0]).
-export([set_env/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(A, realm).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #{}}.

add_product(Connection, Id, Name, Price, Description) ->
    Product = #{
        <<"_id">> => Id, <<"name">> => Name, <<"price">> => Price, <<"description">> => Description
    },
    handle_cast({add_product, Connection, Product}, []).

find_product(Connection, Id) ->
    Selector = #{<<"_id">> => Id},
    case mdbcc:find_one(Connection, <<"products">>, Selector) of
        undefined -> {error, "Produto não encontrado"};
        Product -> {ok, Product}
    end.

list_products(Connection) ->
    Selector = #{},
    mdbcc:find(Connection, <<"products">>, Selector).

delete_product(Connection, Id) ->
    Selector = #{<<"_id">> => Id},
    mdbcc:delete(Connection, <<"products">>, Selector).

get_env(K, D) ->
    application:get_env(?A, K, D).

mdbcc() ->
    get_env(mdbcc, <<"mongodb://localhost:27018/techstore">>).

set_env(K, V) ->
    application:set_env(?A, K, V).

handle_call({find_product, Connection, Id}, _From, State) ->
    Selector = #{<<"_id">> => Id},
    case mdbcc:find_one(Connection, <<"products">>, Selector) of
        undefined -> {reply, {error, "Produto não encontrado"}, State};
        Product -> {reply, {ok, Product}, State}
    end;
handle_call({list_products, Connection}, _From, State) ->
    Selector = #{},
    Result = mdbcc:find(Connection, <<"products">>, Selector),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_product, Connection, Product}, State) ->
    io:format("Tentando inserir: ~p~n", [Product]),

    mdbcc:insert(Connection, <<"products">>, Product),
    timer:sleep(500),
    case mdbcc:find_one(Connection, <<"products">>, #{<<"_id">> => maps:get(<<"_id">>, Product)}) of
        undefined ->
            io:format("Falha na inserção do produto.~n"),
            {noreply, State};
        _InsertedProduct ->
            io:format("Produto inserido com sucesso!~n"),
            {noreply, State}
    end;
handle_cast({delete_product, Connection, Id}, State) ->
    Selector = #{<<"_id">> => Id},
    mdbcc:delete(Connection, <<"products">>, Selector),
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("Recebi uma mensagem inesperada: ~p~n", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
