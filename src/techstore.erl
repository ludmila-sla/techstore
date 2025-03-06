-module(techstore).
-behaviour(gen_server).
-export([start/0, add_product/2, find_product/2, init/1, list_products/1, delete_product/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(A, realm).

-define(RULES, #{
    rules => [
        #{key => <<"name">>, required => true},
        #{key => <<"price">>, required => true},
        #{key => <<"description">>, required => false, default => <<"">>}
    ]
}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #{}}.

add_product(Connection, ProductData) ->
    gen_server:cast(?MODULE, {add_product, Connection, ProductData}).

find_product(Connection, Name) ->
       Selector = #{<<"name">> => Name},
    case mdbcc:find_one(Connection, <<"products">>, Selector) of
        undefined -> {error, "Produto não encontrado"};
        Product -> {ok, Product}
    end.

list_products(Connection) ->
    Selector = #{},
    case mdbcc:find(Connection, <<"products">>, Selector) of
        {error, Reason} -> {error, Reason};
        Result -> Result
    end.

delete_product(Connection, Id) ->
        Selector = #{<<"_id">> => Id},
    case mdbcc:delete(Connection, <<"products">>, Selector) of
        ok -> {ok, "Produto deletado com sucesso"};
        {error, Reason} -> {error, Reason}
    end.

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

handle_cast({add_product, Connection, ProductData}, State) ->
    case validate_product(ProductData, ?RULES) of
        {ok, ValidatedProduct} ->
            Id = generate_id(),
            Product = ValidatedProduct#{<<"_id">> => Id},
            mdbcc:insert(Connection, <<"products">>, Product),
            {noreply, State};
        {error, Reason} ->
            io:format("Erro ao adicionar produto: ~p~n", [Reason]),
            {noreply, State}
    end;

handle_cast({delete_product, Connection, Id}, State) ->
    Selector = #{<<"_id">> => Id},
    mdbcc:delete(Connection, <<"products">>, Selector),
    {noreply, State};
handle_cast({new, Border, Agent, {_, _} = From}, S) ->
  gen_server:cast(self(), {new, Border, Agent, tech_settings:session_ttl(), From}),
  {noreply, S};
handle_cast(_Msg, State) ->
    io:format("Recebi uma mensagem inesperada: ~p~n", [_Msg]),
    {noreply, State}.



validate_product(ProductData, #{rules := Rules}) ->
    validate_product_rules(ProductData, Rules, #{}).
validate_product_rules(_, [], ValidatedProduct) ->
    {ok, ValidatedProduct};
validate_product_rules(ProductData, [Rule | Rest], ValidatedProduct) ->
    #{key := Key, required := Required} = Rule,
    case maps:get(Key, ProductData, undefined) of
        undefined when Required ->
            {error, <<"Campo obrigatório faltando: ", Key/binary>>};
        undefined ->
            Default = maps:get(default, Rule, <<"">>),
            validate_product_rules(ProductData, Rest, ValidatedProduct#{Key => Default});
                Value when Key =:= <<"price">> andalso not is_number(Value) ->
            {error, <<"O campo 'price' deve ser um número">>};
        Value when Key =:= <<"price">> andalso Value =< 0 ->
            {error, <<"O campo 'price' deve ser maior que zero">>};
        Value ->
            validate_product_rules(ProductData, Rest, ValidatedProduct#{Key => Value})
    end.


generate_id() ->
    Id = erlang:unique_integer([positive]),
    list_to_binary(integer_to_list(Id)).

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
