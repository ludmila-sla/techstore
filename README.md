é necessário estar com o mongo e mdbc rodando na maquina.
execute o modulo techstore, dentro da pasta src.
adicionando produto:
utilize o comando no shell erlang:
techstore:add_product("conexão", "id", "nome do produto", "descrição", "outros dados").

procurando produto:
techstore:find_product("conexão", "id").

listar produtos:
techstore:list_products("conexão").

deletar um produto:
techstore:delete_product("conexão", "id").
