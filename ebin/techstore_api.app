{application, 'techstore_api', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['r_service','techstore','techstore_api_app','techstore_api_sup']},
	{registered, [techstore_api_sup]},
	{applications, [kernel,stdlib]},
	{optional_applications, []},
	{mod, {techstore_api_app, []}},
	{env, []}
]}.