
/* args have already been type-checked */
embl_value add(embl_state *E, embl_type ty, embl_args args)
{
	embl_args_iter i = embl_args_iter_init(ty, args);

	/* these will panic if there is a type mismatch */
	int x = embl_args_next_int(i);
	int y = embl_args_next_int(i);

	return embl_value_from_int(x + y);
}



/* later on, in main() or something... */
embl_type add_type = embl_get_type("int -> int -> int");
embl_register(E, add, add_type);

