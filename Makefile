##
## EPITECH PROJECT, 2019
## FUN_deBruijn_2018
## File description:
## Makefile
##

SOURCES	=	app/

ROOT	=	./

NAME	=	deBruijn

CC		= 	stack

all:	$(NAME)

$(NAME):
	@$(CC) build
	@cp `stack path --local-install-root`/bin/deBruijn-exe $(ROOT)$@

clean:
	$(CC) clean

fclean:	clean
	rm -f $(NAME)

re: fclean all

.PHONY: clean fclean re