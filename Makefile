##
## EPITECH PROJECT, 2018
## imageCompressor project func prog
## File description:
## Makefile
##

NAME	=	imageCompressor

SRC		+=	app/Main.hs				\
			src/Parser.hs			\
			src/Compressor.hs		\
			src/Print.hs			\
			src/Utils.hs

all:	$(NAME)

$(NAME): $(SRC)
	stack build --copy-bins --local-bin-path .

clean:
	stack clean
	rm .stack-work imageCompressor.cabal -rf

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY:	all clean fclean re