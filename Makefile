##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## Makefile
##

TARGET	:=	glados

all: $(TARGET)

$(TARGET):
	cabal build
	cp $(shell cabal list-bin glados) .

clean:
	cabal clean

fclean: clean
	$(RM) -rf dist-newstyle
	$(RM) $(TARGET)

re: fclean all

tests_run:
	cabal test

.PHONY: all clean fclean re
