##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## Makefile
##

COMPILER	:=	glados-compiler
VM			:=	glados-vm

all: $(COMPILER) $(VM)

$(COMPILER):
	@cabal build $(COMPILER)
	@cp $(shell cabal list-bin $(COMPILER)) .

$(VM):
	@cabal build $(VM)
	@cp $(shell cabal list-bin $(VM)) .


clean:
	@cabal clean

fclean: clean
	@$(RM) $(COMPILER) $(VM)

re: fclean all

tests_run:
	@cabal test --enable-coverage

doc:
	@cabal haddock all | grep "Documentation created" -A1

.PHONY: all clean fclean re $(COMPILER) $(VM)
