##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## Makefile
##
 
TARGET		:=	glados

all: $(TARGET)

$(TARGET):
	stack build
	cp $(shell stack path --local-install-root)/bin/$(TARGET) .
 
clean:
	$(RM) -rf .stack-work

fclean:
	$(RM) $(TARGET)
 
re: fclean all

run_tests:
	stack test

.PHONY: all clean fclean re
