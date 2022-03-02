all:
	flex lex.l
	bison -d -t parser.y
	gcc lex.yy.c parser.tab.c -o out
	./out