MAIN=./app/Main.hs


all_w: main_w clean_w run_w

all: main

gen_w:
	alex -g ./res/Lexer.x -o ./src/Lexer.hs
	happy -gac ./res/Parser.y -o ./src/Parser.hs

main_w:
	stack ghc -- -O2 ./app/Main.hs ./src/*.hs -o ./out/main

main_l:
	sudo stack --allow-different-user ghc -- -O2 ./app/Main.hs ./src/*.hs -o ./main

main:
	ghc --make -dynamic  -O3 ./app/Main.hs ./src/*.hs -o ./main

run_w:
	.\out\main < .\test\test2.txt

run:
	./main

clean_w:
	del .\app\*.o  .\src\*.o
	del .\app\*.hi .\src\*.hi

clean:
	rm ./app/*.o  ./src/*.o
	rm ./app/*.hi ./src/*.hi
	rm ./main

zip:
	zip B.zip  ./app/Main.hs ./src/*.hs ./res/* ./Makefile
