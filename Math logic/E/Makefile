all:
	javac -cp ./src src/ru/ifmo/rain/rozovsky/Main.java -d ./out

run:
	java -cp ./out ru.ifmo.rain.rozovsky.Main

clean:
	rm -rf ./out

run_test:
	(cat ./test/test1.txt) | (java -cp ./out ru.ifmo.rain.rozovsky.Main)
