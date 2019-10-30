package test;

import ru.ifmo.rain.rozovsky.Parser;

public class Test {
    public static void main(String[] args) throws Exception {
        test1();
        test2();
        test3();
        test4();
    }

    private static void test1() throws Exception {
        System.out.println(new Parser().parse("@x.P(x)->A(x)").to_string());
    }

    private static void test2() throws Exception {
        System.out.println(new Parser().parse("@x.P(x)->A(x)->P(x)").to_string());
    }

    private static void test3() throws Exception {
        System.out.println(new Parser().parse("(@x.P(x)->A(x))->P(x)").to_string());
    }

    private static void test4() throws Exception {
        System.out.println(new Parser().parse("(0=0->0=0->0=0)->@x.Q(x)").to_string());
    }
}
