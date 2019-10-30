package ru.ifmo.rain.rozovsky;

import java.util.*;

public class Main {
    private static Scanner in= new Scanner(System.in);

    private static String deleteWhitespace(String str) {
        StringBuilder new_str = new StringBuilder();
        for (char i : str.toCharArray()) {
            if (!Character.isWhitespace(i) && i != 13 && i != '\n') {
                new_str.append(i);
            }
        }
        return new_str.toString();
    }


    private static Parser parser = new Parser();

    private static List<Tree> logic_axioms = new ArrayList<>();
    private static List<Tree> fa_axioms = new ArrayList<>();
    private static List<Tree> supp = new ArrayList<>();
    private static Set<String> free_vars = new HashSet<>();
    private static List<Tree> proved = new ArrayList<>();
    private static Map<String, String> axiomVer = new HashMap<>();
    private static Map<String, Tree> vars = new HashMap<>();

    private static List<Tree> parse_header(String header) throws Exception {

        List<Tree> res = new ArrayList<>();
        int balance = 0;
        StringBuilder tmp = new StringBuilder();
        for (int i = 0; i < header.length(); ++i) {
            if (header.charAt(i) == ',' && balance == 0 || header.charAt(i) == '|' && header.charAt(i + 1) == '-') {
                if (tmp.length() > 0) {
                    res.add(parser.parse(tmp.toString()));
                }
                if (header.charAt(i) == '|' && header.charAt(i + 1) == '-') {
                    ++i;
                }
                tmp = new StringBuilder();
            } else {
                if (header.charAt(i) == '(') {
                    ++balance;
                }
                if (header.charAt(i) == ')') {
                    --balance;
                }
                tmp.append(header.charAt(i));
            }
        }
        res.add(parser.parse(tmp.toString()));
        return res;
    }

    private static void init_axioms() throws Exception {
        logic_axioms.add(parser.parse("1->2->1"));
        logic_axioms.add(parser.parse("(1->2)->(1->2->3)->(1->3)"));
        logic_axioms.add(parser.parse("1->2->(1&2)"));
        logic_axioms.add(parser.parse("1&2->1"));
        logic_axioms.add(parser.parse("1&2->2"));
        logic_axioms.add(parser.parse("1->(1|2)"));
        logic_axioms.add(parser.parse("2->(1|2)"));
        logic_axioms.add(parser.parse("(1->3)->(2->3)->((1|2)->3)"));
        logic_axioms.add(parser.parse("(1->2)->(1->!2)->!1"));
        logic_axioms.add(parser.parse("!!1->1"));

        fa_axioms.add(parser.parse("a=b->a'=b'"));
        fa_axioms.add(parser.parse("a=b->a=c->b=c"));
        fa_axioms.add(parser.parse("a'=b'->a=b"));
        fa_axioms.add(parser.parse("!a'=0"));
        fa_axioms.add(parser.parse("a+b'=(a+b)'"));
        fa_axioms.add(parser.parse("a+0=a"));
        fa_axioms.add(parser.parse("a*0=0"));
        fa_axioms.add(parser.parse("a*b'=a*b+a"));
    }

    private static boolean checkAxiom(Tree vertex, Tree axiom) {
        if (axiom == null && vertex == null) {
            return true;
        } else if (axiom == null || vertex == null) {
            return false;
        }

        if (Character.isDigit(axiom.sign.charAt(0))) {
            if (axiomVer.containsKey(axiom.sign)) {
                return axiomVer.get(axiom.sign).equals(vertex.to_string());
            } else {
                axiomVer.put(axiom.sign, vertex.to_string());
                return true;
            }
        }

        if (axiom.sign.equals(vertex.sign)) {
            return checkAxiom(vertex.left, axiom.left) &&
                    checkAxiom(vertex.right, axiom.right);
        }

        return false;
    }

    private static boolean isAxiom(Tree vertex) {
        for (Tree logic_axiom : logic_axioms) {
            axiomVer.clear();
            if (checkAxiom(vertex, logic_axiom)) {
                return true;
            }
        }

        for (Tree fa_axiom : fa_axioms) {
            if (fa_axiom.to_string().equals(vertex.to_string()))
                return true;
        }
        return false;
    }

    private static Tree substitute(Tree expr, String var, Tree theta) {
        if (expr == null) return null;
        if (expr.terms.isEmpty() && expr.sign.equals(var)) {
            return theta;
        }
        return new Tree(substitute(expr.left, var, theta), substitute(expr.right, var, theta), expr.sign);
    }

    private static boolean check_on_substitution(Tree axiom, Tree expr) {
        if (expr == null) return false;
        if (axiom.sign.equals("->") || axiom.sign.equals("|") || axiom.sign.equals("&")
                || axiom.sign.equals("=") || axiom.sign.equals("+") || axiom.sign.equals("*")
                || axiom.sign.equals("@") || axiom.sign.equals("?")) {
            if (!axiom.sign.equals(expr.sign)) {
                return false;
            }
            return check_on_substitution(axiom.left, expr.left)
                    && check_on_substitution(axiom.right, expr.right);
        }
        if (axiom.sign.equals("!") || axiom.sign.equals("\'")) {
            if (!axiom.sign.equals(expr.sign)) {
                return false;
            }
            return check_on_substitution(axiom.left, expr.left);
        }
        if (!axiom.terms.isEmpty()) {
            if (!axiom.sign.equals(expr.sign) || axiom.terms.size() != expr.terms.size()) {
                return false;
            }
            for (int i = 0; i < axiom.terms.size(); ++i) {
                if (!check_on_substitution(axiom.terms.get(i), expr.terms.get(i))) {
                    return false;
                }
            }
            return true;
        }
        if (!vars.containsKey(axiom.sign)) {
            vars.put(axiom.sign, expr);
        }
        return Tree.equals(vars.get(axiom.sign), expr);
    }

    // ψ[x := 0] & ∀x((ψ) → (ψ)[x := x']) → ψ
    private static boolean check_induction(Tree expr) {
        if (expr.sign.equals("->") &&
                expr.left.sign.equals("&") &&
                expr.left.right.sign.equals("@") &&
                expr.left.right.right.sign.equals("->")) {
            Tree r = expr.right;
            String x = expr.left.right.left.sign;

            Tree tmp = new Tree(
                    new Tree(
                            substitute(r, x, new Tree("0", false)),
                            new Tree(
                                    new Tree(x, true),
                                    new Tree(r,
                                            substitute(r, x, new Tree(new Tree(x, true), null, "\'")),
                                            "->"),
                                    "@"),
                            "&"),
                    r,
                    "->");
            return Tree.equals(expr, tmp);
        }
        return false;
    }

    // axiom 11 ∀x(ψ)->(ψ[x := θ])
    private static boolean check_11(Tree expr) {
        if (expr.sign.equals("->") && expr.left.sign.equals("@")) {
            String x = expr.left.left.sign;
            vars.clear();
            Tree l = expr.left;
            Tree r = expr.right;

            if (!check_on_substitution(l.right, r)) {
                return false;
            }

            if (r.get_vars().contains(x) && !vars.get(x).get_vars().contains(x)) {
                return false;
            }

            Set<String> fi_variables = l.right.get_vars();

            Map<String, Tree> vv = new HashMap<>();
            for (Map.Entry<String, Tree> qq : vars.entrySet()) {
                if (!qq.getKey().equals(qq.getValue().to_string()) || qq.getKey().equals(x)) {
                    vv.put(qq.getKey(), qq.getValue());
                }
            }

            if (vv.isEmpty() && !fi_variables.contains(x))
                return true;

            int tmp = 0;
            if (fi_variables.contains(x)) {
                tmp = 1;
            }
            if (vv.size() == tmp && vv.containsKey(x)) {
                Tree theta = vv.get(x);
                Set<String> fi_free_variables = new HashSet<>(r.freeVars);
                for (String s : theta.freeVars) {
                    if (!fi_free_variables.contains(s)) {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }

    // axiom 12 (ψ[x := θ]) → ∃x(ψ)
    private static boolean check_12(Tree expr) {
        if (expr.sign.equals("->") && expr.right.sign.equals("?")) {
            String x = expr.right.left.sign;
            Tree r = expr.right;
            Tree l = expr.left;
            vars.clear();

            if (!check_on_substitution(r.right, l)) {
                return false;
            }
            if (l.get_vars().contains(x) && !vars.get(x).get_vars().contains(x)) {
                return false;
            }
            Map<String, Tree> vv = new HashMap<>();
            Set<String> fi_variables = r.right.get_vars();

            for (Map.Entry<String, Tree> qq : vars.entrySet()) {
                if (!qq.getKey().equals(qq.getValue().to_string()) || qq.getKey().equals(x)) {
                    vv.put(qq.getKey(), qq.getValue());
                }
            }

            if (vv.isEmpty() && !fi_variables.contains(x)) {
                return true;
            }
            int tmp = 0;
            if (fi_variables.contains(x)) {
                tmp = 1;
            }
            if (vv.size() == tmp && vv.containsKey(x)) {
                Tree theta = vv.get(x);
                Set<String> fi_free_variables = new HashSet<>(l.freeVars);
                for (String s : theta.freeVars) {
                    if (!fi_free_variables.contains(s)) {
                        return false;
                    }
                }
                return true;
            }

        }
        return false;
    }

    // rule for (φ) → ∀x(ψ)
    private static boolean rule1(Tree expr) {
        if (expr.sign.equals("->") && expr.right.sign.equals("@")) {
            String x = expr.right.left.to_string();
            if (expr.left.freeVars.contains(x)) {
                return false;
            }
            for (int i = (int) proved.size() - 1; i >= 0; --i) {
                if (!proved.get(i).sign.equals("->"))
                    continue;
                if (proved.get(i).left.to_string().equals(expr.left.to_string()) &&
                        proved.get(i).right.to_string().equals(expr.right.right.to_string())) {
                    return true;
                }
            }
        }
        return false;
    }

    // rule for ∃x(ψ) → (φ)
    private static boolean rule2(Tree expr) {

        if (expr.sign.equals("->") && expr.left.sign.equals("?")) {
            String x = expr.left.left.to_string();
            if (expr.right.freeVars.contains(x)) { // x is free in φ
                return false;
            }

            for (int i = (int) proved.size() - 1; i >= 0; --i) {
                if (!proved.get(i).sign.equals("->"))
                    continue;
                if (proved.get(i).left.to_string().equals(expr.left.right.to_string()) &&
                        proved.get(i).right.to_string().equals(expr.right.to_string()))
                    return true;
            }
        }
        return false;

    }

    private static boolean check(Tree expr) {
        for (Tree pr : supp) {
            if (pr.to_string().equals(expr.to_string())) {
                return true;
            }
        }
        if (isAxiom(expr) || check_induction(expr) || check_11(expr) || check_12(expr))
            return true;

        boolean MP = false;
        int counter = (int) proved.size() - 1;
        for (int i = counter; i >= 0 && !MP; --i) {
            if (proved.get(i).right != null && proved.get(i).sign.equals("->") && Tree.equals(proved.get(i).right, expr)) {
                Tree leftTree = proved.get(i).left;
                for (int z = counter - 1; z >= 0; --z) {
                    if (Tree.equals(proved.get(z), leftTree)) {
                        MP = true;
                        break;
                    }
                }
            }
        }
        if (MP)
            return true;

        return rule1(expr) || rule2(expr);
    }

    private static void task() throws Exception {
        init_axioms();
        String s = in.nextLine();
        supp = parse_header(deleteWhitespace(s));
        Tree x = supp.get(supp.size() - 1);
        supp.remove(supp.size() - 1);
        for (Tree expr : supp) {
            free_vars.addAll(expr.freeVars);
        }
        int line = 0;
        boolean lastIsTarget = false;
        while (in.hasNextLine()) {
            s = in.nextLine();
            s = deleteWhitespace(s);
            if (s.isEmpty()) continue;
            ++line;
            Tree cur = parser.parse(s);
            proved.add(cur);
            if (!check(cur)) {
                System.out.println("Line #" + line + " can’t be obtained");
                return;
            }
            lastIsTarget = x.to_string().equals(cur.to_string());
        }
        if (lastIsTarget) {
            System.out.println("Proof is correct.");
        } else {
            System.out.println("Required hasn’t been proven");
        }
    }

    public static void main(String[] args) throws Exception {
        task();
    }
}
