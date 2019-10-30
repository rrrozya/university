package ru.ifmo.rain.rozovsky;

public class Parser {
    private String deleteWhitespace(String str) {
        StringBuilder new_str = new StringBuilder();
        for (char i : str.toCharArray()) {
            if (!Character.isWhitespace(i) && i != 13 && i != '\n') {
                new_str.append(i);
            }
        }
        return new_str.toString();
    }

    public Tree parse(String s) throws Exception {
        str = deleteWhitespace(s);
        if (str.isEmpty()) {
            throw new Exception("Empty string parse");
        }
        index = 0;

        return expr();
    }

    private enum List {
        IMPL, OR, AND, NOT, A, Q, PRED, func, prime, sPRED,
        plus, mul, zero, comma, doc, equal, bracket1, bracket2, var, END, num
    }

    private List id;
    private String str;
    private StringBuilder variable;

    private int index;


    private Tree expr() throws Exception {
        Tree left = disjunction();

        while (true) {
            if (id == List.IMPL) {
                left = new Tree(left, expr(), "->");
            } else {
                return left;
            }
        }
    }

    private Tree disjunction() throws Exception {
        Tree left = conjunction();

        while (true) {
            if (id == List.OR) {
                left = new Tree(left, conjunction(), "|");
            } else {
                return left;
            }
        }

    }

    private Tree conjunction() throws Exception {
        Tree left = unar();

        while (true) {
            if (id == List.AND) {
                left = new Tree(left, unar(), "&");
            } else {
                return left;
            }
        }
    }

    private Tree unar() throws Exception {
        getNext();
        Tree vertex;
        switch (id) {

            case NOT:
                vertex = new Tree(unar(), null, "!");
                break;

            case A: {
                Tree var = varr();
                char c = next();
                if (c != '.') throw new Exception("Point expected");
                vertex = new Tree(var, expr(), "@");
                vertex.freeVars.remove(vertex.left.sign);
                break;
            }
            case Q: {
                Tree var = varr();
                char c = next();
                if (c != '.') throw new Exception("Point expected");
                vertex = new Tree(var, expr(), "?");
                vertex.freeVars.remove(vertex.left.sign);
                break;
            }
            case bracket1: {
                int i = index;
                boolean isTerm = true;
                for (int balance = 1; balance != 0; ++i) {
                    if (str.charAt(i) == '(')
                        balance++;
                    if (str.charAt(i) == ')')
                        balance--;
                    if (str.charAt(i) == '-' && str.charAt(i+1) == '>' || str.charAt(i) == '&' || str.charAt(i) == '|' || str.charAt(i) == '@' ||
                            str.charAt(i) == '?' || str.charAt(i) == '!' || isBigLet(str.charAt(i)) || str.charAt(i) == '=')
                        isTerm = false;
                }

                if (!isTerm) {
                    vertex = expr();
                    if (id != List.bracket2) {
                        throw new Exception(index + ":" + str.charAt(index) + ") missing unar");
                    }
                    getNext();
                } else {
                    vertex = pred();
                }

                break;
            }
            case num://for axiom
            case sPRED:
                vertex = new Tree(null, null, variable.toString());
                getNext();
                break;
            default:
                vertex = pred();
                break;
        }
        return vertex;
    }

    private Tree varr() throws Exception {
        getNext();
        return new Tree(variable.toString(), false);
    }


    private Tree pred() throws Exception {
        Tree vertex = null;
        if (id == List.PRED) {
            vertex = new Tree(variable.toString(), false);
            vertex.terms.add(term());
            vertex.freeVars.addAll(
                    vertex.terms.get(vertex.terms.size() - 1).freeVars);
            while (id == List.comma) {
                vertex.terms.add(term());
                vertex.freeVars.addAll(
                        vertex.terms.get(vertex.terms.size() - 1).freeVars);
            }
            getNext();
        } else {
            getPrev();
            vertex = term();
            vertex = new Tree(vertex, term(), "=");
        }
        return vertex;
    }

    private Tree term() throws Exception {
        Tree left = sum();

        while (true) {
            if (id == List.plus) {
                left = new Tree(left, sum(), "+");
            } else {
                return left;
            }
        }
    }

    private Tree sum() throws Exception {
        Tree left = mult();

        while (true) {
            if (id == List.mul) {
                left = new Tree(left, mult(), "*");
            } else {
                return left;
            }
        }
    }

    private Tree mult() throws Exception {
        getNext();
        Tree vertex = null;
        switch (id) {
            case func:
                vertex = new Tree(variable.toString(), false);
                vertex.terms.add(term());
                vertex.freeVars.addAll(
                        vertex.terms.get(vertex.terms.size() - 1).freeVars);
                while (id == List.comma) {
                    vertex.terms.add(term());
                    vertex.freeVars.addAll(
                            vertex.terms.get(vertex.terms.size() - 1).freeVars);
                }
                break;
            case var:
                vertex = new Tree(variable.toString(), true);
                break;
            case bracket1:
                vertex = term();
                if (id != List.bracket2) {
                    throw new Exception(") missing");
                }
                break;
            case zero:
                vertex = new Tree("0", false);
                break;
            default:
                //System.err.println("In line: " + str);
                //System.err.println("mult " + str.charAt(index) + ":" + index);
                break;
        }
        getNext();
        while (id == List.prime) {
            vertex = new Tree(vertex, null, "'");
            getNext();
        }
        return vertex;
    }

    private boolean isBigLet(char ch) {
        return (ch >= 'A' && ch <= 'Z');
    }

    private void getNext() throws Exception {
        char ch = next();
        variable = new StringBuilder();
        if (Character.isAlphabetic(ch)) {
            if (!isBigLet(ch)) {
                while (Character.isDigit(ch) || Character.isAlphabetic(ch)) {
                    if (isBigLet(ch))
                        break;
                    variable.append(ch);
                    ch = next();
                }
                if (id != List.A && id != List.Q && ch == '(') {
                    id = List.func;
                } else {
                    id = List.var;
                    index--;
                }
            } else {
                while (Character.isDigit(ch) || Character.isAlphabetic(ch)) {
                    variable.append(ch);
                    ch = next();
                }
                if (ch == '(')
                    id = List.PRED;
                else {
                    --index;
                    id = List.sPRED;
                }
            }
        } else if (Character.isDigit(ch) && ch != '0') {
            variable.append(ch);
            id = List.num;
        } else {
            switch (ch) {
                case '!':
                    id = List.NOT;
                    break;
                case '&':
                    id = List.AND;
                    break;
                case '|':
                    id = List.OR;
                    break;
                case '-':
                    index++;
                    id = List.IMPL;
                    break;
                case '(':
                    id = List.bracket1;
                    break;
                case ')':
                    id = List.bracket2;
                    break;
                case '@':
                    id = List.A;
                    break;
                case '?':
                    id = List.Q;
                    break;
                case ',':
                    id = List.comma;
                    break;
                case '.':
                    id= List.doc;
                    break;
                case '=':
                    id = List.equal;
                    break;
                case '+':
                    id = List.plus;
                    break;
                case '*':
                    id = List.mul;
                    break;
                case '0':
                    id = List.zero;
                    break;
                case '\'':
                    id = List.prime;
                    break;
                case '#':
                    id = List.END;
                    break;
                default:
                    throw new Exception("incorrect character: '" + ch + "' at index: " + index + "\n");
            }
        }
    }

    private void getPrev() {
        if (id == List.func || id == List.var) {
            index -= variable.length();
        } else {
            index--;
        }
    }

    private char next() {
        if (index < str.length()) {
            return str.charAt(index++);
        } else {
            return '#';
        }
    }
}