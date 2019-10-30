package ru.ifmo.rain.rozovsky;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@SuppressWarnings("WeakerAccess")
public class Tree {
    public String sign;
    public Tree left = null;
    public Tree right = null;
    public List<Tree> terms = new ArrayList<>();
    public Set<String> freeVars = new HashSet<>();

    public Tree(Tree left, Tree right, String sign) {
        this.left = left;
        this.right = right;
        this.sign = sign;
        if (left != null) {
            this.freeVars.addAll(left.freeVars);
        }
        if (right != null) {
            this.freeVars.addAll(right.freeVars);
        }
    }

    public Tree(String sign, boolean isVar) {
        this.sign = sign;
        if (isVar)
            this.freeVars.add(sign);
    }

    public String to_string() {
        if (sign.equals("->") || sign.equals("|") || sign.equals("&") || sign.equals("=") || sign.equals("+") || sign.equals("*")) {
            return "(" + left.to_string() + sign + right.to_string() + ")";
        } else if (sign.equals("!")) {
            return sign + left.to_string();
        } else if (sign.equals("\'")) {
            return left.to_string() + "\'";
        } else if (sign.equals("?") || sign.equals("@")) {
            return sign + left.to_string() + "(" + right.to_string() + ")";
        } else if (!terms.isEmpty()) {
            StringBuilder res = new StringBuilder(sign + "(");
            boolean f = false;
            for (Tree arg : terms) {
                if (f) res.append(",");
                f = true;
                res.append(arg.to_string());
            }
            return res + ")";
        } else {
            return sign;
        }
    }

    public static boolean equals(Tree first, Tree second) {
        if (first == null && second == null) {
            return true;
        }
        if (first == null || second == null) {
            return false;
        }

        if (!first.sign.equals(second.sign))
            return false;
        if (first.terms.size() != second.terms.size())
            return false;
        for (int i = 0; i < first.terms.size(); ++i) {
            if (!first.terms.get(i).to_string().equals(second.terms.get(i).to_string())) {
                return false;
            }
        }
        return equals(first.left, second.left) && equals(first.right, second.right);
    }

    public Set<String> get_vars() {
        Set<String> res = new HashSet<>();
        if (Character.isLowerCase(sign.charAt(0)) && terms.isEmpty()) {
            res.add(sign);
        }
        if (left != null) {
            res.addAll(left.get_vars());
        }
        if (right != null) {
            res.addAll(right.get_vars());
        }
        for (Tree arg : terms) {
            res.addAll(arg.get_vars());
        }
        return res;
    }
}
