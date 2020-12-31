#!/usr/bin/env python

class Num:
    def __init__(self, n):
        self.n = n
    def pretty(self):
        return str(self.n)
    def eval(self):
        return self.n

class Add:
    def __init__(self, a, b):
        self.a = a
        self.b = b
    def pretty(self):
        return "(" + self.a.pretty() + " + " + self.b.pretty() + ")"
    def eval(self):
        return self.a.eval() + self.b.eval()

class Mul:
    def __init__(self, a, b):
        self.a = a
        self.b = b
    def pretty(self):
        return "(" + self.a.pretty() + " * " + self.b.pretty() + ")"
    def eval(self):
        return self.a.eval() * self.b.eval()

#sam = Add(Num(100),Add(Num(20),Num(3)))
#print(sam.pretty())
#print(sam.eval())

def parse(s):
    max = len(s)
    pos = 0
    def get(who):
        nonlocal pos
        if pos < max:
            c = s[pos]
            pos = pos + 1
            #print("get("+who+"): s["+str(pos)+"] -> '"+c+"'")
            return c
        else:
            return None

    def put_back():
        nonlocal pos
        pos = pos - 1

    def digit():
        c = get("digit")
        if (c is None):
            return None
        if (c < '0' or c >'9'):
            put_back()
            return None
        return c;

    def numberAcc(acc):
        c = digit()
        if (c is None):
            return acc;
        else:
            return numberAcc(10*acc + int(c))

    def number():
        c = digit()
        if (c is None):
            return None
        return numberAcc(int(c))

    def err(msg):
        print(msg + " at " + str(pos) + ", got: '" + s[pos] + "'")

    def lit(x,who):
        c = get(who)
        if (c == x):
            return True
        else:
            put_back()
            return None

    def atom():
        if (lit('(',"open")):
            a = term()
            nibble()
            if (lit(')',"close")):
                return a
            else:
                err("atom() expected a matching close paren")
        else:
            n = number();
            if (n is None):
                err("atom() expected a number or bracketed expression")
            return Num(n)

    def nibble():
        if (lit(' ',"space") is None):
            return
        else:
            nibble()

    def op():
        if (lit('+',"plus") is None):
            if (lit('*',"star") is None):
                return None
            else:
                return Mul
        else:
            return Add

    def termAcc(a1):
        nibble()
        o = op()
        if (o is None):
            return a1
        nibble()
        a2 = atom()
        if (a2 is None):
             err("termAcc() expected an atom after op")
        return termAcc(o(a1,a2))

    def term():
        a1 = atom()
        return termAcc(a1)

    #print("s:"+s)
    #print("max="+str(max))
    a = term()
    #print("parse(): done, pos="+str(pos))
    return a


sam = open('input/day18.input.sam', 'r').read()
a = parse(sam)
#print(a.pretty())
print(a.eval())

inp = open('input/day18.input', 'r').read().splitlines()
res = sum([ parse(line).eval() for line in inp ])
assert (res == 75592527415659)
print ("day18,part1 : ", res)

