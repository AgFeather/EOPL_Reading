#type defination
SYMOBL = str   # scheme symbol is implemented by python str
NUMBER = (int, float) # scheme number is implemented by python int and float
ATOM = (SYMOBL, NUMBER) # scheme atom is the basic conponent implemented by .
LIST = list             # scheme list is implemented by python list
EXP = (ATOM, LIST)      # shceme expression ::= ATOM|LIST
ENV = dict              # shceme environment is implemented by python dictionary
                        # which is mapping of (variable: value)


program_exp = "(define add (lambda (x) (+ x 1)))"

#first parsing input Scheme program (string) to a token list
def tokenize(programs):
    '''
    input: get an input string as a Scheme program
    output: return a token list of the input programs
    list elements is string
    '''
    programs = programs.replace('(', ' ( ').replace(')', ' ) ')
    tokens = programs.split()
    return tokens

#funciton test: tokenize
#print(tokenize(program_exp))

def parse(program):
    '''
    input: a string representation of a Scheme program
    step1: function:tokenize will turn the input to string token list
    step2: function: read_from_tokens will will return an AST represented by list
    output: an AST represented by list(number and symbol)
    '''
    def read_from_tokens(tokens):
        if len(tokens) == 0:
            raise SyntaxError('unexpected error')
        token = tokens.pop(0)
        if token == '(':
            L = []
            while tokens[0] != ')':
                L.append(read_from_tokens(tokens))
            tokens.pop(0) #pop off ')'
            return L
        elif token == ')':
            raise SysntaxError('unexpected error: ) ')
        else:
            return atom(token)
    def atom(token):
        '''
        atom function will transform a string represented atom
        to a number/string represented atom.
        '''
        try:
            return int(token)
        except ValueError:
            try:
                return float(token)
            except ValueError:
                return SYMOBL(token)

    return read_from_tokens(tokenize(program))

#function test: parse
#print(parse(program_exp))
# for a in aaa:
#     print(type(a))



#environment is a mapping(variable: value) for current function
import math
import operator as op

def standard_env():
    '''
    an environment with some Scheme standard procedures
    '''
    env = ENV()
    env.update(vars(math)) #sin, cos, sqrt, pi, ...
    env.update({
        '+':op.add, '-':op.sub, '*':op.mul, '/':op.truediv,
        '>':op.gt, '<':op.lt, '<=':op.ge, '>=':op.le, '=':op.eq,
        'abs': abs,
        'expt': pow,
        'map': map,
        'max': max,
        'min': min,

        'begin': lambda *x: x[-1],

        'list': lambda *x: LIST(x),
        'list?': lambda x: isinstance(x, LIST),

        'apply': lambda pro, args: pro(*args),
        'car': lambda x:x[0],
        'cdr': lambda x:x[1:],
        'cons': lambda x,y: [x].append(y),
        'append': op.add,
        'eq?': op.is_,
        'equal?': op.eq,
        'not': op.not_,
        'null?': lambda x:(x == []),
        'number?': lambda x: isinstance(x, NUMBER),
        'symbol?': lambda x: isinstance(x, SYMOBL),
        'print': print,
    #    'procedures?': callable,
    #    'round': round,
        'length': len,
    })
    return env

global_env = standard_env()


class Env(dict):
    def __init__(self, params=(), args=(), outer=None):
        self.update(zip(params, args))
        self.outer = outer
    def find(self, var):
        #find the value of input argument var
        return self if (var in self) else self.outer.find(var)

class Procedure(object):
    #a user-defined Scheme procedures(define lambda)
    def __init__(self, params, body, env):
        self.params, self.body, self.env = params, body, env
    def __call__(self, *args):#make this Procedure Object callable
        return eval(self.body, Env(self.params, args, self.env))



def eval(exp, env=global_env):
    '''
    evaluate an expression in an environment
    '''
    print(exp)
    if isinstance(exp, SYMOBL):  #exp is a variable
        return env[exp]
    elif isinstance(exp, NUMBER): #exp is a number(not list)
        return exp

    #here means the exp is a list, not a single element
    op, *args = exp
    if op == 'quote':
        return args[0]
    elif op == 'if':    #exp is a list and exp[0] is if
        (test, conseq, alt) = args
        temp = (conseq if eval(test, env) else alt)
        return eval(temp, env)
    elif op == 'define':#exp is a list and ...
        #print('define:', exp[0])
        (symbol, lambda_exp) = args
        env[symbol] = eval(lambda_exp, env)
    elif op == 'set!':
        (symbol, ex) = args
        env.find(symbol)[symbol] = eval(ex, env)
    elif op == 'lambda':
        (params, body) = args
        return Procedure(params=params,body=body,env=env)
    else: #any other basic function call
        proc = eval(op, env) #get variable/lambda from current env
        vals = [eval(arg, env) for arg in args]
        return proc(*vals)


def interpreter(exp):
    token_list = parse(exp)
    return eval(token_list)

# function test: eval
scheme_test = "(begin (define r 5) (* pi (* r r)))"
# scheme_test1 = "(beg)"
# ast = parse('(+ 1 2)')
# ast = parse("(define r 10)")
# print(ast)
# result = eval(ast)
result = interpreter(scheme_test)
print(result)
