require "./parser"

def self.tok(symbol : Symbol, t : T) : {Symbol, T} forall T
  {symbol, t}
end

gen1 = Parser::Analysis(Parser::LR0::Builder).new

gen1.add("program", "module")
gen1.add("module", :r_module, :string, "statements", :r_end)
gen1.add("statements")
gen1.add("statements", "statements", "statement")
gen1.add("statement", "function_definition")
gen1.add("statement", :r_return, "expression", :semi)
gen1.add("statement", "expression", :semi)
gen1.add("statement", "block")
gen1.add("block", "block_statements", :r_do, "statements", "block_finally", :r_end)
gen1.add("block_statements")
gen1.add("block_statements", "block_statements", "block_statement")
gen1.add("block_finally")
gen1.add("block_finally", :r_finally, "statement")
gen1.add("block_statement", :r_given, "expression")
gen1.add("block_statement", :r_repeat, "expression")
gen1.add("block_statement", :r_until, "expression")
gen1.add("expression", "decimal_literal")
gen1.add("expression", :word)
gen1.add("expression", "declaration")
gen1.add("expression", "assignment")
gen1.add("expression", "call")
gen1.add("decimal_literal", :number)
gen1.add("declaration", "definition_arg", :assign, "expression")
gen1.add("assignment", :word, :assign, "expression")
gen1.add("call", "call_name", :sq_l, "call_args", :sq_r)
gen1.add("call_name", :word)
gen1.add("call_name", :plus)
gen1.add("call_name", :eq)
gen1.add("call_args")
gen1.add("call_args", "expression")
gen1.add("call_args", "call_args", :pipe, "expression")
gen1.add("type_specifier", :tt_int)
gen1.add("function_definition", "type_specifier", :r_func, :word, :sq_l, "definition_args", :sq_r, "statements", :r_end)
gen1.add("definition_args")
gen1.add("definition_args", "definition_arg")
gen1.add("definition_args", "definition_args", :pipe, "definition_arg")
gen1.add("definition_arg", "type_specifier", :word)
states = gen1.build("program")
at = Parser::Automaton(String, Int32).new(states)

at << :r_module << tok(:string, "default")
  at << :tt_int << :r_func << tok(:word, "collatz") << :sq_l
    at << :tt_int << tok(:word, "num")
  at << :sq_r
    at << :r_given << :tt_int << tok(:word, "a") << :assign << tok(:number, 1) << :r_do << :r_end
  at << :r_end
at << :r_end
tree = at.run
pp tree
