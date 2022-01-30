require "./parser"

def self.tok(symbol : Symbol, t : T) : {Symbol, T} forall T
  {symbol, t}
end

gen1 = Parser::Analysis(Parser::LR0::Builder).build do
  add("program", "module")
  add("module", :r_module, :string, "statements", :r_end)
  add("statements") {
    empty
    line("statements", "statement")
  }
  add("statement") {
    line("function_definition")
    line(:r_return, "expression", :semi)
    line("expression", :semi)
    line("block")
  }
  add("block", "block_statements", :r_do, "statements", "block_finally", :r_end)
  add("block_statements") {
    empty
    line("block_statements", "block_statement")
  }
  add("block_finally") {
    empty
    line(:r_finally, "statement")
  }
  add("block_statement") {
    line(:r_given, "expression")
    line(:r_repeat, "expression")
    line(:r_until, "expression")
  }
  add("expression") {
    line("decimal_literal")
    line(:word)
    line("declaration")
    line("assignment")
    line("call")
  }
  add("decimal_literal", :number)
  add("declaration", "definition_arg", :assign, "expression")
  add("assignment", :word, :assign, "expression")
  add("call", "call_name", :sq_l, "call_args", :sq_r)
  add("call_name") {
    line(:word)
    line(:plus)
    line(:eq)
  }
  add("call_args") {
    empty
    line("expression")
    line("call_args", :pipe, "expression")
  }
  add("type_specifier", :tt_int)
  add("function_definition", "type_specifier", :r_func, :word, :sq_l, "definition_args", :sq_r, "statements", :r_end)
  add("definition_args") {
    empty
    line("definition_arg")
    line("definition_args", :pipe, "definition_arg")
  }
  add("definition_arg", "type_specifier", :word)
end
states = gen1.build("program")
at = Parser::Automaton.new(states)

at << :r_module << tok(:string, "default")
  at << :tt_int << :r_func << tok(:word, "collatz") << :sq_l
    at << :tt_int << tok(:word, "num")
  at << :sq_r
    at << :r_given << :tt_int << tok(:word, "a") << :assign << tok(:number, 1) << :r_do << :r_end
  at << :r_end
at << :r_end
tree = at.run
pp tree
