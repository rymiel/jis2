require "./parser"

def self.tok(symbol : Symbol, t : T) : {Symbol, T} forall T
  {symbol, t}
end

module JIS2
  alias AST = Parser::AST
  alias Ref = Parser::Ref
  alias Opt = Parser::Opt
  @[AST::EnumVal(
    integer: :tt_int
  )]
  @[AST::Aliased(Type)]
  enum PrimitiveType
    Integer
  end

  @[AST::EnumVal(
    given: :r_given,
    repeat: :r_repeat,
    until: :r_until
  )]
  enum BlockStatementKind
    Given
    Repeat
    Until
  end

  alias Type = PrimitiveType

  record TypeName, type : Type, name : String do
    include AST
    rule @type, :word >> @name
  end

  abstract class Statement
    include AST
  end
  abstract struct Expression
    include AST
  end
  abstract struct BlockStatement
    include AST
  end

  record ExpressionBlockStatement < BlockStatement, kind : BlockStatementKind, value : Expression do
    include AST
    rule @kind, @value
  end

  record Declaration < Expression, type : TypeName, value : Ref(Expression) do
    include AST
    rule @type, :assign, @value
  end

  record DecimalLiteral < Expression, value : Int64 do
    include AST
    rule :number >> @value
  end

  record Finally, statement : Statement do
    include AST
    rule :r_finally, @statement
  end

  class Block < Statement
    include AST
    getter b_statements : Array(BlockStatement)
    getter body : Array(Statement)
    getter finally : Finally?

    rule @b_statements, :r_do, @body, @finally, :r_end

    def initialize(@b_statements, @body, @finally)
    end
  end

  class FunctionDefinition < Statement
    include AST
    getter return_type : Type
    getter name : String
    @[Separator(:pipe, name: DefinitionArgs)]
    getter args : Array(TypeName)
    getter body : Array(Statement)

    rule @return_type, :r_func, :word >> @name, :sq_l, @args, :sq_r, @body, :r_end

    def initialize(@return_type, @name, @args, @body)
    end
  end

  @[AST::Aliased(Program)]
  class Module
    include AST
    getter name : String
    getter body : Array(Statement)

    rule :r_module, :string >> @name, @body, :r_end

    def initialize(@name, @body)
    end
  end
end

parser = Parser::Analysis(Parser::LR1::Builder).new
JIS2::AST.compose_known_rules parser
states = parser.build("program")
at = Parser::Automaton.new(states)

at << :r_module << tok(:string, "default")
  at << :tt_int << :r_func << tok(:word, "collatz") << :sq_l
    at << :tt_int << tok(:word, "num")
  at << :sq_r
    at << :r_given << :tt_int << tok(:word, "a") << :assign << tok(:number, 1i64) << :r_do << :r_end
  at << :r_end
at << :r_end
tree = at.run
PrettyPrint.format(tree.value(JIS2::Module), STDOUT, 119)
puts
