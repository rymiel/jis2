require "./parser"
require "./lexer"

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
    given: :given,
    repeat: :repeat,
    until: :until
  )]
  enum BlockStatementKind
    Given
    Repeat
    Until
  end

  @[AST::EnumVal(
    equal: :"="
  )]
  enum Operator
    Equal
  end

  alias Type = PrimitiveType

  record TypeName, type : Type, name : String do
    include AST
    rule @type, :_word >> @name

    def inspect(io : IO)
      io << "JIS2::TypeName("
      @type.to_s io
      io << " "
      @name.to_s io
      io << ")"
    end
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
    rule @type, :":=", @value
  end

  record Assignment < Expression, name : String, value : Ref(Expression) do
    include AST
    rule :_word >> @name, :":=", @value
  end

  record DecimalLiteral < Expression, value : Int64 do
    include AST
    rule :_number >> @value
  end

  record VariableLiteral < Expression, name : String do
    include AST
    rule :_word >> @name
  end

  class ExpressionStatement < Statement
    include AST
    getter expression : Expression

    rule @expression, :";"

    def initialize(@expression)
    end
  end

  struct Call < Expression
    include AST
    getter name : String
    @[Separator(:"|", name: CallArgs)]
    getter args : Array(Expression)

    rule :_word >> @name, :"[", @args, :"]"
    rule ::JIS2::Operator =~ @name, :"[", @args, :"]"

    def initialize(@name, @args)
    end
    def initialize(name operator : Operator, @args)
      @name = case operator
      in .equal? then "="
      end
    end
  end

  record Finally, statement : Statement do
    include AST
    rule :finally, @statement
  end

  class Block < Statement
    include AST
    getter b_statements : Array(BlockStatement)
    getter body : Array(Statement)
    getter finally : Finally?

    rule @b_statements, :do, @body, @finally, :end

    def initialize(@b_statements, @body, @finally)
    end
  end

  class FunctionDefinition < Statement
    include AST
    getter return_type : Type
    getter name : String
    @[Separator(:"|", name: DefinitionArgs)]
    getter args : Array(TypeName)
    getter body : Array(Statement)

    rule :func, :_word >> @name, :"[", @args, :"]", @return_type, @body, :end

    def initialize(@return_type, @name, @args, @body)
    end
  end

  @[AST::Aliased(Program)]
  class Module
    include AST
    getter name : String
    getter body : Array(Statement)

    rule :module, :_string >> @name, @body, :end

    def initialize(@name, @body)
    end
  end
end

lexer = Lexer(String, Int64).build do
  match :module, :func, :do, :end, :given, :until, :repeat, :finally
  match /∇/, :tt_int
  match :"[", :"]", :":=", :"=", :"|", :";"
  match /"([^"]*)"/, :_string, &.[1]
  match /!(\d+)/, :_number, &.[1].to_i64
  match /[a-z]+/, :_word, &.[0]
  skip /\s+/
end

parser = Parser::Analysis(Parser::LR1::Builder).new
JIS2::AST.compose_known_rules parser
at = Parser::Automaton.new(parser.build("program"))

input = <<-JIS2
module "default"
  func collatz[∇ num] ∇
    given ∇ a := !1
    until =[a|!2] do
    finally a := !0;
    end
  end

  func main[] ∇
  end
end
JIS2

tree = lexer.lex(input, at)

PrettyPrint.format(tree.try &.value(JIS2::Module), STDOUT, 119)
puts
