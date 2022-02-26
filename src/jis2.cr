require "./parser"
require "./lexer"

module JIS2
  alias AST = Parser::AST
  alias Ref = Parser::Ref
  alias Opt = Parser::Opt

  @[AST::Aliased(Type)]
  AST.smart_enum(PrimitiveType, {
    integer: :tt_int,
    void: :tt_void,
  })

  AST.smart_enum(BlockStatementKind, {
    given: :given,
    repeat: :repeat,
    until: :until,
  })

  AST.smart_enum(Operator, {
    equal: :"=",
    plus: :"+",
    times: :"*",
    minus: :"-",
    divide: :"÷",
    int_divide: :"÷∇",
    modulo: :"mod",
  })

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

  record LeftReference < Expression do
    include AST
    rule :"$"
  end

  class ExpressionStatement < Statement
    include AST
    getter expression : Expression

    rule @expression, :";"

    def initialize(@expression)
    end
  end

  class ReturnStatement < Statement
    include AST
    getter expression : Expression

    rule :return, @expression, :";"

    def initialize(@expression)
    end
  end

  struct Call < Expression
    include AST
    getter name : String
    @[Separator(:"|", name: CallArgs)]
    getter args : Array(Expression)
    getter operator : Operator?

    rule :_word >> @name, :"[", @args, :"]"
    rule ::JIS2::Operator =~ @name, :"[", @args, :"]"

    def initialize(@name, @args)
    end
    def initialize(name @operator : Operator, @args)
      @name = ""
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

  class IfStatement < Statement
    include AST
    getter condition : Expression
    getter body : Array(Statement)
    getter else_body : Array(Statement)?

    rule :if, @condition, :then, @body, :end
    rule :if, @condition, :then, @body, :else, Array(::JIS2::Statement) =~ @else_body, :end

    def initialize(@condition, @body)
    end
    def initialize(@condition, @body, @else_body)
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

    def diagnostic(io : IO)
      @name.to_s io
      io << " ["
      @args.each { |i| io << "#{i.type} #{i.name}" }
      io << "] "
      @return_type.to_s io
    end

    def diagnostic : String
      String.build do |s|
        diagnostic s
      end
    end

    def to_s(io : IO)
      io << "#<JIS2::FunctionDefinition("
      diagnostic io
      io << ")>"
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
  match :module, :func, :if, :then, :else, :do, :end, :given, :until, :repeat, :finally, :return
  match /∇/, :tt_int
  match :"[", :"]", :":=", :"=", :|, :"$", :";", :+, :-, :*, :÷∇, :÷, :mod
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
