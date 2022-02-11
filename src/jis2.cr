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

class Lexer(*T)
  @matches = Array({Regex, Symbol, Proc(Regex::MatchData, Union(*T))?}).new
  @skips = Array(Regex).new

  def self.build(&) : self
    ins = new
    with ins yield
    ins
  end

  def match(regex : Regex, result : Symbol)
    @matches << {regex, result, nil}
  end

  def match(regex : Regex, symbol : Symbol, &result : Regex::MatchData -> Union(*T))
    @matches << {regex, symbol, result}
  end

  def skip(regex : Regex)
    @skips << regex
  end

  def lex(input : String, at)
    pos = 0
    until pos == input.size
      found = false
      puts "at pos #{pos}..."
      @skips.each do |m|
        if match = m.match(input, pos, Regex::Options::ANCHORED)
          pos = match.end
          found = true
          break
        end
      end
      next if found
      @matches.each do |m, sym, r|
        if match = m.match(input, pos, Regex::Options::ANCHORED)
          if r.is_a? Proc
            r = r.call match
          end
          p! match, sym, r
          if r.nil?
            at << sym
          else
            at << {sym, r}
          end
          pos = match.end
          found = true
          break
        end
      end
      next if found
      raise "no match at pos #{pos}: `#{input[pos..pos+30].dump_unquoted}`..."
    end
  end
end

lexer = Lexer(String, Int64).build do
  match /module/, :r_module
  match /func/, :r_func
  match /do/, :r_do
  match /end/, :r_end
  match /given/, :r_given
  match /∇/, :tt_int
  match /\[/, :sq_l
  match /\]/, :sq_r
  match /:=/, :assign
  match /"([^"]*)"/, :string, &.[1]
  match /!(\d+)/, :number, &.[1].to_i64
  match /[a-z]+/, :word, &.[0]
  skip /\s+/
end

input = <<-JIS2
module "default"
  ∇ func collatz[∇ num]
    given ∇ a := !1 do
    end
  end
end
JIS2

pp! lexer
lexer.lex(input, at)

pp! at.@input
# at << :r_module << tok(:string, "default")
  # at << :tt_int << :r_func << tok(:word, "collatz") << :sq_l
    # at << :tt_int << tok(:word, "num")
  # at << :sq_r
    # at << :r_given << :tt_int << tok(:word, "a") << :assign << tok(:number, 1i64) << :r_do << :r_end
  # at << :r_end
# at << :r_end
tree = at.run
PrettyPrint.format(tree.value(JIS2::Module), STDOUT, 119)
puts
