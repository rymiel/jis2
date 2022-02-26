require "colorize"
require "./parser/util"
require "./parser/lr0"
require "./parser/lr1"
require "./parser/analysis"
require "./parser/automaton"
require "./parser/ast"

module Parser
  VERSION = "0.1.0"

  abstract struct Node
  end

  record NonTerminal < Node, name_idx : Int32 do
    def initialize(name : String)
      idx = Parser.string_pool[name]?
      if idx.nil?
        idx = Parser.string_pool.size
        Parser.string_pool[name] = idx
      end
      @name_idx = idx
    end

    def name : String
      Parser.string_pool.key_for @name_idx
    end

    def inspect(io : IO)
      io << name.inspect.colorize.yellow
    end
  end
  record Terminal < Node, symbol : Symbol do
    def inspect(io : IO)
      io << symbol.inspect.colorize.cyan
    end
  end

  struct Dot
    def inspect(io : IO)
      io << "·".colorize.magenta.bold
    end
  end

  struct Epsilon < Node
    def inspect(io : IO)
      io << "ɛ".colorize.red.bold
    end
  end

  struct EndOfStream < Node
    def inspect(io : IO)
      io << "$".colorize.light_red.bold
    end
  end

  DOT        = Dot.new
  EPSILON    = Epsilon.new
  EOS        = EndOfStream.new
  ENTRYPOINT = "ENTRYPOINT"
  class_getter string_pool = {ENTRYPOINT => 0}

  record Production, name_idx : Int32, body : Array(Node) do
    def initialize(name : String, @body)
      idx = Parser.string_pool[name]?
      if idx.nil?
        idx = Parser.string_pool.size
        Parser.string_pool[name] = idx
      end
      @name_idx = idx
    end

    def name : String
      Parser.string_pool.key_for @name_idx
    end

    def inspect(io : IO)
      io << smart_name << " -> " << @body.join " "
    end

    def smart_name : String
      (@name_idx == ENTRYPOINT ? "*".colorize.bold.green : name.colorize.bold).to_s
    end

    def epsilon? : Bool
      @body.size == 1 && @body.first.is_a?(Epsilon)
    end

    def result : NonTerminal
      NonTerminal.new @name_idx
    end
  end

  record Token(T), symbol : Symbol, object : T
end
