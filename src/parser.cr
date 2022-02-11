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
  record NonTerminal < Node, name : String do
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
  DOT = Dot.new
  EPSILON = Epsilon.new
  EOS = EndOfStream.new
  ENTRYPOINT = "ENTRYPOINT"

  record Production, name : String, body : Array(Node) do
    def inspect(io : IO)
      io << smart_name << " -> " << @body.join " "
    end
    def smart_name : String
      (name == ENTRYPOINT ? "*".colorize.bold.green : name.colorize.bold).to_s
    end
    def epsilon? : Bool
      @body == [EPSILON]
    end
    def result : NonTerminal
      NonTerminal.new @name
    end
  end

  record Token(T), symbol : Symbol, object : T
end
