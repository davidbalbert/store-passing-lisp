require 'singleton'

module SPL
  class SPLError < StandardError; end
  class NameError < SPLError; end
  class ArgError < SPLError; end

  class NullEnvironment
    include Singleton

    def make_environment(bindings = {}, store = EmptyStore.instance)
      Environment.make(bindings, self, store)
    end

    def has_key?(name)
      false
    end

    def [](name)
      raise NameError, "`#{name}' is unbound"
    end
  end

  class Environment
    attr_reader :outer_env, :locations

    def self.make(bindings = {}, outer_env = NullEnvironment.instance, store = EmptyStore.instance)
      starting_location = store.next_location

      locations = Hash[bindings.keys.map.with_index {|name, i| [name, i + starting_location]}]
      values = Hash[bindings.values.map.with_index {|value, i| [i + starting_location, value]}]

      env = Environment.new(locations, outer_env)
      new_store = values.reduce(store) { |store, (location, value)| Store.new(location, value, store) }

      [env, new_store]
    end

    def initialize(locations, outer_env)
      @outer_env = outer_env
      @locations = locations
    end

    def make_environment(bindings = {}, store = EmptyStore.instance)
      Environment.make(bindings, self, store)
    end

    def def(name, value, store)
      loc = store.next_location
      [Environment.new(locations.merge(name => loc), NullEnvironment.instance), Store.new(loc, value, store)]
    end

    def has_key?(name)
      locations.has_key?(name) || outer_env.has_key?(name)
    end

    def [](name)
      if locations.has_key?(name)
        locations[name]
      else
        outer_env[name]
      end
    end

    def to_s
      "#<SPL::Environment #{locations}>"
    end
  end

  class Store
    attr_reader :location, :value, :next_store, :size

    def initialize(location, value, next_store)
      @location = location
      @value = value
      @next_store = next_store
      @size = next_store.size + 1
    end

    def [](loc)
      if location == loc
        value
      else
        next_store[loc]
      end
    end

    def set(loc, value)
      Store.new(loc, value, self)
    end

    def next_location
      locations.max + 1
    end

    def locations
      [location] + next_store.locations
    end

    def to_a
      [[location, value]] + next_store.to_a
    end

    def to_h
      Hash[to_a]
    end

    def to_s
      "#<SPL::Store (#{to_h})>"
    end
  end

  class EmptyStore
    class StoreError < SPLError; end

    include Singleton

    def initialize
      def [](loc)
        raise StoreError, "Can't find location #{loc}. This shouldn't happen"
      end
    end

    def size
      0
    end

    def next_location
      0
    end

    def locations
      []
    end

    def to_a
      []
    end
  end

  module CountableArgs
    private

    def check_arg_count(args)
      if !varargs? && args.size != argc
        raise ArgError, "`#{name}': wrong number of arguments (#{args.size} for #{argc})"
      end

      if varargs?
        min_argc = argc * -1 - 1

        if args.size < min_argc
          raise ArgError, "`#{name}': wrong number of arguments (#{args.size} for #{min_argc}+)"
        end
      end
    end
  end

  class Builtin
    attr_reader :name, :body

    include CountableArgs

    def initialize(name, &body)
      @name = name
      @body = body
    end

    def call(interp, *args)
      check_arg_count(args)

      body.call(interp, *args)
    end

    def argc
      if varargs?
        body.arity + 1
      else
        body.arity - 1
      end
    end

    def varargs?
      body.arity < 0
    end

    def to_s
      "#<builtin #{name}>"
    end
  end

  class Function
    attr_reader :arg_names, :argc, :bodies, :env

    include CountableArgs

    def initialize(arg_names, bodies, env, name = nil)
      @arg_names = arg_names
      @bodies = bodies.to_a
      @env = env
      @argc = get_argc(arg_names)
      @name = name
    end

    def call(interp, *args)
      check_arg_count(args)

      bindings = zip_args(args)
      interp, local_env = interp.make_environment(bindings, env)

      ret = bodies.map do |expr|
        interp, val = interp.eval_without_macroexpand(expr, local_env)

        val
      end.last

      [interp, ret]
    end

    def with_name(name)
      NamedFunction.new(arg_names, bodies, env, name)
    end

    def name
      @name || "(anonymous)"
    end

    def to_s
      "#<function #{name}>"
    end

    def varargs?
      argc < 0
    end

  private
    def zip_args(args)
      if varargs?
        # [0..-3] drops the last two ("&" and "rest")
        positional = arg_names.to_a[0..-3].zip(args)
        varargs = [[arg_names.to_a[-1], List.build(args.drop(positional.size))]]

        Hash[positional + varargs]
      else
        Hash[arg_names.to_a.zip(args)]
      end
    end

    def get_argc(arg_names)
      names_array = arg_names.to_a
      if names_array.include?("&")
        if names_array.count("&") > 1
          raise ArgError, "`&' can't appear more than once in the arg list"
        end

        if names_array[-2] != "&"
          raise ArgError, "`&' must appear in the second to last position of the arg list"
        end

        (arg_names.size - 1) * -1
      else
        arg_names.size
      end
    end
  end

  class Macro < Function
    def to_s
      "#<macro #{name}>"
    end
  end

  class NamedFunction < Function
    undef with_name if method_defined?(:with_name)
  end

  class AST
    class ASTError < SPLError; end;

    extend CountableArgs

    def self.name(name = nil)
      if name
        @name = name
      else
        @name
      end
    end

    def self.args(*arg_names)
      attr_reader *arg_names

      define_singleton_method :argc do
        arg_names.size
      end

      define_method :initialize do |*args|
        arg_names.zip(args).each do |name, val|
          instance_variable_set("@#{name}", val)
        end
      end
    end

    def self.parse_args(args)
      args.map { |a| AST.build(a) }
    end

    def self.parse(args)
      check_arg_count(args)
      args = parse_args(args)
      new(*args)
    end

    def self.varargs?
      argc < 0
    end

    class Def < AST
      name "def"
      args :var, :value
    end

    class SetBang < AST
      name "set!"
      args :var, :value
    end

    class If < AST
      name "if"
      args :predicate, :yes, :no
    end

    class Quote < AST
      name "quote"
      attr_reader :value

      def self.parse_args(args)
        args
      end

      def initialize(value)
        @value = value
      end

      def self.argc
        1
      end
    end

    class QuasiQuote < AST
      name "quasiquote"
      attr_reader :value

      def self.parse_args(args)
        args
      end

      def initialize(value)
        @value = value
      end

      def self.argc
        1
      end
    end

    class Lambda < AST
      name "lambda"
      attr_reader :arglist, :bodies

      def self.parse_args(args)
        unless args.car.respond_to?(:to_a)
          raise TypeError, "#{args.car.inspect} cannot be coerced into Array"
        end

        [args.car.to_a, args.cdr.map { |a| AST.build(a) }.to_a]
      end

      def self.argc
        -3
      end

      def initialize(arglist, bodies)
        @arglist = arglist
        @bodies = bodies
      end
    end

    class DefMacro < AST
      name "defmacro"
      attr_reader :var, :arglist, :bodies

      def self.parse_args(args)
        unless args.second.respond_to?(:to_a)
          raise TypeError, "#{args.second.inspect} cannot be coerced into Array"
        end

        [args.first, args.second.to_a, args.drop(2).map { |a| AST.build(a) }.to_a]
      end

      def self.argc
        -4
      end

      def initialize(var, arglist, bodies)
        @var = var
        @arglist = arglist
        @bodies = bodies
      end
    end

    class Call
      attr_reader :f, :args

      def self.parse(f, args)
        new(AST.build(f), args.map { |a| AST.build(a) }.to_a)
      end

      def initialize(f, args)
        @f = f
        @args = args
      end
    end

    TailCall = Struct.new(:f, :args)

    def self.build(expr)
      case expr
      when Integer, String, EmptyList
        expr
      when List
        args = expr.cdr
        case expr.car
        when "def"
          Def.parse(args)
        when "set!"
          SetBang.parse(args)
        when "if"
          If.parse(args)
        when "quote"
          Quote.parse(args)
        when "quasiquote"
          QuasiQuote.parse(args)
        when "unquote"
          Unquote.parse(args)
        when "lambda"
          Lambda.parse(args)
        when "defmacro"
          DefMacro.parse(args)
        else
          Call.parse(expr.car, args)
        end
      else
        raise ASTError, "Don't know how to build an AST from a #{expr.class}"
      end
    end

    def self.mark_tail_calls(node, is_tail_call: false)
      case node
      when Integer, String, EmptyList, Quote, QuasiQuote
        node
      when Def
        var = AST.mark_tail_calls(node.var, is_tail_call: false)
        value = AST.mark_tail_calls(node.value, is_tail_call: false)
        Def.new(var, value)
      when SetBang
        var = AST.mark_tail_calls(node.var, is_tail_call: false)
        value = AST.mark_tail_calls(node.value, is_tail_call: false)
        SetBang.new(var, value)
      when If
        predicate = AST.mark_tail_calls(node.predicate, is_tail_call: false)
        yes = AST.mark_tail_calls(node.yes, is_tail_call: is_tail_call)
        no = AST.mark_tail_calls(node.no, is_tail_call: is_tail_call)
        If.new(predicate, yes, no)
      when Lambda
        non_tail = node.bodies[0..-2].map { |n| AST.mark_tail_calls(n, is_tail_call: false) }
        tail = AST.mark_tail_calls(node.bodies[-1], is_tail_call: true)
        Lambda.new(node.arglist, non_tail + [tail])
      when DefMacro
        non_tail = node.bodies[0..-2].map { |n| AST.mark_tail_calls(n, is_tail_call: false) }
        tail = AST.mark_tail_calls(node.bodies[-1], is_tail_call: true)
        DefMacro.new(node.var, node.arglist, non_tail + [tail])
      when Call
        if is_tail_call
          TailCall.new(node.f, node.args)
        else
          node
        end
      else
        raise ASTError, "Unrecognized AST node type #{node.class}"
      end
    end
  end

  TailCallResult = Struct.new(:interp, :f, :args)

  class Interpreter
    class EvalError < SPLError; end

    attr_reader :global_env, :store

    def initialize(global_env = NullEnvironment.instance, store = EmptyStore.instance)
      @global_env, @store = global_env, store

      if @global_env.is_a? NullEnvironment
        @global_env, @store = @global_env.make_environment({
          "t" => "t",
          "nil" => EmptyList.instance,
          "car" => Builtin.new("car") { |interp, l| [interp, l.car] },
          "cdr" => Builtin.new("cdr") { |interp, l| [interp, l.cdr] },
          "cons" => Builtin.new("cons") { |interp, car, cdr| [interp, List.new(car, cdr)] },
          "apply" => Builtin.new("apply") { |interp, f, args| f.call(interp, *args) },
          "eval" => Builtin.new("eval") { |interp, expr| interp.eval(expr) },
          "+" => Builtin.new("+") { |interp, *args| [interp, args.reduce(0, :+)] },
          "*" => Builtin.new("*") { |interp, *args| [interp, args.reduce(1, :*)] },
          "-" => Builtin.new("-") do |interp, first, *rest|
            if rest.empty?
              [interp, -1 * first]
            else
              [interp, rest.reduce(first, :-)]
            end
          end,
          "/" => Builtin.new("/") do |interp, first, *rest|
            if rest.empty?
              [interp, first]
            else
              [interp, rest.reduce(first, :/)]
            end
          end,
          "=" => Builtin.new("=") { |interp, a, b| [interp, a == b ? "t" : EmptyList.instance] },
          ">" => Builtin.new(">") { |interp, a, b| [interp, a > b ? "t" : EmptyList.instance] },
          "<" => Builtin.new("<") { |interp, a, b| [interp, a < b ? "t" : EmptyList.instance] },
          ">=" => Builtin.new(">=") { |interp, a, b| [interp, a >= b ? "t" : EmptyList.instance] },
          "<=" => Builtin.new("<=") { |interp, a, b| [interp, a <= b ? "t" : EmptyList.instance] },
          "length" => Builtin.new("length") { |interp, o| [interp, o.length ] },
          "puts" => Builtin.new("puts") do |interp, s|
            puts s
            [interp, EmptyList.instance]
          end,
          "load" => Builtin.new("load") do |interp, path|
            s = File.read(path)

            reader = Reader.new
            forms = reader.read_string(s)

            forms.each do |f|
              interp, _ = interp.eval(f)
            end

            [interp, "t"]
          end,
          "list?" => Builtin.new("list?") do |interp, l|
            [interp, l.is_a?(List) ? "t" : EmptyList.instance]
          end,
          "symbol?" => Builtin.new("symbol?") do |interp, s|
            [interp, s.is_a?(String) ? "t" : EmptyList.instance]
          end,
          "integer?" => Builtin.new("integer?") do |interp, i|
            [interp, i.is_a?(Integer) ? "t" : EmptyList.instance]
          end,
          "macroexpand-1" => Builtin.new("macroexpand-1") do |interp, expr|
            interp.macroexpand_1(expr)
          end
        })
      end
    end

    def make_environment(bindings, outer_env)
      new_env, new_store = outer_env.make_environment(bindings, store)

      [Interpreter.new(global_env, new_store), new_env]
    end

    def def(name, value)
      if global_env.has_key?(name)
        raise EvalError, "cannot `def' bound variable `#{name}'"
      end

      Interpreter.new(*global_env.def(name, value, store))
    end

    def set!(name, value, local_env)
      if local_env.has_key?(name)
        Interpreter.new(global_env, store.set(local_env[name], value))
      elsif global_env.has_key?(name)
        Interpreter.new(global_env, store.set(global_env[name], value))
      else
        raise EvalError, "cannot `set!' unbound variable `#{name}'"
      end
    end

    def eval(expr, local_env = NullEnvironment.instance)
      interp, expanded_expr = compile_macros(expr, local_env)

      ast = AST.mark_tail_calls(AST.build(expanded_expr))

      interp.eval_without_macroexpand(ast, local_env)
    end

    def eval_without_macroexpand(node, local_env = NullEnvironment.instance)
      interp = self

      case node
      when String
        value = get(node, local_env)

        if value.is_a? Macro
          raise EvalError, "can't take value of macro `#{node}'"
        end

        [interp, value]
      when Integer, EmptyList
        [interp, node]
      when AST::Def
        name = node.var
        interp, value = interp.eval_without_macroexpand(node.value, local_env)

        if value.respond_to?(:with_name)
          value = value.with_name(name)
        end

        interp = interp.def(name, value)

        [interp, name]
      when AST::SetBang
        name = node.var

        interp, value = interp.eval_without_macroexpand(node.value, local_env)

        interp = interp.set!(name, value, local_env)

        [interp, name]
      when AST::If
        interp, predicate = interp.eval_without_macroexpand(node.predicate, local_env)

        if predicate != EmptyList.instance
          interp.eval_without_macroexpand(node.yes, local_env)
        else
          interp.eval_without_macroexpand(node.no, local_env)
        end
      when AST::Quote
        [interp, node.value]
      when AST::QuasiQuote
        check_unquotes_for_errors(node.value)
        process_unquotes(node.value, local_env)
      when AST::Lambda
        [interp, Function.new(node.arglist, node.bodies, local_env)]
      when AST::DefMacro
        macro_name = node.var
        macro = Macro.new(node.arglist, node.bodies, local_env, macro_name)

        interp = interp.def(node.var, macro)

        [interp, macro_name]
      when AST::Call
        interp, f = interp.eval_without_macroexpand(node.f, local_env)

        args = node.args

        evaled_args = args.map do |arg|
          interp, arg = interp.eval_without_macroexpand(arg, local_env)

          arg
        end

        #begin
          #if f.respond_to?(:call)
            #f.call(interp, *evaled_args)
          #else
            #raise EvalError, "#{f} is not callable"
          #end
        #rescue TailCallException => tc
          #interp = tc.interp
          #f = tc.f
          #evaled_args = tc.args
          #retry
        #end

        result = TailCallResult.new(interp, f, evaled_args)

        while result
          interp = result.interp
          f = result.f
          evaled_args = result.args

          result = catch(:tailcall) do
            if f.respond_to?(:call)
              return f.call(interp, *evaled_args)
            else
              raise EvalError, "#{f} is not callable"
            end
          end
        end
      when AST::TailCall
        interp, f = interp.eval_without_macroexpand(node.f, local_env)

        args = node.args

        evaled_args = args.map do |arg|
          interp, arg = interp.eval_without_macroexpand(arg, local_env)

          arg
        end

        result = TailCallResult.new(interp, f, evaled_args)
        throw(:tailcall, result)
      else
        raise EvalError, "Don't know how to eval #{node.inspect}"
      end
    end

  protected
    def compile_macros(expr, local_env)
      if expr.is_a?(List) && expr.first == "quote"
        [self, expr]
      elsif expr.is_a?(List) && expr.first == "quasiquote"
        [self, expr]
      elsif expr.is_a?(List)
        interp = self

        old = new = expr

        loop do
          interp, new = interp.macroexpand_1(old)
          break if old == new
          old = new
        end

        macroexpanded_expr = new.map do |ex|
          interp, ex = interp.compile_macros(ex, local_env)

          ex
        end

        [interp, macroexpanded_expr]
      else
        [self, expr]
      end
    end

    def check_unquotes_for_errors(expr)
      if expr.is_a? List
        expr.to_a.each do |ex|
          if ex.is_a?(List) && ex.first == "unquote-splicing"
            raise EvalError, "`unquote-splicing' takes one argument" unless ex.size == 2
          elsif ex.is_a?(List) && ex.first == "unquote"
            raise EvalError, "`unquote' takes one argument" unless ex.size == 2
          else
            check_unquotes_for_errors(ex)
          end
        end
      end
    end

    def process_unquotes(expr, local_env)
      if expr.is_a? List
        interp = self
        processed_exprs = expr.to_a.reduce([]) do |array, ex|
          if ex.is_a?(List) && ex.first == "unquote-splicing"
            interp, processed = interp.eval(ex.second, local_env)

            unless processed.is_a?(List) || processed.is_a?(EmptyList)
              raise EvalError, "`unquote-splicing' must take a list"
            end

            array + processed.to_a
          elsif ex.is_a?(List) && ex.first == "unquote"
            interp, processed = interp.eval(ex.second, local_env)

            array << processed
          else
            interp, processed = interp.process_unquotes(ex, local_env)

            array << processed
          end
        end

        [interp, List.build(processed_exprs)]
      else
        [self, expr]
      end
    end

    def macroexpand_1(expr)
      unless expr.is_a?(List)
        return [self, expr]
      end

      unless expr.car.is_a?(String)
        return [self, expr]
      end

      unless global_env.has_key?(expr.car)
        return [self, expr]
      end

      macro = get(expr.car, NullEnvironment.instance)

      unless macro.is_a?(Macro)
        return [self, expr]
      end

      macro.call(self, *expr.rest.to_a)
    end

  private
    def get(name, local_env)
      if local_env.has_key?(name)
        store[local_env[name]]
      else
        store[global_env[name]]
      end
    end

    def has_key?(name, local_env)
      local_env.has_key?(name) || global_env.has_key?(name)
    end
  end

  class List
    def self.build(array)
      if array.empty?
        EmptyList.instance
      else
        List.new(array.first, build(array.drop(1)))
      end
    end

    attr_reader :car, :cdr, :size

    alias length size

    def initialize(car, cdr)
      @car = car
      @cdr = cdr
      @size = cdr.size + 1
    end

    alias first car
    alias rest cdr

    def drop(n)
      if n == 0
        self
      else
        cdr.drop(n - 1)
      end
    end

    def second
      cdr.first
    end

    def third
      cdr.second
    end

    def fourth
      cdr.third
    end

    def map(&block)
      List.new(yield(car), cdr.map(&block))
    end

    def to_a
      [car] + cdr.to_a
    end

    def to_s
      "(#{to_a.join(" ")})"
    end

    def inspect
      "#<SPL::List #{to_s}>"
    end
  end

  class EmptyList
    include Singleton

    def car
      self
    end

    def cdr
      self
    end

    alias first car
    alias second car
    alias third car
    alias fourth car
    alias rest cdr

    def size
      0
    end

    alias length size

    def map
      self
    end

    def to_a
      []
    end

    def to_s
      "nil"
    end
  end

  True = "t"
  False = EmptyList.instance

  class Reader
    class ReadError < SPLError; end

    def read
      s = ""

      begin
        s += gets
      end while s.strip.empty? || parens_are_unbalanced?(s)

      parse(s)
    end

    def read_string(s)
      if parens_are_unbalanced?(s)
        raise ReadError, "Parens are unbalanced"
      end

      parse(s)
    end

    def read_one(s)
      read_string(s).first
    end

  private
    def parse(s)
      tokens = s.gsub('(', ' ( ').
        gsub(')', ' ) ').
        gsub('\'', ' \' ').
        gsub('`', ' ` ').
        gsub(',@', ',@ ').
        gsub(/,([^@])/) {", #{$1}"}.
        split

      forms = []
      until tokens.empty?
        forms << read_tokens(tokens)
      end

      forms
    end

    def read_tokens(tokens)
      token = tokens.shift

      if token == '('
        l = []

        while tokens.first != ')'
          l << read_tokens(tokens)
        end

        tokens.shift # => ')'

        List.build(l)
      elsif token == '\''
        List.build(["quote", read_tokens(tokens)])
      elsif token == '`'
        List.build(["quasiquote", read_tokens(tokens)])
      elsif token == ','
        List.build(["unquote", read_tokens(tokens)])
      elsif token == ',@'
        List.build(["unquote-splicing", read_tokens(tokens)])
      elsif token.to_i.to_s == token
        token.to_i
      else
        token
      end
    end

    def parens_are_unbalanced?(s)
      count = 0
      s.each_char do |c|
        if c == '('
          count += 1
        elsif c == ')'
          count -= 1
        end

        if count < 0
          raise ReadError, "Too many ')'s"
        end
      end

      count != 0
    end
  end

  class REPL
    attr_reader :interp, :reader

    def initialize
      @interp = Interpreter.new
      @reader = Reader.new
    end

    def run
      trap(:INT) {
        puts
        exit 0
      }

      @interp, _ = eval(reader.read_one("(load 'kernel.lisp)"))

      loop do
        print "spl> "

        begin
          forms = read

          forms.each do |f|
            @interp, out = eval(f)
            puts "=> #{out}"
          end
        rescue StandardError => e
          puts e.message
        end
      end
    end

  private
    def read
      reader.read
    end

    def eval(form)
      interp.eval(form)
    end
  end
end

if $0 == __FILE__
  SPL::REPL.new.run
end
