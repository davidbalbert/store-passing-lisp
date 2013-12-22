require 'singleton'

module SPL
  class SPLError < StandardError; end

  class Environment
    class NameError < SPLError; end

    attr_reader :outer_env, :locations

    def self.make(bindings = {}, outer_env = nil, store = EmptyStore.instance)
      starting_location = store.next_location

      locations_names_and_values = Hash[bindings.map.with_index {|(name, value), i| [i + starting_location, [name, value]]}]

      locations = Hash[locations_names_and_values.map { |loc, (name, value)| [name, loc] }]
      values = Hash[locations_names_and_values.map { |loc, (name, value)| [loc, value] }]

      env = Environment.new(locations, outer_env)
      new_store = values.reduce(store) { |store, (location, value)| Store.new(location, value, store) }

      [env, new_store]
    end

    def initialize(locations, outer_env)
      @outer_env = outer_env
      @locations = locations
    end

    def def(name, value, store)
      loc = store.next_location
      [Environment.new(locations.merge(name => loc), self), Store.new(loc, value, store)]
    end

    def has_key?(name)
      locations.has_key?(name) || (outer_env && outer_env.has_key?(name))
    end

    def [](name)
      env = self

      env = env.outer_env until env.nil? || env.locations.has_key?(name)

      raise NameError, "`#{name}' is unbound" if env.nil?

      env.locations[name]
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

  class Function
    class ArgError < SPLError; end

    attr_reader :arg_names, :argc, :bodies, :env, :store, :name

    def initialize(arg_names, bodies, env, store, name = nil)
      @arg_names = arg_names
      @bodies = bodies.to_a
      @env = env
      @store = store
      @argc = arg_names.size
      @name = name
    end

    def call(interp, *args)
      raise ArgError, "wrong number of arguments (#{args.size} for #{argc})" unless args.size == argc

      bindings = Hash[arg_names.to_a.zip(args)]
      new_env, new_store = Environment.make(bindings, env, store)

      ret = bodies.map do |expr|
        interp, val = interp.eval(expr, new_env, new_store)

        val
      end.last

      [interp, ret]
    end

    def with_name(name)
      Function.new(arg_names, bodies, env, store, name)
    end

    def to_s
      if name
        "#<function #{name}>"
      else
        "#<function (anonymous)>"
      end
    end
  end

  class Interpreter
    class EvalError < SPLError; end

    attr_reader :global_env, :global_store

    def initialize(global_env = nil, global_store = nil)
      if global_env.nil?
        # Don't refer to self in any build in lambdas. They may be used with
        # other instances of Interpreter.
        @global_env, @global_store = Environment.make({
          "t" => "t",
          "nil" => EmptyList.instance,
          "car" => lambda { |interp, l| [interp, l.car] },
          "cdr" => lambda { |interp, l| [interp, l.cdr] },
          "cons" => lambda { |interp, car, cdr| [interp, List.new(car, cdr)] },
          "list" => lambda { |interp, *args| [interp, List.build(args)] },
          "+" => lambda { |interp, *args| [interp, args.reduce(:+) || 0] }
        })
      else
        @global_env, @global_store = global_env, global_store
      end
    end

    def def(name, value)
      Interpreter.new(*global_env.def(name, value, global_store))
    end

    def eval(expr, local_env = nil, local_store = EmptyStore.instance)
      case expr
      when String
        [self, get(expr, local_env, local_store)]
      when Integer
        [self, expr]
      when EmptyList
        [self, expr]
      when List
        case expr.car
        when "def"
          check_special_form_args(expr, 2)
          name = expr.second
          interp, value = eval(expr.third, local_env, local_store)

          if value.respond_to?(:with_name)
            value = value.with_name(name)
          end

          interp = interp.def(name, value)

          [interp, name]
        when "if"
          check_special_form_args(expr, 3)

          interp, predicate = eval(expr.second, local_env, local_store)

          if predicate != EmptyList.instance
            interp.eval(expr.third, local_env, local_store)
          else
            interp.eval(expr.fourth, local_env, local_store)
          end
        when "quote"
          check_special_form_args(expr, 1)

          [self, expr.second]
        when "quasiquote"
          check_special_form_args(expr, 1)

          process_unquotes(expr.second, local_env, local_store)
        when "lambda"
          check_special_form_args(expr, 2..-1)
          arg_names = expr.second

          raise EvalError, "arglist #{arg_names} must be a list" unless arg_names.is_a?(List) || arg_names.is_a?(EmptyList)

          bodies = expr.drop(2)

          [self, Function.new(arg_names, bodies, local_env, local_store)]
        when String, List
          interp, val = eval(expr.car, local_env, local_store)

          interp.eval(List.new(val, expr.cdr), local_env, local_store)
        when Proc, Function
          interp = self
          args = expr.rest

          evaled_args = args.to_a.map do |arg|
            interp, arg = interp.eval(arg, local_env, local_store)

            arg
          end

          expr.car.call(self, *evaled_args)
        else
          raise EvalError, "#{expr.car} is not callable"
        end
      end
    end

  protected
    def process_unquotes(expr, local_env, local_store)
      case expr
      when List
        case expr.first
        when "unquote"
          raise EvalError, "`unquote' takes one argument" unless expr.size == 2
          eval(expr.second, local_env, local_store)
        else
          interp = self
          processed_exprs = expr.to_a.reduce([]) do |array, ex|
            if ex.is_a?(List) && ex.first == "unquote-splicing"
              raise EvalError, "`unquote-splicing' takes one argument" unless ex.size == 2

              interp, processed = interp.eval(ex.second, local_env, local_store)

              unless processed.is_a?(List) || processed.is_a?(EmptyList)
                raise EvalError, "`unquote-splicing' must take a list"
              end

              array + processed
            else
              interp, processed = interp.process_unquotes(ex, local_env, local_store)

              array << processed
            end
          end

          [interp, List.build(processed_exprs)]
        end
      else
        [self, expr]
      end
    end

  private
    def get(name, local_env, local_store)
      if local_env && local_env.has_key?(name)
        local_store[local_env[name]]
      else
        global_store[global_env[name]]
      end
    end

    def check_special_form_args(expr, expected_argc)
      argc = expr.size - 1

      case expected_argc
      when Integer
        unless argc == expected_argc
          raise EvalError, "`#{expr.car}' takes #{expected_argc} arguments"
        end
      when Range
        if expected_argc.end == -1 && expected_argc.begin > argc
          raise EvalError, "`#{expr.car}' needs at least #{expected_argc.begin} arguments"
        elsif expected_argc.end != -1 && !expected_argc.include?(argc)
          raise EvalError, "`#{expr.car}' takes #{expected_argc} arguments"
        end
      end
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

    def to_a
      [car] + cdr.to_a
    end
    alias to_ary to_a

    def to_s
      "(#{to_a.join(" ")})"
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

    def to_a
      []
    end

    def to_s
      "nil"
    end
  end

  class Reader
    class ReadError < SPLError; end

    def read
      s = ""

      begin
        s += gets
      end while s.strip.empty? || parens_are_unbalanced?(s)

      tokens = s.gsub('(', ' ( ').
        gsub(')', ' ) ').
        gsub('\'', ' \' ').
        gsub('`', ' ` ').
        gsub(',@', ' ,@ ').
        gsub(/,([^@])/) {" , #{$1}"}.
        split

      forms = []
      until tokens.empty?
        forms << read_tokens(tokens)
      end

      forms
    end

  private
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

      loop do
        print "spl> "

        begin
          forms = read

          forms.each do |f|
            @interp, out = eval(f)
            puts "=> #{out}"
          end
        rescue SPLError => e
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

SPL::REPL.new.run
