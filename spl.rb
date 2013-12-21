require 'singleton'

module SPL
  class SPLError < StandardError; end

  class Environment
    class NameError < SPLError; end

    attr_reader :outer_env, :locations

    def self.make(bindings = {}, store = EmptyStore.new, outer_env = nil)
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

    def [](name)
      env = self

      env = env.outer_env until env.nil? || env.locations.has_key?(name)

      raise NameError, "`#{name}' is unbound" if env.nil?

      env.locations[name]
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

  private
    def locations
      locs = []
      store = self

      until store.is_a? EmptyStore
        locs << store.location
        store = store.next_store
      end

      locs
    end
  end

  class EmptyStore
    class StoreError < SPLError; end

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
  end

  class Function
    class ArgError < SPLError; end

    attr_reader :arg_names, :argc, :bodies, :env, :store, :name

    def initialize(arg_names, bodies, env, store)
      @arg_names = arg_names
      @bodies = bodies.to_a
      @env = env
      @store = store
      @argc = arg_names.size
    end

    def call(interp, *args)
      raise ArgError, "wrong number of arguments (#{args.size} for #{argc})" unless args.size == argc

      bindings = Hash[arg_names.to_a.zip(args)]
      new_env, new_store = Environment.make(bindings, store, env)

      bodies.map do |expr|
        interp.eval(expr, new_env, new_store)
      end.last
    end

    def to_s
      "#<function (anonymous)>"
    end
  end

  class Lisp
    class EvalError < SPLError; end

    attr_reader :env, :store

    def initialize
      @env, @store = Environment.make({
        "t" => "t",
        "nil" => EmptyList.instance,
        "car" => lambda { |l| l.car },
        "cdr" => lambda { |l| l.cdr },
        "+" => lambda { |*args| args.reduce(:+) }
      })
    end

    def eval(form, env = @env, store = @store)
      case form
      when String
        store[env[form]]
      when Integer
        form
      when EmptyList
        form
      when List
        case form.car
        when "def"
          check_special_form_args(form, 2)
          name = form.cdr.car
          value = eval(form.cdr.cdr.car, env, store)
          @env, @store = @env.def(name, value, store)

          value
        when "if"
          check_special_form_args(form, 3)

          predicate = eval(form.cdr.car, env, store)

          if predicate != EmptyList.instance
            eval(form.cdr.cdr.car, env, store)
          else
            eval(form.cdr.cdr.cdr.car, env, store)
          end
        when "quote"
          check_special_form_args(form, 1)

          form.cdr.car
        when "lambda"
          check_special_form_args(form, 2..-1)
          arg_names = form.cdr.car

          raise EvalError, "arglist #{arg_names} must be a list" unless arg_names.is_a?(List) || arg_names.is_a?(EmptyList)

          bodies = form.cdr.cdr

          Function.new(arg_names, bodies, env, store)
        when String, List
          eval(List.new(eval(form.car, env, store), form.cdr), env, store)
        when Proc
          form.car.call(*form.cdr.to_a.map { |arg| eval(arg, env, store) })
        when Function
          form.car.call(self, *form.cdr.to_a.map { |arg| eval(arg, env, store) })
        else
          raise EvalError, "#{form.car} is not callable"
        end
      end
    end

  private
    def check_special_form_args(form, expected_argc)
      argc = form.size - 1

      case expected_argc
      when Integer
        unless argc == expected_argc
          raise EvalError, "`#{form.car}' takes #{expected_argc} arguments"
        end
      when Range
        if expected_argc.end == -1 && expected_argc.begin > argc
          raise EvalError, "`#{form.car}' needs at least #{expected_argc.begin} arguments"
        elsif expected_argc.end != -1 && !expected_argc.include?(argc)
          raise EvalError, "`#{form.car}' takes #{expected_argc} arguments"
        end
      end
    end
  end

  class List
    def self.build(array)
      if array.empty?
        EmptyList.instance
      else
        List.new(array.shift, build(array))
      end
    end

    attr_reader :car, :cdr, :size

    def initialize(car, cdr)
      @car = car
      @cdr = cdr
      @size = cdr.size + 1
    end

    def to_a
      [car] + cdr.to_a
    end

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
      end until !s.gsub(/\s/, "").empty? && parens_are_balanced?(s)

      tokens = s.gsub('(', ' ( ').gsub(')', ' ) ').split

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

        tokens.shift

        List.build(l)
      elsif token.to_i.to_s == token
        token.to_i
      else
        token
      end
    end

    def parens_are_balanced?(s)
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

      count == 0
    end
  end

  class REPL
    attr_reader :interp, :reader

    def initialize
      @interp = Lisp.new
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

          forms.each { |f| puts "=> #{interp.eval(f)}" }
        rescue SPLError => e
          puts e.message
        end
      end
    end

  private
    def read
      reader.read
    end
  end
end

SPL::REPL.new.run
