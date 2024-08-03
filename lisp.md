<details>
  <summary>Interpreter</summary>

  ```js   
  //
  // lisp.js
  //

  // Tokenize a source string
  function tokenize(source) {
      const spec = {
          open: /\(/,
          close: /\)/,
          quote: /'/,
          quasiquote: /`/,                                        //CUT
          'unquote-splicing': /~@/,                               //CUT
          unquote: /~/,                                           //CUT
          number: /-?(?:\d*[.])?\d+/,
          string: /"(?:\\\\|\\.|[^"])*"/,
          comment: /;;[^\n]*/,
          symbol: /[^0-9\s\)\('"~`][^\s\)\('"~`;]*/,
          space: /\s+/
      };
      const language = Object.values(spec).map(s => s.source).join("|");
      return source
          .split(new RegExp("(" + language + ")"))
          .filter(t => t && t.trim().length > 0)
          .map(text => ({
              token: Object.keys(spec).find(name =>
                  text.match(new RegExp('^' + spec[name].source + '$'))),
              value: text }));
  }

  function createSimpleType(type, value) {
      const obj = { };
      obj[type] = value;
      return obj;
  }

  function isSimpleType(type, obj) {
      const keys = Object.keys(obj || {});
      return keys.length === 1 && keys[0] === type;
  }

  function symbol(name) { return createSimpleType('symbol', name); }
  function isSymbol(value) { return isSimpleType('symbol', value); }

  function read(source) {
      const expressions = [];
      const tokens = tokenize(source).filter(t => t.token !== 'comment');
      while (tokens.length > 0) {
          expressions.push(parse());
      }
      return expressions;

      function parse() {
          const next = tokens.shift();
          switch (next.token) {
              case 'number':
                  return parseFloat(next.value);
              case 'string':
                  return JSON.parse(next.value);
              case 'symbol':
                  return symbol(next.value);
              case 'quote':
              case 'quasiquote':                                         //CUT
              case 'unquote':                                            //CUT
              case 'unquote-splicing':                                   //CUT
                  if (tokens.length <= 0) { throw `${next.token} expects a value`; }
                  return [symbol(next.token), parse()];
              case 'open':
                  const values = [];
                  while (tokens.length > 0 && tokens[0].token !== 'close') {
                      values.push(parse());
                  }
                  if (tokens.length <= 0) { throw 'Unbalanced expression'; }
                  tokens.shift(); // read the close token
                  return values;
              case 'close': throw 'Unbalanced expression';
              default: throw 'Unknown token type ' + next.token;
          }
      }
  }

  function write(value) {
      switch (typeof value) {
          case 'undefined':
          case 'boolean':
          case 'number':
          case 'string':
              return JSON.stringify(value);
          case 'function':
              return '#<fn>';
          case 'object':
          default:
              if (value === null) {
                  return 'null';
              } else if (isSymbol(value)) {
                  return value.symbol;
              } else if (Array.isArray(value)) {
                  return `(${value.map(write).join(' ')})`;
              } else {
                  return `#<${JSON.stringify(value)}>`;
              }
      }
  }

  // Core environment
  const core = { };

  function createEnvironment(parentEnv = core) {
      return Object.create(parentEnv);
  };

  function set(env, key, val) {
      let binding = env[key];
      if (binding) {
          // Update the binding if it already exists
          binding[1] = val;
      } else {
          // Create a new binding if it doesn't
          env[key] = [key, val];
      }
      return val;
  }

  var debug = false;                                                       //CUT
  function get(env, key) {
      const binding = env[key];
      if (binding) {
          return binding[1];
      } else {
          if (debug) {                                                     //CUT
              console.log(`// WARNING: Accessing undefined ${key}`);       //CUT
          }                                                                //CUT
          return undefined;
      }
  }

  set(core, 'null', null);
  set(core, 'undefined', undefined);
  set(core, 'true', true);
  set(core, 'false', false);

  set(core, '+', ([x, y]) => x + y);
  set(core, '-', ([x, y]) => x - y);
  set(core, '*', ([x, y]) => x * y);
  set(core, '/', ([x, y]) => x / y);
  set(core, '==', ([x, y]) => x === y);
  set(core, '!=', ([x, y]) => x !== y);
  set(core, '>', ([x, y]) => x > y);
  set(core, '<', ([x, y]) => x < y);
  set(core, '>=', ([x, y]) => x >= y);
  set(core, '<=', ([x, y]) => x <= y);
  set(core, 'not', ([x]) => !x);

  set(core, 'obj', () => ({ }));
  set(core, 'create', ([proto]) => Object.create(proto || null));
  set(core, 'get-property', ([a, i]) => a ? a[isSymbol(i) ? i.symbol : i] : undefined);
  set(core, 'set-property', ([a, i, v]) => a[isSymbol(i) ? i.symbol : i] = v);

  set(core, 'array', (vals) => vals);
  set(core, 'length', ([a]) => a || a === "" ? a.length : undefined);
  set(core, 'concat', (args) => [].concat.apply([], args));
  set(core, 'push!', ([a, v]) => a.push(v));
  set(core, 'slice', ([a, i, e]) => a && a.length > i ? a.slice(i, e) : []);

  set(core, 'split', ([str, sep]) => str.split(sep));
  set(core, 'regex', ([pat]) => new RegExp(pat, 'g'));
  set(core, 'replace', ([str, pat, sub]) => str.replace(pat, sub));
  set(core, '->json', ([a]) => JSON.stringify(a));
  set(core, '<-json', ([str]) => JSON.parse(str));

  set(core, 'symbol->string', ([s]) => s.symbol);
  set(core, 'string->symbol', ([s]) => symbol(s));

  set(core, 'undefined?', ([v]) => v === undefined);
  set(core, 'null?', ([v]) => v === undefined ||
                              v === null ||
                              (Array.isArray(v) && v.length == 0));
  set(core, 'bool?', ([v]) => typeof v === 'boolean');
  set(core, 'number?', ([v]) => typeof v === 'number');
  set(core, 'array?', ([v]) => Array.isArray(v));
  set(core, 'string?', ([v]) => typeof v === 'string');
  set(core, 'symbol?', ([v]) => isSymbol(v));
  set(core, 'fn?', ([v]) => typeof v === 'function');

  set(core, 'read', ([s]) => read(s)[0]);
  set(core, 'write', ([v]) => write(v));
  set(core, 'print', ([x]) => console.log(x));
  set(core, 'read-file', ([path]) => require('fs').readFileSync(path, 'utf8'));
  set(core, 'write-file', ([path, contents]) =>
                            require('fs').writeFileSync(path, contents, 'utf8'));

  set(core, 'throw', ([v]) => { throw v; });

  function form(evaluator) { return createSimpleType('form', evaluator); }
  function isForm(value) { return isSimpleType('form', value); }

  set(core, 'quote', form(([, val], env) => val));

  set(core, 'set', form(([, sym, val], env) => {
      const value = evaluate(val, env);
      set(env, sym.symbol, value);
  }));

  set(core, 'if', form(([, condition, consequent, alternative], env) =>
       evaluate(condition, env) ?
           evaluate(consequent, env) :
           evaluate(alternative, env)));

  set(core, 'begin', form(([, ...body], env) => evaluateAll(body, env)));
  function evaluateAll(exprs, env) {
      let last = null;
      for (const e of exprs) {
          last = evaluate(e, env);
      }
      return last;
  }

  set(core, 'fn', form(([, args, ...body], env) =>
      (vals, runtimeEnv) => {
          const extended = extend(env, args, vals);
          return evaluateAll(body, extended);
      }));

  function extend(env, args, vals) {
      const extended = createEnvironment(env);
      for (let i = 0; i < args.length; i++) {
          let name = args[i].symbol;
          let value = vals[i];
          if (name === '...') {
              name = args[i - 1].symbol;
              value = vals.slice(i - 1);
          }
          extended[name] = [name, value]; //@@1
      }
      return extended;
  }

  set(core, 'apply', ([f, a], env) => f(a, env));

  function evaluate(expr, env) {
      if (isSymbol(expr)) {
          // Look a symbol up in the environment
          return get(env, expr.symbol);
      } else if (Array.isArray(expr) && expr.length > 0) {
          // Get the operator and check whether it's a special form or function
          const operator = evaluate(expr[0], env);
          if (isForm(operator)) {
              // Evaluate the special form
              return operator.form(expr, env);
          } else if (typeof operator === 'function') {
              // Apply a function
              const args = expr.slice(1).map(a => evaluate(a, env));
              return operator(args, env);
          } else {
              // If you've accidentally evaluated (1 + 2), we'll throw an error
              // reminding you that 1 isn't a function
              throw `Cannot apply ${write(expr[0])}`;
          }
      } else {
          // Everything else evaluates to itself
          return expr;
      }
  }

  set(core, 'eval', ([e], env) => evaluate(e, env));

  set(core, 'load', ([path], env) => load(path, env));
  function load(path, env = core) {
      const source = require('fs').readFileSync(path, 'utf8');
      return run(source, env);
  }

  function repl() {
      const toplevel = createEnvironment();
      require('repl').start({
          terminal: false,
          writer: v => write(v) + '\n',
          eval: function(source, context, filename, callback) {
              if (source.trim() === '%debug') { //CUT
                  debug = true;                 //CUT
              }                                 //CUT
              callback(null, run(source, toplevel));
          }
      });
  }

  function macro(transformer) { return createSimpleType('macro', transformer); }
  function isMacro(value) { return isSimpleType('macro', value); }

  set(core, 'macro?', ([v]) => isMacro(v));

  function run(source, env = core) {
      let exprs = read(source);
      let value = null;
      for (let e of exprs) {
          e = expand(e, env);                               //!!
          value = evaluate(e, env);
      }
      return value;
  }

  set(core, 'expand', ([e], env) => expand(e, env));

  function defmacro([[name, ...args], ...body], env, expand) {
      const source = [symbol('fn'), args, ...body];
      const expanded = expand(source, env, expand);
      const transformer = evaluate(expanded, env);
      set(env, name.symbol, macro(transformer));
      return undefined;
  }
  set(core, 'define-macro', macro(defmacro));

  set(core, 'unquote', macro(([...args], env) => {
      throw `unquote can only be used inside quasiquote.`; }));
  set(core, 'unquote-splicing', macro(([...args], env) => {
      throw `unquote-splicing can only be used inside quasiquote.`; }));

  function quasiquote(expr) {
      // Anything that's not a list just gets returned
      if (!Array.isArray(expr) || expr.length === 0) {
          // Though we quote symbols so they're treated as data
          return isSymbol(expr) ? [symbol('quote'), expr] : expr;
      }

      // Split the list into a head and tail
      const [first, ...rest] = expr;

      // If the first item is unquote, just return the second item
      if (isSymbol(first) && first.symbol === 'unquote') {
          return rest[0];
      }

      // If the first item is an array with unquote-splicing
      if (Array.isArray(first) &&
          isSymbol(first[0]) &&
          first[0].symbol === 'unquote-splicing') {

          // Concat the values onto the rest of the expression
          return [symbol('concat'), first[1], quasiquote(rest)];
      }

      // Otherwise we'll just recurse, but since we don't when to expect an
      // unquote-splicing we'll wrap everything in arrays and concat it together
      const value = [symbol('array'), quasiquote(first)];
      return [symbol('concat'), value, quasiquote(rest)];
  }
  set(core, 'quasiquote', macro(([e], env) => quasiquote(e)));

  function expandSteps(n) {
      return function recur(expr, env) {
          return (--n <= 0) ?
              expr :
              expand(expr, env, recur);
      };
  };
  set(core, 'expand-once', ([e], env) => expand(e, env, expandSteps(1)));
  set(core, 'expand-n', ([n, e], env) => expand(e, env, expandSteps(n)));

  const gensym = (() => {
      let count = 1;
      return ([name]) => name ?
          symbol(name.symbol + '$$' + count++) :
          symbol('$$' + count++);
  })();
  set(core, 'gensym', gensym);

  set(core, 'begin-for-syntax', macro(([...body], env, expand) => {
      body.map(e => evaluate(expand(e, env, expand), env));
      return undefined;
  }));

  function expandUntil(stop) {
      return (expr, env, recur) => {
          const first = (Array.isArray(expr) && expr.length > 0) ?
              expr[0] :
              expr;
          // Continue expanding until we see the stop symbol
          return (isSymbol(first) && first.symbol === stop.symbol) ?
              expr :
              expand(expr, env, recur);
      };
  }
  set(core, 'partial-expand', ([e, stop], env) => {
      const expander = expandUntil(stop);
      return expander(e, env, expander);
  });

  const synthetic = macro(([value], env) => value);
  set(core, '#literal', synthetic);

  function expand(expr, env, recur = expand) {
      if (Array.isArray(expr) && expr.length > 0) {
          // Expand the first part of the expression
          const first = recur(expr[0], env, recur);
          let rest = expr.slice(1);

          // Check if this expression represents a macro and expand it if so
          if (isSymbol(first)) {
              const operator = get(env, first.symbol);
              if (isMacro(operator)) {
                  // Invoke the macro's syntax transformer and then continue
                  // expanding the result (in case the transformer introduced
                  // new macros to expand)
                  const result = operator.macro(rest, env, recur);
                  return operator === synthetic ?                           //!!
                      result :                                              //!!
                      recur(result, env, recur);                            //!!
              }
          }

          // Expand the rest of the expression, unless it's a quotation
          if (!isSymbol(first) || first.symbol !== 'quote') {
              rest = rest.map(e => recur(e, env, recur));
          }
          return [first, ...rest];
      }

      // Everything but symbols are treated as literals                     //!!
      return isSymbol(expr) ?                                               //!!
          expr :                                                            //!!
          recur([symbol('#literal'), expr], env, recur);                    //!!
  }

  // Load our runtime library
  load('runtime.lisp');

  // Keep this code at the bottom of the file so any top level statements are run
  // before launching the REPL
  if (require.main === module) {
      if (process.argv[2]) {
          // Load the file
          load(process.argv[2], createEnvironment());
      } else {
          // Launch the REPL by default
          repl();
      }
  }
  ```
</details>

<details>
  <summary>Runtime</summary>

  ```scheme
  ;;
  ;; runtime.lisp
  ;;

  (set first (fn (l) (get-property l 0)))
  (set second (fn (l) (get-property l 1)))
  (set rest (fn (l) (slice l 1))) ;;@@1
  (set last (fn (l) (get-property l (- (length l) 1))))
  (set penultimate (fn (l) (get-property l (- (length l) 2))))

  (define-macro (define sig body ...)
    `(set ~(first sig) (fn ~(rest sig) ~@body)))

  (define (map f l)
    ((fn (loop result)
         (set result (array))
         (define (loop i)
           (if (< i (length l))
               (begin (push! result (f (get-property l i) i))
                      (loop (+ i 1)))
               result))
         (loop 0))
     null
     null))

  (define-macro (when condition body ...)
    `(if ~condition
         (begin ~@body)
         undefined))

  (define-macro (with-gensyms vars body ...)
    `(let ~(map (fn (v) `(~v (gensym '~v))) vars)
       ~@body))

  (define-macro (let* bindings body ...)
    (if (null? bindings)
        `(begin ~@body)
        `(let (~(first bindings))
           (let* ~(rest bindings)
                 ~@body))))

  (define-macro (letrec bindings body ...)
    `(let ~(map (fn (b) `(~(first b) null)) bindings)
       ~@(map (fn (b) `(set ~(first b) ~(second b))) bindings)
       ~@body))

  (define-macro (let bindings body ...)
    (if (symbol? bindings)                                                  ;;!!
        `(let ((~bindings null))                                            ;;!!
           (set ~bindings (fn ~(map first (first body)) ~@(rest body)))     ;;!!
           (~bindings ~@(map second (first body))))                         ;;!!
        `((fn ~(map first bindings) ~@body)
          ~@(map second bindings))))

  (define (make-counter)
     (let ((count 0))
       (fn ()
         (set count (+ count 1))
         count)))

  (define-macro (cond clauses ...)
    (if (null? clauses)
        false
        (let* ((clause (first clauses))
               (check (first clause))
               (body (rest clause))
               (remaining (rest clauses)))
          `(if ~check
               (begin ~@body)
               (cond ~@remaining)))))

  (define-macro (and a b ...)
    (if (null? b)
        a
        `(if ~a (and ~@b) false)))

  (define-macro (or a b ...)
    (if (null? b)
        a
        (with-gensyms (x)
          `(let ((~x ~a))
             (if ~x ~x (or ~@b))))))

  (define (all? f l)
    (cond
     ((null? l) true)
     ((f (first l)) (all? f (rest l)))))

  (define (any? f l)
    (cond
     ((null? l) false)
     ((f (first l)) (first l))
     (true (any? f (rest l)))))

  (define (flatmap f l)
    (apply concat (map f l)))

  (define (prepend x l)
    (concat (array x) l))

  (define (filter f l)
    (if (null? l)
        '()
        (let ((a (first l))
              (b (filter f (rest l))))
          (if (f a)
              (prepend a b)
              b))))

  (define (reduce f l a)
    (cond
     ((null? l) a) ;;@@1
     ((undefined? a) (reduce f (rest l) (first l))) ;;@@2
     (true (reduce f (rest l) (f a (first l))))))

  (define (str values ...)
    (let ((toString (fn (x) (if (symbol? x) (symbol->string x) (+ "" x)))))
      (reduce + (map toString values) "")))

  (define (join separator values)
    (cond
     ((== 0 (length values)) "")
     ((== 1 (length values)) (str (first values)))
     (true (reduce (fn (a x) (str a separator x)) values))))

  (define (equal? a b)
    (cond
     ((undefined? a)
      (undefined? b))
     ((null? a)
      (null? b))
     ((or (bool? a) (number? a) (string? a) (fn? a) (macro? a))
      (== a b))
     ((symbol? a)
      (and (symbol? b)
           (== (symbol->string a) (symbol->string b))))
     ((array? a)
      (and (array? b)
           (== (length a) (length b))
           (equal? (first a) (first b))
           (equal? (rest a) (rest b))))))

  (define (member? v l)
    (any? (fn (x) (equal? x v)) l))

  (define (union sets ...)
    (let ((all (apply concat sets))
          (adjoin (fn (u x) (if (member? x u) u (concat u (array x))))))
      (reduce adjoin all '())))

  (define (pattern-var? v)
    (and (symbol? v)
         (== "?" (get-property (symbol->string v) 0))))

  (define (pattern-sequence? v)
    (and (array? v)
         (> (length v) 1)
         (equal? '... (last v))))

  (define (get-pattern-vars pattern)
    (cond
     ((pattern-var? pattern) (array pattern))
     ((array? pattern) (apply union (map get-pattern-vars pattern)))
     (true '())))

  (define-macro (matcher pattern)
    (with-gensyms (vars input)
      `(fn (~input)
         (let ((~vars (obj)))
           (and (match-pattern ~pattern ~input ~vars false)
                ~vars)))))

  (define-macro (match-pattern pattern value vars nested)
    (let ((m (cond
              ((pattern-sequence? pattern) 'match-pattern-sequence)
              ((array? pattern) 'match-pattern-list)
              ((pattern-var? pattern) 'match-pattern-var)
              (true 'match-pattern-literal))))
      `(~m ~pattern ~value ~vars ~nested)))

  (define-macro (match-pattern-literal pattern value vars nested)
    `(equal? '~pattern ~value))

  (define-macro (match-pattern-var pattern value vars nested)
    `(collect-pattern-var ~vars '~pattern ~value ~nested))

  (define (collect-pattern-var vars variable value nested)
    (if nested
      (push! (get-property vars variable) value)
      (set-property vars variable value))
    true) ;; Always return true so the match succeeds

  (define-macro (match-pattern-list pattern value vars nested)
    `(and (array? ~value)
          (== ~(length pattern) (length ~value))
          ~@(map (fn (p i) `(match-pattern ~p (get-property ~value ~i) ~vars ~nested))
                 pattern)))

  (define-macro (match-pattern-sequence pattern value vars nested)
    (with-gensyms (item)
      (let ((fixed (- (length pattern) 2))
            (subpattern (penultimate pattern)))
        `(and (array? ~value)
              (>= (length ~value) ~fixed)
              ~@(map (fn (p i) `(match-pattern ~p (get-property ~value ~i) ~vars ~nested))
                     (slice pattern 0 fixed))
              ~@(map (fn (v) `(set-property ~vars '~v (array)))
                     (get-pattern-vars subpattern))
              (all? (fn (~item) (match-pattern ~subpattern ~item ~vars true))
                    (slice ~value ~fixed))))))

  (define-macro (if-match value pattern then else)
    (with-gensyms (match matchfn)
      (let ((vars (get-pattern-vars pattern)))
        `(let* ((~matchfn (matcher ~pattern))
                (~match (~matchfn ~value)))
           (if ~match
               (let ~(map (fn (v) `(~v (get-property ~match '~v))) vars)
                 ~then)
               ~else)))))

  (define-macro (match value clauses ...)
    (if (null? clauses)
        `false
        (with-gensyms (val)
          `(let ((~val ~value))
            (if-match ~val ~(first (first clauses))
              (begin ~@(rest (first clauses)))
              (match ~val ~@(rest clauses)))))))

  (define-macro (-> vals ...)
    (match vals
      ((?v)
       `~?v)
      ((?v (?f ?a ...) ?fs ...)
       `(-> (~?f ~@?a ~?v) ~@?fs))
      ((?v ?f ?fs ...)
       `(-> (~?f ~?v) ~@?fs))))

  (define-macro (override-macro signature body ...)
      (let* ((name (first signature))
             (args (rest signature)))
        (with-gensyms (temp)
          `(begin-for-syntax
              (set ~temp ~name)
              (define-macro (~name ~@args)
                  (let ((base '~temp)) ;;@@1
                      ~@body))))))

  (override-macro (#literal value)
    (let ((interpolate null) (unwrap null))
      (define (interpolate text)
        (let* ((pattern (regex "(\\$\\{[^}]+\\})"))
               (parts (split text pattern)))
          (if (== 1 (length parts))
              (unwrap text)
              `(str ~@(map unwrap parts)))))
      (define (unwrap part)
        (if (and (== "$" (first part))
                 (== "{" (second part))
                 (== "}" (last part)))
            (read (slice part 2 -1)) ;;@1
            `(~base ~part)))
      (if (string? value)
          (interpolate value)
          `(~base ~value))))
  ```
</details>