const w = require('./worker/worker.cjs');
const { execSync } = require('child_process');
const app = w.Elm.ParseWorker.init();

const tests = [
  // Basic negation contexts (all should parse as negation)
  ["list-neg", "module T exposing (..)\nx = [-1]"],
  ["list-neg-multi", "module T exposing (..)\nx = [-1, -2, -3]"],
  ["tuple-neg", "module T exposing (..)\nx = (-1, -2)"],
  ["paren-neg", "module T exposing (..)\nx = (-1)"],

  // Negation vs subtraction ambiguity
  ["sub-vs-neg-1", "module T exposing (..)\nx = a -1"],         // subtraction: a - 1
  ["sub-vs-neg-2", "module T exposing (..)\nx = a - 1"],        // subtraction: a - 1
  ["sub-vs-neg-3", "module T exposing (..)\nx = a -b"],         // subtraction: a - b
  ["sub-vs-neg-4", "module T exposing (..)\nx = a - b"],        // subtraction: a - b
  ["sub-vs-neg-5", "module T exposing (..)\nx = f -1"],         // application of f to -1
  ["sub-vs-neg-6", "module T exposing (..)\nx = f (-1)"],       // unambiguous: f applied to -1

  // After various opening tokens
  ["neg-after-lbracket", "module T exposing (..)\nx = [-1]"],
  ["neg-after-lparen", "module T exposing (..)\nx = (-1)"],
  ["neg-after-comma-list", "module T exposing (..)\nx = [1,-2]"],
  ["neg-after-comma-tuple", "module T exposing (..)\nx = (1,-2)"],
  ["neg-after-comma-record", "module T exposing (..)\nx = {a=1,b=-2}"],
  ["neg-after-equals", "module T exposing (..)\nx = -1"],
  ["neg-after-arrow", "module T exposing (..)\nf x = case x of\n        _ -> -1"],
  ["neg-after-of", "module T exposing (..)\nf x = case -1 of\n        _ -> 0"],
  ["neg-after-in", "module T exposing (..)\nf = let x = 1 in -1"],
  ["neg-after-then", "module T exposing (..)\nf = if True then -1 else 0"],
  ["neg-after-else", "module T exposing (..)\nf = if True then 0 else -1"],
  ["neg-after-backslash", "module T exposing (..)\nf = \\x -> -x"],

  // Negation of various expression types
  ["neg-of-var", "module T exposing (..)\nx = -a"],
  ["neg-of-qualified", "module T exposing (..)\nx = -Foo.bar"],
  ["neg-of-func-call", "module T exposing (..)\nx = -(f a)"],
  ["neg-of-record-access", "module T exposing (..)\nx = -a.b"],
  ["neg-of-accessor", "module T exposing (..)\nx = -.field"],
  ["neg-of-paren-expr", "module T exposing (..)\nx = -(a + b)"],
  ["neg-of-tuple-access", "module T exposing (..)\nx = -(Tuple.first pair)"],

  // Negation with operators
  ["neg-lhs-plus", "module T exposing (..)\nx = -1 + 2"],
  ["neg-rhs-plus", "module T exposing (..)\nx = 1 + -2"],
  ["neg-both-plus", "module T exposing (..)\nx = -1 + -2"],
  ["neg-in-pipe", "module T exposing (..)\nx = -1 |> abs"],
  ["neg-after-pipe", "module T exposing (..)\nx = f |> negate |> abs"],

  // Double/triple negation
  ["double-neg-parens", "module T exposing (..)\nx = -(-1)"],
  ["double-neg-no-parens", "module T exposing (..)\nx = - -1"],
  ["neg-of-neg-var", "module T exposing (..)\nx = -(-a)"],

  // Negation at specific positions that exercise parser offset code
  ["neg-at-col-1", "module T exposing (..)\nx =\n-1"],
  ["neg-at-col-2", "module T exposing (..)\nx =\n -1"],
  ["neg-at-col-4", "module T exposing (..)\nx =\n    -1"],
  ["neg-in-multiline-list", "module T exposing (..)\nx =\n    [ -1\n    , -2\n    ]"],
  ["neg-in-multiline-tuple", "module T exposing (..)\nx =\n    ( -1\n    , -2\n    )"],
  ["neg-in-multiline-record", "module T exposing (..)\nx =\n    { a = -1\n    , b = -2\n    }"],

  // Negation with hex and float
  ["neg-hex", "module T exposing (..)\nx = -0xFF"],
  ["neg-hex-in-list", "module T exposing (..)\nx = [-0xFF]"],
  ["neg-float", "module T exposing (..)\nx = -1.5"],
  ["neg-float-in-list", "module T exposing (..)\nx = [-1.5]"],
  ["neg-zero", "module T exposing (..)\nx = -0"],
  ["neg-zero-float", "module T exposing (..)\nx = -0.0"],
];

function testOne(name, source) {
  return new Promise(resolve => {
    function handler(data) {
      app.ports.parseResult.unsubscribe(handler);
      const syntaxOk = data.parsed;
      let compilerAccepts = null;
      try {
        const fs = require('fs');
        fs.writeFileSync('/tmp/elm-neg-test/src/Main.elm', source.replace(/module \w+/,'module Main'));
        execSync('elm make src/Main.elm --output=/dev/null', {
          cwd: '/tmp/elm-neg-test', encoding: 'utf-8', timeout: 10000, stdio: ['pipe', 'pipe', 'pipe']
        });
        compilerAccepts = true;
      } catch (e) {
        const stderr = (e.stderr||'').toString();
        // Parse error = compiler rejects syntax
        // Type error = compiler accepts syntax
        compilerAccepts = !stderr.includes('UNEXPECTED') && !stderr.includes('UNFINISHED')
          && !stderr.includes('MISSING') && !stderr.includes('PROBLEM IN DEFINITION')
          && !stderr.includes('WEIRD DECLARATION');
      }

      if (syntaxOk === compilerAccepts) {
        console.log(`  [pass] ${name}`);
      } else if (syntaxOk && !compilerAccepts) {
        console.log(`  [BUG?] ${name} — elm-syntax ACCEPTS but compiler REJECTS`);
      } else {
        console.log(`  [BUG!] ${name} — elm-syntax REJECTS but compiler ACCEPTS!`);
      }
      resolve();
    }
    app.ports.parseResult.subscribe(handler);
    app.ports.requestParsing.send(source);
  });
}

(async () => {
  for (const [name, source] of tests) {
    await testOne(name, source);
  }
})();
