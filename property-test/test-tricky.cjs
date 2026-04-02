const w = require('./worker/worker.cjs');
const { execSync } = require('child_process');
const app = w.Elm.ParseWorker.init();

const tests = [
  // Tricky operator/negation interactions
  ["neg-of-neg", "module T exposing (..)\nx = -(-1)"],
  ["neg-in-record-value", "module T exposing (..)\nx = { a = -1, b = -2 }"],
  ["neg-in-let-binding", "module T exposing (..)\nx =\n    let\n        y = -1\n    in\n    y"],
  ["neg-after-equals", "module T exposing (..)\nx = -1"],
  ["neg-after-pipe", "module T exposing (..)\nx = y |> negate"],

  // Tricky indentation
  ["case-with-where-clause", "module T exposing (..)\nx =\n    case y of\n        Just a ->\n            let\n                b = a\n            in\n            b\n        Nothing -> 0"],
  ["let-in-if", "module T exposing (..)\nx =\n    if True then\n        let\n            a = 1\n        in\n        a\n    else\n        2"],
  ["if-in-let", "module T exposing (..)\nx =\n    let\n        a =\n            if True then 1 else 2\n    in\n    a"],

  // Tricky pattern combinations
  ["pattern-parens-named", "module T exposing (..)\nf x = case x of\n        (Just a) -> a\n        _ -> 0"],
  ["pattern-nested-tuple", "module T exposing (..)\nf x = case x of\n        ((a, b), c) -> a\n        _ -> 0"],
  ["pattern-named-record", "module T exposing (..)\nf x = case x of\n        Foo { a, b } -> a\n        _ -> 0"],

  // Type annotation edge cases
  ["type-parens-func", "module T exposing (..)\nf : (Int -> Int) -> Int\nf g = g 1"],
  ["type-unit-func", "module T exposing (..)\nf : () -> Int\nf _ = 1"],
  ["type-nested-maybe", "module T exposing (..)\nf : Maybe (Maybe Int)\nf = Nothing"],

  // Multiline expressions
  ["multiline-pipe", "module T exposing (..)\nx =\n    a\n        |> b\n        |> c"],
  ["multiline-if-else", "module T exposing (..)\nx =\n    if\n        condition\n    then\n        thenBranch\n    else\n        elseBranch"],
  ["multiline-case-branch", "module T exposing (..)\nx =\n    case y of\n        Just a ->\n            a\n                + 1\n\n        Nothing ->\n            0"],

  // Record type with function type field
  ["record-func-field", "module T exposing (..)\ntype alias R = { f : Int -> Int }"],
  // Extensible record in function type
  ["extensible-in-func", "module T exposing (..)\nf : { a | x : Int } -> Int\nf r = r.x"],

  // Operator as function in various positions
  ["op-in-list", "module T exposing (..)\nx = [(+), (-), (*)]"],
  ["op-as-arg", "module T exposing (..)\nx = List.foldl (+) 0 [1,2,3]"],

  // Tricky string edge cases
  ["string-empty", 'module T exposing (..)\nx = ""'],
  ["string-with-curly", 'module T exposing (..)\nx = "{ a = 1 }"'],
  ["multiline-string-empty", 'module T exposing (..)\nx = """"""'],

  // Custom type with no constructors (should fail)
  ["empty-custom-type", "module T exposing (..)\ntype Foo"],

  // Exposing with type constructors
  ["exposing-type-ctors", "module T exposing (Foo(..))\ntype Foo = Bar | Baz"],

  // Multiple case branches with same pattern
  ["case-dup-patterns", "module T exposing (..)\nf x = case x of\n        0 -> \"zero\"\n        1 -> \"one\"\n        _ -> \"other\""],
];

function testOne(name, source) {
  return new Promise(resolve => {
    function handler(data) {
      app.ports.parseResult.unsubscribe(handler);
      const syntaxOk = data.parsed;
      let formatOk = false;
      try {
        const result = execSync('npx elm-format --stdin --json', {
          input: source, encoding: 'utf-8', timeout: 10000, stdio: ['pipe', 'pipe', 'pipe']
        });
        formatOk = result.includes('moduleName');
      } catch {}

      if (syntaxOk && !formatOk) {
        // Check with elm compiler
        let compilerOk = false;
        try {
          const fs = require('fs');
          fs.writeFileSync('/tmp/elm-neg-test/src/Main.elm', source.replace('module T', 'module Main'));
          execSync('elm make src/Main.elm --output=/dev/null', {
            cwd: '/tmp/elm-neg-test', encoding: 'utf-8', timeout: 10000, stdio: ['pipe', 'pipe', 'pipe']
          });
          compilerOk = true;
        } catch (e) {
          // Check if it's a type error (parse succeeded) vs syntax error
          const stderr = e.stderr || '';
          compilerOk = !stderr.includes('UNEXPECTED') && !stderr.includes('UNFINISHED') && !stderr.includes('MISSING');
        }
        if (compilerOk) {
          console.log(`  [pass*] ${name} — elm-format rejects but compiler parses OK`);
        } else {
          console.log(`  [BUG?] ${name} — elm-syntax accepts but compiler rejects!`);
        }
      } else if (!syntaxOk && formatOk) {
        // Check with compiler
        let compilerOk = false;
        try {
          const fs = require('fs');
          fs.writeFileSync('/tmp/elm-neg-test/src/Main.elm', source.replace('module T', 'module Main'));
          execSync('elm make src/Main.elm --output=/dev/null', {
            cwd: '/tmp/elm-neg-test', encoding: 'utf-8', timeout: 10000, stdio: ['pipe', 'pipe', 'pipe']
          });
          compilerOk = true;
        } catch (e) {
          const stderr = e.stderr || '';
          compilerOk = !stderr.includes('UNEXPECTED') && !stderr.includes('UNFINISHED') && !stderr.includes('MISSING');
        }
        if (compilerOk) {
          console.log(`  [BUG!] ${name} — elm-syntax REJECTS valid Elm! Compiler accepts it.`);
        } else {
          console.log(`  [pass] ${name} — elm-format is overly permissive`);
        }
      } else if (!syntaxOk) {
        console.log(`  [skip] ${name} — both reject`);
      } else {
        console.log(`  [pass] ${name}`);
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
