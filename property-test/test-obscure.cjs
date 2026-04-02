const w = require('./worker/worker.cjs');
const { execSync } = require('child_process');
const app = w.Elm.ParseWorker.init();

const tests = [
  // === WHITESPACE / INDENTATION EDGE CASES ===
  // Tab characters (Elm requires spaces, but what happens?)
  ["tab-indent", "module T exposing (..)\nx =\n\t1"],
  // No trailing newline
  ["no-trailing-newline", "module T exposing (..)\nx = 1"],
  // Multiple trailing newlines
  ["multi-trailing-newline", "module T exposing (..)\nx = 1\n\n\n"],
  // CR+LF line endings
  ["crlf", "module T exposing (..)\r\nx = 1\r\n"],
  // Expression at column 1 (same as declaration start)
  ["col1-expr", "module T exposing (..)\nx =\n1"],

  // === DEEPLY NESTED STRUCTURES ===
  ["deep-parens", "module T exposing (..)\nx = ((((((1))))))"],
  ["deep-list", "module T exposing (..)\nx = [[[[1]]]]"],
  ["deep-tuple", "module T exposing (..)\nx = (((1, 2), 3), 4)"],
  ["deep-if", "module T exposing (..)\nx = if True then if False then 1 else 2 else 3"],
  ["deep-func-type", "module T exposing (..)\nf : Int -> Int -> Int -> Int -> Int\nf a b c d = a"],

  // === OPERATOR PRECEDENCE EDGE CASES ===
  ["mixed-bool-ops", "module T exposing (..)\nx = a && b || c && d"],
  ["compare-with-bool", "module T exposing (..)\nx = a == True && b == False"],
  ["pipe-and-apply", "module T exposing (..)\nx = f <| g <| h a"],
  ["compose-and-apply", "module T exposing (..)\nx = (f >> g) a"],
  ["cons-and-append", "module T exposing (..)\nx = a :: b ++ c"],

  // === RECORD EDGE CASES ===
  ["record-single-field", "module T exposing (..)\nx = { a = 1 }"],
  ["record-trailing-comma", "module T exposing (..)\nx = { a = 1, }"],
  ["record-update-same-field", "module T exposing (..)\nx = { r | a = 1, a = 2 }"],
  ["record-access-after-call", "module T exposing (..)\nx = (f a).b"],
  ["record-access-after-if", "module T exposing (..)\nx = (if True then a else b).c"],

  // === UNICODE EDGE CASES ===
  ["unicode-string", 'module T exposing (..)\nx = "café"'],
  ["unicode-identifier", "module T exposing (..)\nx = café"],
  ["unicode-escape-max", 'module T exposing (..)\nx = "\\u{10FFFF}"'],
  ["unicode-escape-zero", 'module T exposing (..)\nx = "\\u{0}"'],

  // === COMMENT EDGE CASES ===
  ["comment-in-expr", "module T exposing (..)\nx = 1 {- comment -} + 2"],
  ["comment-after-module", "module T {- hi -} exposing (..)"],
  ["doc-comment", "module T exposing (..)\n{-| Doc comment -}\nx = 1"],
  ["comment-in-list", "module T exposing (..)\nx = [ 1 {- a -} , 2 ]"],

  // === IMPORT EDGE CASES ===
  ["import-dotted", "module T exposing (..)\nimport Html.Attributes as Attr\nx = 1"],
  ["import-type-exposing", "module T exposing (..)\nimport Maybe exposing (Maybe(..))\nx = 1"],

  // === DECLARATION EDGE CASES ===
  ["infix-decl", "module T exposing (..)\ninfixl 6 (~~) = myOp"],
  ["type-no-params", "module T exposing (..)\ntype Foo = Bar"],
  ["type-many-params", "module T exposing (..)\ntype Foo a b c d = Bar a b c d"],
  ["type-alias-unit", "module T exposing (..)\ntype alias X = ()"],

  // === PATTERN EDGE CASES ===
  ["pattern-negative-int", "module T exposing (..)\nf x = case x of\n        -1 -> True\n        _ -> False"],
  ["pattern-hex", "module T exposing (..)\nf x = case x of\n        0xFF -> True\n        _ -> False"],
  ["pattern-qualified-ctor", "module T exposing (..)\nf x = case x of\n        Maybe.Just a -> a\n        Maybe.Nothing -> 0"],
  ["pattern-empty-record", "module T exposing (..)\nf {} = 1"],

  // === GLSL ===
  ["glsl-basic", "module T exposing (..)\nx = [glsl| void main() {} |]"],

  // === MULTILINE LET WITH COMPLEX BINDINGS ===
  ["let-func-pattern-arg", "module T exposing (..)\nx =\n    let\n        f (a, b) = a + b\n    in\n    f (1, 2)"],
  ["let-func-record-arg", "module T exposing (..)\nx =\n    let\n        f { a, b } = a + b\n    in\n    f { a = 1, b = 2 }"],
];

function testOne(name, source) {
  return new Promise(resolve => {
    function handler(data) {
      app.ports.parseResult.unsubscribe(handler);
      const syntaxOk = data.parsed;
      let formatOk = false;
      let formatCrash = false;
      try {
        const result = execSync('npx elm-format --stdin --json', {
          input: source, encoding: 'utf-8', timeout: 10000, stdio: ['pipe', 'pipe', 'pipe']
        });
        formatOk = result.includes('moduleName');
      } catch(e) {
        const stderr = (e.stderr||'').toString();
        if (stderr.includes('Non-exhaustive') || stderr.includes('elm-format:')) {
          formatCrash = true;
        }
      }

      if (formatCrash) {
        console.log(`  [skip] ${name} — elm-format crash`);
        resolve(); return;
      }

      if (syntaxOk && !formatOk) {
        let compilerAccepts = false;
        try {
          const fs = require('fs');
          fs.writeFileSync('/tmp/elm-neg-test/src/Main.elm', source.replace(/module \w+/,'module Main'));
          execSync('elm make src/Main.elm --output=/dev/null', {
            cwd: '/tmp/elm-neg-test', encoding: 'utf-8', timeout: 10000, stdio: ['pipe', 'pipe', 'pipe']
          });
          compilerAccepts = true;
        } catch (e) {
          const stderr = (e.stderr||'').toString();
          compilerAccepts = !stderr.includes('UNEXPECTED') && !stderr.includes('UNFINISHED')
            && !stderr.includes('MISSING') && !stderr.includes('PROBLEM');
        }
        console.log(compilerAccepts
          ? `  [pass*] ${name} — elm-format rejects valid Elm`
          : `  [BUG?] ${name} — elm-syntax FALSE ACCEPT`);
      } else if (!syntaxOk && formatOk) {
        let compilerAccepts = false;
        try {
          const fs = require('fs');
          fs.writeFileSync('/tmp/elm-neg-test/src/Main.elm', source.replace(/module \w+/,'module Main'));
          execSync('elm make src/Main.elm --output=/dev/null', {
            cwd: '/tmp/elm-neg-test', encoding: 'utf-8', timeout: 10000, stdio: ['pipe', 'pipe', 'pipe']
          });
          compilerAccepts = true;
        } catch (e) {
          const stderr = (e.stderr||'').toString();
          compilerAccepts = !stderr.includes('UNEXPECTED') && !stderr.includes('UNFINISHED')
            && !stderr.includes('MISSING') && !stderr.includes('PROBLEM');
        }
        console.log(compilerAccepts
          ? `  [BUG!] ${name} — elm-syntax REJECTS valid Elm!`
          : `  [pass] ${name} — both reject (elm-format overly permissive)`);
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
