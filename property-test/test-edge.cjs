const w = require('./worker/worker.cjs');
const { execSync } = require('child_process');
const app = w.Elm.ParseWorker.init();

const tests = [
  // Operator at start of continuation line
  ["op-continuation", "module T exposing (..)\nx =\n    1\n    + 2"],
  // Negative number at start of continuation
  ["neg-continuation", "module T exposing (..)\nx =\n    1\n    -2"],
  // Multiline record update
  ["record-update-multiline", "module T exposing (..)\nx =\n    { a\n        | b = 1\n    }"],
  // Nested case
  ["nested-case", "module T exposing (..)\nx =\n    case y of\n        Just z ->\n            case z of\n                True -> 1\n                False -> 2\n        Nothing -> 0"],
  // Operator section
  ["neg-then-app", "module T exposing (..)\nx = negate -1"],
  // Tricky: negative in tuple after comma
  ["neg-after-comma-tuple", "module T exposing (..)\nx = (1,-2,3)"],
  // Tricky: negation of function call
  ["neg-func-call", "module T exposing (..)\nx = -(f 1)"],
  // Nested lambda
  ["nested-lambda-pattern", "module T exposing (..)\nx = \\(a,b) (c,d) -> a"],
  // Multiline lambda
  ["multiline-lambda", "module T exposing (..)\nx =\n    \\a ->\n        a + 1"],
  // Record access on complex expression
  ["record-access-complex", "module T exposing (..)\nx = (f a).b"],
  // Chained record access
  ["record-access-deep", "module T exposing (..)\nx = a.b.c.d"],
  // Empty record
  ["empty-record-expr", "module T exposing (..)\nx = {}"],
  // Hex in various positions
  ["hex-in-ops", "module T exposing (..)\nx = 0xFF + 0x10"],
  // Negative hex
  ["neg-hex", "module T exposing (..)\nx = -0xFF"],
  // Char with special escapes
  ["char-newline", "module T exposing (..)\nx = '\\n'"],
  ["char-tab", "module T exposing (..)\nx = '\\t'"],
  ["char-carriage-return", "module T exposing (..)\nx = '\\r'"],
  // Triple quoted string edge cases
  ["triple-string-with-newlines", 'module T exposing (..)\nx = """line1\nline2\nline3"""'],
  // Qualified references
  ["qualified-ref", "module T exposing (..)\nx = List.map"],
  ["qualified-type", "module T exposing (..)\ntype alias X = Dict.Dict String Int"],
  // Import edge cases
  ["import-exposing-all", "module T exposing (..)\nimport List exposing (..)\nx = 1"],
  ["import-exposing-specific", "module T exposing (..)\nimport List exposing (map, filter)\nx = 1"],
  ["import-as", "module T exposing (..)\nimport Dict as D\nx = 1"],
  // Type annotations with complex types
  ["type-ann-record", "module T exposing (..)\nx : { a : Int, b : String }\nx = { a = 1, b = \"\" }"],
  // Where negation is ambiguous
  ["neg-in-infix", "module T exposing (..)\nx = 1 + -2"],
  ["neg-in-comparison", "module T exposing (..)\nx = a > -1"],
  // Comments
  ["line-comment", "module T exposing (..)\n-- comment\nx = 1"],
  ["block-comment", "module T exposing (..)\n{- comment -}\nx = 1"],
  ["nested-block-comment", "module T exposing (..)\n{- {- nested -} -}\nx = 1"],
];

let pending = tests.length;

function testOne(name, source) {
  return new Promise(resolve => {
    // Test elm-syntax
    function handler(data) {
      app.ports.parseResult.unsubscribe(handler);
      const syntaxOk = data.parsed;

      // Test elm-format
      let formatOk = false;
      try {
        const result = execSync('npx elm-format --stdin --json', {
          input: source, encoding: 'utf-8', timeout: 10000, stdio: ['pipe', 'pipe', 'pipe']
        });
        formatOk = result.includes('moduleName');
      } catch {}

      if (syntaxOk !== formatOk) {
        console.log(`  [DISAGREE] ${name} — syntax:${syntaxOk} format:${formatOk}`);
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
