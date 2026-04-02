// Systematically test every character that could precede `-` in valid Elm
// where `-` should be negation (not subtraction).
const w = require('./worker/worker.cjs');
const { execSync } = require('child_process');
const app = w.Elm.ParseWorker.init();

const tests = [
  // Already handled: space, (, ), }, comma, [, =
  // What about these?

  // After pipe operator |>
  ["after-pipe-nospace", "module T exposing (..)\nx = y|> -1"],   // unlikely but test it

  // After various keywords without space before -
  // These probably always have space, but let's check edge cases

  // After closing bracket ]
  ["after-rbracket", "module T exposing (..)\nx = case [] of\n        _-> -1"],

  // The real question: what contexts put a non-space char right before `-`?
  // Let's systematically generate `<char>-1` for every printable ASCII char
  // and check if the compiler accepts it in some expression context
];

// Generate tests for every expression context where char-1 could appear
const contextTests = [
  // Record field: {<name>=<expr>} — = before -
  ["rec-eq", "{a=-1}", true],
  // List after comma: [1,<expr>] — , before -
  ["list-comma", "[1,-1]", true],
  // Tuple after comma: (1,<expr>) — , before -
  ["tuple-comma", "(1,-1)", true],
  // After opening paren: (<expr>) — ( before -
  ["paren", "(-1)", true],
  // After opening bracket: [<expr>] — [ before -
  ["bracket", "[-1]", true],
  // After closing paren in application: f(x)-1 — ) before - (this is subtraction!)
  ["after-close-paren", "f (x)-1", false],  // should be subtraction

  // After arrow in lambda: \x-><expr> — no, arrow needs space
  // After pipe: x|>negate — the | before > before -, too far

  // What about record update? { r | field=-1 }
  ["rec-update-eq", "{ r | field=-1 }", true],

  // What about in case branch: _ -><expr>
  // The > before - ... but there's always a space after ->

  // Multiline: what about after a newline+indent?
  // The \n doesn't match, but the indent spaces do

  // After semicolons or other unusual characters?
  // Elm doesn't use semicolons

  // What about after type annotation colon? x:-1 ... not valid Elm expression context

  // Key question: are there other operator-like characters that can appear
  // right before - without space?

  // After :: (cons) with no space: a::-1
  ["after-cons-nospace", "a :: -1", true],  // space version
  ["after-cons-nospace2", "case x of\n        a::-1 -> 0\n        _->0", false], // not valid

  // Record update pipe: {r|-1} ... doesn't make sense

  // What about in let: let\n    x=-1\n in x
  ["let-eq-nospace", "let\n    x=-1\n in\n x", true],
];

function testOne(name, exprSource, expectParse) {
  const source = `module T exposing (..)\nx = ${exprSource}`;
  return new Promise(resolve => {
    function handler(data) {
      app.ports.parseResult.unsubscribe(handler);
      const syntaxOk = data.parsed;

      // Check with compiler
      let compilerOk = false;
      try {
        const fs = require('fs');
        fs.writeFileSync('/tmp/elm-neg-test/src/Main.elm', `module Main exposing (..)\nx = ${exprSource}\n`);
        execSync('elm make src/Main.elm --output=/dev/null', {
          cwd: '/tmp/elm-neg-test', encoding: 'utf-8', timeout: 10000, stdio: ['pipe', 'pipe', 'pipe']
        });
        compilerOk = true;
      } catch (e) {
        const stderr = (e.stderr || '').toString();
        compilerOk = !stderr.includes('UNEXPECTED') && !stderr.includes('UNFINISHED')
          && !stderr.includes('MISSING') && !stderr.includes('PROBLEM IN DEFINITION')
          && !stderr.includes('WEIRD DECLARATION');
      }

      if (syntaxOk === compilerOk) {
        console.log(`  [pass] ${name} — both ${syntaxOk ? 'accept' : 'reject'}`);
      } else if (syntaxOk && !compilerOk) {
        console.log(`  [BUG?] ${name} — elm-syntax accepts, compiler rejects — source: ${exprSource}`);
      } else {
        console.log(`  [BUG!] ${name} — elm-syntax REJECTS, compiler accepts — source: ${exprSource}`);
      }
      resolve();
    }
    app.ports.parseResult.subscribe(handler);
    app.ports.requestParsing.send(source);
  });
}

(async () => {
  for (const [name, source, _] of contextTests) {
    await testOne(name, source);
  }

  // Also test some more exotic contexts
  const exoticTests = [
    // Negation right after keywords (no space)
    ["if-neg", "if True then -1 else 0"],
    ["then-neg-nospace", "if True then-1 else 0"],
    ["else-neg-nospace", "if True then 0 else-1"],
    ["in-neg-nospace", "let x = 1 in-1"],
    ["of-neg-nospace", "case x of\n        _-> -1"],

    // Negation after various brackets/parens with no space
    ["after-close-bracket-neg", "let x = [1] in -1"],
    ["after-close-curly-neg", "let x = {} in -1"],

    // Multiple negations in record
    ["record-multi-neg", "{a=-1,b=-2,c=-3}"],
    ["record-neg-float", "{a=-1.5}"],
    ["record-neg-hex", "{a=-0xFF}"],

    // Nested records with negation
    ["nested-record-neg", "{a={b=-1}}"],

    // Record update with negation
    ["record-update-neg", "{ r | a=-1, b=-2 }"],
  ];

  for (const [name, source] of exoticTests) {
    await testOne(name, `${source}`);
  }
})();
