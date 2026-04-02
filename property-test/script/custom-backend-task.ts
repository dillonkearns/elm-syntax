import { execSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { join } from "node:path";

// Load the compiled Elm worker (CommonJS output from elm make)
import { createRequire } from "node:module";
const require = createRequire(import.meta.url);
const workerPath = join(process.cwd(), "worker", "worker.cjs");
const worker = require(workerPath);

// Initialize the worker app once
const workerApp = worker.Elm.ParseWorker.init();

/**
 * Parse Elm source with the local elm-syntax parser (via compiled Elm worker).
 * Returns the elm-syntax JSON AST or a parse error.
 */
export async function parseWithElmSyntax(
  source: string
): Promise<Record<string, unknown>> {
  return new Promise((resolve) => {
    function handler(data: Record<string, unknown>) {
      workerApp.ports.parseResult.unsubscribe(handler);
      resolve(data);
    }
    workerApp.ports.parseResult.subscribe(handler);
    workerApp.ports.requestParsing.send(source);
  });
}

/**
 * Parse Elm source with elm-format --json (uses the real Elm compiler parser).
 * Returns the elm-format JSON AST or null if elm-format fails.
 */
export async function parseWithElmFormat(
  source: string
): Promise<Record<string, unknown> | null> {
  try {
    const result = execSync("npx elm-format --stdin --json", {
      input: source,
      encoding: "utf-8",
      timeout: 10000,
      cwd: process.cwd(),
    });
    return JSON.parse(result);
  } catch {
    return null;
  }
}

/**
 * Compile an Elm file with `elm make` to check if it's valid Elm.
 * Returns { success: true } or { success: false, error: string }.
 */
export async function compileWithElm(
  source: string,
  moduleName: string
): Promise<{ success: boolean; error?: string }> {
  const projectDir = join(process.cwd(), "generated", "elm-project");
  const srcDir = join(projectDir, "src");
  mkdirSync(srcDir, { recursive: true });

  // Ensure generated project has an elm.json
  const elmJsonPath = join(projectDir, "elm.json");
  try {
    readFileSync(elmJsonPath);
  } catch {
    writeFileSync(
      elmJsonPath,
      JSON.stringify(
        {
          type: "application",
          "source-directories": ["src"],
          "elm-version": "0.19.1",
          dependencies: {
            direct: {
              "elm/core": "1.0.5",
              "elm/json": "1.1.3",
            },
            indirect: {},
          },
          "test-dependencies": { direct: {}, indirect: {} },
        },
        null,
        4
      )
    );
  }

  const filePath = join(srcDir, moduleName + ".elm");
  writeFileSync(filePath, source);

  try {
    execSync(`elm make src/${moduleName}.elm --output=/dev/null`, {
      cwd: projectDir,
      encoding: "utf-8",
      timeout: 30000,
      stdio: "pipe",
    });
    return { success: true };
  } catch (e: any) {
    return { success: false, error: e.stderr || e.message };
  }
}
