import fs from 'fs';
import * as gleamWasm from './pkg/gleam_wasm_wc_mangled.mjs';

gleamWasm.init(false);

const gleam_input = 'fn main() { todo }';

const sourceFiles = Object.assign(
  {
    './src/main.gleam': gleam_input,
    'build/packages/gleam_stdlib/gleam.toml': 'name = "gleam_stdlib"',
  },
  {}
);

const result = gleamWasm.build("javascript");
console.log(result)

//var ok = files['Ok'];
//console.log(ok);
//console.log(Array.from(ok.keys()));
//var g = ok.get('build/dev/javascript/gleam-wasm/dist/main.mjs');
//console.log(g);

