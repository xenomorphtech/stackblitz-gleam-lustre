import fs from 'fs';
import * as gleamWasm from './gleam_wasm_wc_mangled.mjs';

gleamWasm.init(false);

const result = gleamWasm.build("javascript");
console.log(result)

