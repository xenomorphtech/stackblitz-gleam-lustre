import fs from 'fs';
import * as fspath from 'path';
import { cwd } from 'process';

console.log(`Current directory: ${cwd()}`);

export function fs_is_directory(path) {
  const ret     = fs.existsSync(path) && fs.lstatSync(path).isDirectory()
//  console.log("is_directory(",path,") = ", ret);
  return ret;
}

export function fs_is_file(path) {
  const ret     = fs.existsSync(path) && fs.lstatSync(path).isFile()
 // console.log("is_file(",path,") returning", ret);
  return ret;
}

export function fs_delete(path) {
  console.log("delete(",path,")");
  if (fs.existsSync(path)) { 
//    fs.rmSync(path, { recursive: true, force: true });
  }
}

export function fs_delete_file(path) {
  console.log("delete_file(",path,")");
  if (fs.existsSync(path)) { 
//    fs.rmSync(path, { recursive: true, force: true });
  }
}

export function fs_mkdir(path) {
//  console.log("mkdir(",path,")");
  fs.mkdirSync(path, { recursive: true });
}

export function fs_write(path, contents) {
//  console.log("write(",path,")");
  fs.writeFileSync(path, Buffer.from(contents));
}

export function fs_read(path) {
  const ret = fs.readFileSync(path, {flag: 'r'});
//  console.log("read(",path,") = ", ret);
  return new Uint8Array(ret);
}

export function fs_read_dir(path) {
  const route = fspath.resolve(process.cwd(), path);
  const ret = fs.readdirSync( path ).map(x =>  path + '/' + x);
//  console.log("read_dir(",path,") = ", ret);
  return ret;
}

export function fs_copy(from, to) {
//  console.log("fs_copy(",from, ",", to,")");
  fs.copyFileSync(from, to);
}

export function fs_copy_dir(from, to) {
  console.log("fs_copy_dir(",from, ",", to,")");
}


