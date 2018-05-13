"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.consoleLog = function (x) { return function () { return console.log(x); }; };
exports.jsonStringify = function (x) { return JSON.stringify(x, null, 2); };
