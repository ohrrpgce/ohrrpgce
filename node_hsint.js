#!/usr/bin/env node
// This commandline node.js program can run a HamsterSpeak script
// written using the hamsterspeak.hsd subset of Plotscripting.
// It uses zzo38's hspeakrt javascript implementation of a HamsterSpeak
// interpreter, available from npm:
// https://www.npmjs.com/package/hspeakrt
//
// E.g. to run the HS testcases:
//   ./hspeak -b testgame/hstests_standalone.hss
//   ./node_hsint.js testgame/hstests_standalone.hs
//
// Not all commands in hamsterspeak.hsd are implemented yet. Other commands
// are ignored with a warning.

"use strict";

const fs = require("fs");
const path = require('path');
const hspeakrt = require("hspeakrt");

let commands = {
    0: function noop(stab) {
    },
    1: function wait(stab, ticks) {
    },
    73: function gameover(stab) {
        process.exit();
    },
    114: function readglobal(stab, index) {
        return interpreter.global[index];
    },
    115: function writeglobal(stab, index, val) {
        interpreter.global[index] = val;
    },
    176: function runscriptbyid(stab, id, ...args) {
        return runscript(interpreter.call(id, ...args));
    },
    210: function showstring(stab, id) {
        console.log(interpreter.plotstring[id]);
    },
    211: function clearstring(stab, id) {
        interpreter.plotstring[id] = "";
    },
    212: function appendascii(stab, id, chr) {
        interpreter.plotstring[id] += String.fromCharCode(chr);
    },
    213: function appendnumber(stab, id, val) {
        interpreter.plotstring[id] += val;
    },
    214: function copystring(stab, destid, srcid) {
        interpreter.plotstring[destid] = interpreter.plotstring[srcid];
    },
    215: function concatenatestrings(stab, destid, srcid) {
        interpreter.plotstring[destid] += interpreter.plotstring[srcid];
    },
    216: function stringlength(stab, id) {
        return interpreter.plotstring[id].length;
    },
    219: function asciifromstring(stab, id, pos) {
        return interpreter.plotstring[id].charCodeAt(pos - 1);
    },
    229: function stringcompare(stab, id1, id2) {
        return interpreter.plotstring[id1] == interpreter.plotstring[id2];
    },
    232: function trace(stab, id) {
        console.log(interpreter.plotstring[id]);
    },
    251: function setstringfromtable(stab, id, offset) {
        interpreter.plotstring[id] = hspeakrt.getString(stab, offset);
        return id;
    },
    252: function appendstringfromtable(stab, id, offset) {
        interpreter.plotstring[id] += hspeakrt.getString(stab, offset);
        return id;
    },
    273: function milliseconds(stab) {
        return new Date().getTime()|0;
    },
    466: function tracevalueinternal(stab, ...args) {
        if (args.length % 2)
            throw new Error("Miscompiled tracevalue: odd number of args");
        let line = "";
        for (let idx = 0; idx < args.length; idx += 2) {
            if (idx > 0)
                line += ", ";
            line += hspeakrt.getString(stab, args[idx]) + " = " + args[idx + 1];
        }
        console.log(line);
    },
    542: function microseconds(stab) {
        // node.js specific
        let [secs, ns] = process.hrtime();
        // I hope this will avoid loss of least significant bits
        // if secs is large, but haven't checked.
        return ( ((secs*1000)|0) * 1000 + ns/1000)|0;
    },
    565: function stringsprintf(stab, destid, formatid, ...args) {
        let argidx = -1;
        interpreter.plotstring[destid] =
            interpreter.plotstring[formatid]
            .replace(/%([%sdcx])/g, function(match, code) { 
                //console.log("MATCH", argidx, match, code);
                switch (code) {
                    case "%":  return "%";
                    case "d":  return args[++argidx].toString();
                    case "x":  {
                        let temp = args[++argidx];
                        if (temp < 0) temp += 0x100000000;
                        return temp.toString(16);
                    }
                    case "s":  return interpreter.plotstring[args[++argidx]];
                    case "c":  return String.fromCharCode(args[++argidx]);
                };
            });
    },
    566: function scripterror(stab, id) {
        throw new Error("Script threw error: " + interpreter.plotstring[id]);
    },
    649: function multdiv(stab, a, b, c) {
        let ret = (a * b) / c;
        if (ret < -2147483648)
            return -2147483648;
        else if (ret > 2147483647)
            return 2147483647;
        // Math.round breaks ties by rounding up, which differs from FB/x86's rounding to even
        return Math.round(ret);
    }
};

// Execute a script instance returned by interpreter.call()
function runscript(script) {
    let res = script.next();
    while (true) {
        if (res.done)
            return res.value;
        let cmdid = res.value[0];
        if (!(cmdid in commands)) {
            //throw new RangeError("Command " + cmdid + " not implemented")
            console.error("Command", cmdid, commandNames[cmdid], "not implemented");
            // Ignore future errors by mapping to noop
            commands[cmdid] = commands[0];
        }
        //console.log("Command", cmdid, commands[cmdid].name);
        let scriptret = commands[cmdid](...res.value.slice(1));
        res = script.next(scriptret);
    }
}

function benchmark_script(scrname) {
    for (let i = 0; i < 10; i++) {
        console.time(scrname);
        runscript(interpreter.call(scrname));
        console.timeEnd(scrname);
    }
}

if (process.argv.length != 3) {
    console.error("  Usage: " + path.basename(process.argv[1]) + " scripts.hs\n" +
                  "Given a compiled HamsterSpeak scripts.hs file constrained to the\n" +
                  "hamsterspeak.hsd subset of HS, executes the script named 'main'.");
    process.exit(1);
} else {

    var interpreter = new hspeakrt(process.argv[2], 8192);
    var commandNames = interpreter.commandNames() || [];

    interpreter.plotstring = []
    for (let idx = 0; idx < 100; idx++)
        interpreter.plotstring.push("");

    //console.log(interpreter.list()) // List all scripts in the .hs file

    let main = interpreter.call("main")
    if (!main)
        throw new Error("Couldn't load main script");
    process.exit(runscript(main));
}
