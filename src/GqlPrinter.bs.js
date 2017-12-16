// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List                  = require("bs-platform/lib/js/list.js");
var Block                 = require("bs-platform/lib/js/block.js");
var Curry                 = require("bs-platform/lib/js/curry.js");
var Printf                = require("bs-platform/lib/js/printf.js");
var $$String              = require("bs-platform/lib/js/string.js");
var Pervasives            = require("bs-platform/lib/js/pervasives.js");
var GraphQL$ReactTemplate = require("./GraphQL.bs.js");

function joinWith(sep, strs) {
  if (strs) {
    return List.fold_left((function (a, b) {
                  return a + (sep + b);
                }), strs[0], strs[1]);
  } else {
    return "";
  }
}

function indent(param) {
  return Pervasives.$caret("  ", param);
}

function surround(s, t, str) {
  return s + (str + t);
}

function unlines(param) {
  return joinWith("\n", param);
}

function $less$less(f, g, x) {
  return Curry._1(f, Curry._1(g, x));
}

function strKeyval(key, value) {
  return Curry._2(Printf.sprintf(/* Format */[
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* String_literal */Block.__(11, [
                          ": ",
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "%s: %s"
                ]), key, value);
}

function block(header, blockLines) {
  var inner = List.map(indent, blockLines);
  return header + (" {\n" + (unlines(inner) + "\n}"));
}

function printEnumValue(param) {
  return param[0];
}

function printDeprecated(param) {
  if (param) {
    return "@deprecated(reason: \"" + (param[0] + "\")");
  } else {
    return "";
  }
}

function printType(param) {
  switch (param.tag | 0) {
    case 0 : 
        return "scalar " + param[0][/* name */0];
    case 1 : 
        var match = param[0];
        var printInterfaces = function (interfaces) {
          if (interfaces) {
            return " implements " + joinWith(", ", List.map(printType, interfaces));
          } else {
            return "";
          }
        };
        var header = "type " + (match[/* name */0] + printInterfaces(match[/* interfaces */3]));
        var blockLines = List.sort($$String.compare, List.map(printField, match[/* fields */2]));
        return block(header, blockLines);
    case 2 : 
        var match$1 = param[0];
        var header$1 = "interface " + match$1[/* name */0];
        var blockLines$1 = List.sort($$String.compare, List.map(printField, match$1[/* fields */2]));
        return block(header$1, blockLines$1);
    case 3 : 
        var match$2 = param[0];
        return "union " + (match$2[/* name */0] + (" = " + joinWith(" | ", List.map(printType, match$2[/* possibleTypes */2]))));
    case 4 : 
        var match$3 = param[0];
        var header$2 = "enum " + match$3[/* name */0];
        return block(header$2, List.map(printEnumValue, match$3[/* enumValues */2]));
    case 5 : 
        var match$4 = param[0];
        var header$3 = "input " + match$4[/* name */0];
        var blockLines$2 = List.sort($$String.compare, List.map(printInputValue, match$4[/* inputValueTypes */2]));
        return block(header$3, blockLines$2);
    case 6 : 
        return "[" + (printType(param[0]) + "]");
    case 7 : 
        return printType(param[0]) + "!";
    case 8 : 
        return param[0];
    
  }
}

function printField(f) {
  var printArgs = function (args) {
    if (args) {
      return surround("(", ")", joinWith(", ", List.sort($$String.compare, List.map(printInputValue, args))));
    } else {
      return "";
    }
  };
  return Curry._4(Printf.sprintf(/* Format */[
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* String_literal */Block.__(11, [
                              ": ",
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* Char_literal */Block.__(12, [
                                      /* " " */32,
                                      /* String */Block.__(2, [
                                          /* No_padding */0,
                                          /* End_of_format */0
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ]),
                  "%s%s: %s %s"
                ]), f[/* name */0], printArgs(f[/* args */2]), GraphQL$ReactTemplate.typeLabel(f[/* output_type */3]), printDeprecated(f[/* deprecated */4]));
}

function printInputValue(inp) {
  return inp[/* name */0] + (": " + printType(inp[/* graphqlType */3]));
}

function printSchema(param) {
  var schema = block("schema", /* :: */[
        "query: " + GraphQL$ReactTemplate.typeName(param[/* query */0]),
        /* [] */0
      ]);
  var body = joinWith("\n\n", List.filter((function (x) {
                return +(x !== "");
              }))(List.map(printType, List.map((function (prim) {
                      return prim[1];
                    }), Curry._1(GraphQL$ReactTemplate.TypeMap[/* bindings */16], param[/* types */2])))));
  return schema + ("\n\n" + body);
}

var format = Printf.sprintf;

exports.format          = format;
exports.joinWith        = joinWith;
exports.indent          = indent;
exports.surround        = surround;
exports.unlines         = unlines;
exports.$less$less      = $less$less;
exports.strKeyval       = strKeyval;
exports.block           = block;
exports.printEnumValue  = printEnumValue;
exports.printDeprecated = printDeprecated;
exports.printType       = printType;
exports.printField      = printField;
exports.printInputValue = printInputValue;
exports.printSchema     = printSchema;
/* GraphQL-ReactTemplate Not a pure module */