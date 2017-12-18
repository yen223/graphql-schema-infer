open GraphQL;
open List;

let format = Printf.sprintf;
let joinWith = (sep: string, strs: list(string)) =>
switch strs {
| [] => ""
| [x, ...xs] => List.fold_left((a, b) => a ++ (sep ++ b), x, xs)
};

let indent = (++)("  ");

let surround = (s, t, str) => s ++ (str ++ t);

let unlines = joinWith("\n");

let (<<) = (f, g, x) => f(g(x));

let strKeyval = (key, value) => format("%s: %s", key, value);

let block = (header, blockLines) => {
  let inner = List.map(indent, blockLines);
  header ++ (" {\n" ++ (unlines(inner) ++ "\n}"))
};

let printEnumValue = (EnumValue(name, _, _)) => name;

let printDeprecated = fun
  | IsDeprecated(reason) => "@deprecated(reason: \"" ++ (reason ++ "\")")
  | NotDeprecated => "";

let rec printType = fun
  | Scalar({name}) => "scalar " ++ name
  | Object({name, fields, interfaces}) => {
      let printInterfaces = (interfaces) =>
        switch interfaces {
        | [] => ""
        | _ => " implements " ++ joinWith(", ", List.map(printType, interfaces))
        };
      let header = "type " ++ (name ++ printInterfaces(interfaces));
      let blockLines = map(printField, fields) |> sort(String.compare);
      block(header, blockLines)
    }
  | Interface({name, fields}) => {
      let header = "interface " ++ name;
      let blockLines = map(printField, fields) |> sort(String.compare);
      block(header, blockLines)
    }
  | Union({name, possibleTypes}) =>
    "union " ++ name ++ " = " ++ (possibleTypes |> map(typeName) |> joinWith(" | "))
  | Enum({name, enumValues}) => {
      let header = "enum " ++ name;
      block(header, List.map(printEnumValue, enumValues))
    }
  | InputObject({name, inputValueTypes}) => {
      let header = "input " ++ name;
      let blockLines = map(printInputValue, inputValueTypes) |> sort(String.compare);
      block(header, blockLines)
    }
  | ListType(typ) => "[" ++ (printType(typ) ++ "]")
  | NonNull(typ) => printType(typ) ++ "!"
  | LazyType(name) => name
and printField = (f: field) => {
  let {name, args, output_type, deprecated} = f;
  let printArgs = (args) =>
    switch args {
    | [] => ""
    | _ =>
      List.map(printInputValue, args)
      |> List.sort(String.compare)
      |> joinWith(", ")
      |> surround("(", ")")
    };
  format("%s%s: %s %s", 
    name, 
    printArgs(args), 
    typeLabel(output_type),
    printDeprecated(deprecated)
  )
}
and printInputValue = (inp) => {
  let {name, graphqlType} = inp;
  name ++ (": " ++ printType(graphqlType))
};

let printSchema({query, types}) = {
  let schema = block("schema", ["query: " ++ typeLabel(query)]);
  let body = TypeMap.bindings(types)
  |> List.map(snd)
  |> List.map(printType)
  |> List.filter(x => x != "")
  |> joinWith("\n\n");
  schema ++ "\n\n" ++ body
};