open Js.Json;

type jsonValueType = 
  | String
  | Float
  | Int
  | Bool
  | Object(list((string, jsonValueType)))  /* List of key-type pairs */
  | Array(jsonValueType)
  | NonNull(jsonValueType)
  | Any /* Null values can be anything before unification */
  | Impossible
;

let nullable = fun 
  | NonNull(x) => x
  | y          => y
;

let joinWith = (sep: string, strs: list(string)) =>
switch strs {
| [] => ""
| [x, ...xs] => List.fold_left((a, b) => a ++ (sep ++ b), x, xs)
};

let rec unifyObjectFields(keyValuePairsA, keyValuePairsB) = {
  let rec aux(pairsA, pairsB) = switch(pairsA, pairsB) {
    | ([], rest) | (rest, []) => rest |> List.map(((k, t)) => (k, nullable(t)))
    | ([(keyA, typeA), ...xs], [(keyB, _), ..._]) when keyA < keyB => [(keyA, nullable(typeA)), ...aux(xs, pairsB)]
    | ([(keyA, _), ..._], [(keyB, typeB), ...ys]) when keyA > keyB => [(keyB, nullable(typeB)), ...aux(pairsA, ys)]
    | ([(keyA, typeA), ...xs], [(keyB, typeB), ...ys]) when keyA == keyB => [(keyA, unify(typeA, typeB)), ...aux(xs, ys)]
    | _ => failwith("Should match one of the above!")
  };
  let pairsA = keyValuePairsA |> List.sort(((x, _), (y, _)) => String.compare(x, y));
  let pairsB = keyValuePairsB |> List.sort(((x, _), (y, _)) => String.compare(x, y));
  Object(aux(pairsA, pairsB))
}
and unify(typeA, typeB) = switch(typeA, typeB) {
    | (Impossible, _) | (_, Impossible)     => Impossible
    | (Any, NonNull(x)) | (NonNull(x), Any) => x
    | (Any, x) | (x, Any)                   => x
    | (Array(x), Array(y))                  => Array(unify(x, y))
    | (NonNull(x), NonNull(y))              => NonNull(unify(x, y))
    | (NonNull(x), y) | (x, NonNull(y))     => unify(x, y)
    | (x, y) when x == y                    => x
    | (Int, Float) | (Float, Int)           => Float
    | (Object(pairsA), Object(pairsB))      => unifyObjectFields(pairsA, pairsB)
    | _ => Impossible
};

let (%) = (f, g) => (x) => f(g(x));
let rec deriveType(json) = switch json {
    | JSONString(_) => NonNull(String)
    | JSONNumber(num) => NonNull(if (floor(num) == num) {Int} else {Float})
    | JSONNull => Any
    | JSONFalse | JSONTrue => NonNull(Bool)
    | JSONArray(arr) => arr |> Array.map(deriveType % classify) 
                            |> Array.fold_left(unify, Any)
                            |> (x) => NonNull(Array(x))
    | JSONObject(dict) => {
        dict |> Js.Dict.entries 
             |> Array.map(((k, v)) => (k, v |> classify |> deriveType))
             |> Array.to_list 
             |> (x) => NonNull(Object(x))
    } 
};

let pascalCase(s) = {
  Js.String.split("_", s) |> Array.to_list |> List.map(String.capitalize) |> joinWith("")
};

let camelCase(s) = {
  Js.String.split("_", s) 
  |> Array.to_list 
  |> (s) => switch(s) {
    | [x, ...xs] => [x, ...List.map(String.capitalize, xs)]
    | [] => []
    } 
  |> joinWith("")
};

let mapToGraphQLSchema(jst) = {
  let typeMap = ref(GraphQL.TypeMap.empty);
  let keyValMap = ref(Hashtbl.create(10));

  let rec buildObject(name, keyVals) {
    if (Hashtbl.mem(keyValMap^, keyVals)) {
      Hashtbl.find(keyValMap^, keyVals)
    } else {
      let newObj = GraphQL.Object({
        name: name,
        description: None,
        fields: keyVals 
                |> List.map(((key, value)) => GraphQL.({
                    name: camelCase(key), 
                    description: None, 
                    output_type: aux(value, buildObject(pascalCase(key) ++ "Object")),
                    args: [],
                    deprecated: NotDeprecated,
                })),
        interfaces: [],
      });
      Hashtbl.add(keyValMap^, keyVals, newObj);
      typeMap := GraphQL.TypeMap.add(name, newObj, typeMap^);
      newObj
    }
  }
  and aux(jst, objectBuilder) = {
    switch(jst) {
      | String          => GraphQL.gqlString
      | Float           => GraphQL.gqlFloat
      | Int             => GraphQL.gqlInt
      | Bool            => GraphQL.gqlBoolean
      | Object(keyVals) => objectBuilder(keyVals)
      | Array(t)        => GraphQL.ListType(aux(t, objectBuilder))
      | NonNull(t)      => GraphQL.NonNull(aux(t, objectBuilder))
      | Any             => GraphQL.gqlString
      | Impossible      => GraphQL.LazyType("CantUnifyTypes")
    }
  };
  GraphQL.{
      query: aux(jst, buildObject("QueryObject")) |> GraphQL.baseType |> t => GraphQL.NonNull(t), 
      mutation: None, 
      types: typeMap^,
  }
};