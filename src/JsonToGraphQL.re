open Js.Json;

type t = 
  | String
  | Float
  | Int
  | Bool
  | Object(list((string, t)))  /* List of key-type pairs */
  | Array(t)
  | NonNull(t)
  | PossibleTypes(list(t))
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

let optMap(fn, x) = switch(x) {
| Some(inner) => Some(fn(inner))
| None        => None
};

let (%) = (f, g) => (x) => f(g(x));
let rec deriveType(json) = switch json {
  | JSONString(_) => NonNull(String)
  | JSONNumber(num) => NonNull(if (floor(num) == num) {Int} else {Float})
  | JSONNull => Any
  | JSONFalse | JSONTrue => NonNull(Bool)
  | JSONArray(arr) => arr |> Array.map(deriveType % classify) 
                          |> Array.to_list 
                          |> (x) => NonNull(Array(PossibleTypes(x)))
  | JSONObject(dict) => {
      dict |> Js.Dict.entries 
           |> Array.map(((k, v)) => (k, v |> classify |> deriveType))
           |> Array.to_list 
           |> (x) => NonNull(Object(x))
  } 
};
let map = List.map;
let fold = List.fold_left;
let concat = List.concat;
  
let uniqueBy(fn, ls) = {
  let rec aux(ls, seen) = {
    switch(ls) {
    | [] => []
    | [x, ...xs] when List.mem(fn(x), seen) => aux(xs, seen)
    | [x, ...xs] => [x, ...aux(xs, [fn(x), ...seen])]
    }
  };
  aux(ls, [])
};

let rec flatten(types) = switch(types){
  | [] => []
  | [PossibleTypes(inner), ...ts] => concat([inner, ts]) |> flatten
  | [t, ...ts] => [t, ...flatten(ts)]
};

let mkPossibleTypes(types) = switch(types) {
  | [] => Any
  | [t] => t
  | _ => PossibleTypes(flatten(types))
};

let rec unify(typeA, typeB) = switch(typeA, typeB) {
  | (Impossible, _) | (_, Impossible)     => Impossible
  | (Any, NonNull(x)) | (NonNull(x), Any) => x
  | (Any, x) | (x, Any)                   => x
  | (Array(x), Array(y))                  => Array(unify(x, y))
  | (NonNull(x), NonNull(y))              => NonNull(unify(x, y))
  | (NonNull(x), y) | (x, NonNull(y))     => unify(x, y)
  | (x, y) when x == y                    => x
  | (Int, Float) | (Float, Int)           => Float
  | (PossibleTypes(typesA), PossibleTypes(typesB)) => mkPossibleTypes(flatten(concat([typesA, typesB])))
  | (PossibleTypes(ts), t) | (t, PossibleTypes(ts)) => unifyWithPossibleTypes(t, ts)
  | (Object(fieldsA), Object(fieldsB))    => unifyObjectFields(fieldsA, fieldsB)
  | (Object(_), _) | (_, Object(_))       => Impossible
  | _ => mkPossibleTypes([typeA, typeB])
}
and unifyWithPossibleTypes(typ, types) = {
  let rec inner(types) = switch(types) {
    | [] => []
    | [t, ...ts] => switch(unify(typ, t)){
      | Impossible => [Impossible]
      | PossibleTypes(_) => [t, ...inner(ts)]
      | unified => [unified, ...ts]
      }
  };
  mkPossibleTypes(inner(types))
} 
and unifyObjectFields(fieldsA, fieldsB) = {
  let rec aux(pairsA, pairsB) = switch(pairsA, pairsB) {
    | ([], []) => Some([])
    | ([], rest) | (rest, []) => None
    | ([(keyA, typeA), ...xs], [(keyB, typeB), ...ys]) when keyA != keyB => None
    | ([(keyA, typeA), ...xs], [(keyB, typeB), ...ys]) => 
      switch(unify(typeA, typeB)) {
        | Impossible => None
        | unified => aux(xs, ys) |> optMap((ls) => [(keyA, unified), ...ls])
      }
  };
  let pairsA = fieldsA |> List.sort(((x, _), (y, _)) => String.compare(x, y));
  let pairsB = fieldsB |> List.sort(((x, _), (y, _)) => String.compare(x, y));
  switch(aux(pairsA, pairsB)) {
  | Some([]) => Any
  | Some(fs) => Object(fs)
  | None => mkPossibleTypes([Object(fieldsA), Object(fieldsB)])
  }
};

let unifyTypes = fold(unify, Any);
let rec simplify(typ) = {
  let newType = switch(typ) {
    | String => typ
    | Float => typ
    | Int => typ
    | Bool => typ
    | Array(typ) => Array(simplify(typ))
    | NonNull(typ) => NonNull(simplify(typ))
    | Object(fs) => Object(fs |> map(((name, t)) => (name, simplify(t))))
    | PossibleTypes(types) => unifyTypes(types |> map(simplify))
    | Any => typ
    | Impossible => typ
  };
  if (newType == typ) {newType} else {simplify(newType)}
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
  let getName(name) = {
    let rec suffix(name, i) = {
      if (GraphQL.TypeMap.mem(name, typeMap^)) {
        suffix(name ++ string_of_int(i+1), i+1)
      } else {
        i
      }
    };
    switch(suffix(name, 1)) {
      | 1 => name
      | i => name ++ "_" ++ string_of_int(i)
    }
  };

  let rec buildObject(name, keyVals) = {
    if (Hashtbl.mem(keyValMap^, keyVals)) {
      Hashtbl.find(keyValMap^, keyVals)
    } else {
      let fields = keyVals 
      |> map(((key, value)) => GraphQL.({
        name: camelCase(key), 
        description: None, 
        output_type: aux(value, pascalCase(key)),
        args: [],
        deprecated: NotDeprecated,
      }));
      let name = getName(name ++ "Object");
      let newObj = GraphQL.Object({
        name,
        description: None,
        fields,
        interfaces: [],
      });
      Hashtbl.add(keyValMap^, keyVals, newObj);
      typeMap := GraphQL.TypeMap.add(name, newObj, typeMap^);
      newObj
    }
  }
  and buildPossibleTypes(name, types) = {
    let types = types |> map((x) => aux(x, name)) |> uniqueBy(GraphQL.typeName);
    switch (types) {
      | [] => aux(Any, name)
      | [t] => t
      | [t, ...ts] => {
          let name = getName(name ++ "Union");
          Js.log(name);
          types |> map(t => Js.log(GraphQL.typeName(t)));
          let unionType = GraphQL.Union({
            name,
            description: None,
            possibleTypes: types,
          });
        typeMap := GraphQL.TypeMap.add(name, unionType, typeMap^);
        unionType    
      }
    }
  }
  and aux(jst, name) = {
    switch(jst) {
      | String          => GraphQL.gqlString
      | Float           => GraphQL.gqlFloat
      | Int             => GraphQL.gqlInt
      | Bool            => GraphQL.gqlBoolean
      | Object(keyVals) => buildObject(name, keyVals)
      | PossibleTypes(types)    => buildPossibleTypes(name, types)
      | Array(t)        => GraphQL.ListType(aux(t, name))
      | NonNull(t)      => GraphQL.NonNull(aux(t, name))
      | Any             => GraphQL.LazyType("Any")
      | Impossible      => GraphQL.LazyType("CantUnifyTypes")
    }
  };
  GraphQL.{
      query: aux(jst, "Query") |> GraphQL.baseType |> t => GraphQL.NonNull(t), 
      mutation: None, 
      types: typeMap^,
  }
};