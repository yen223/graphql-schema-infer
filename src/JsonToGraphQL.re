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
let map2 = List.map2;
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

let mkField(name, outputType) = GraphQL.({
  name, 
  description: None, 
  output_type: outputType,
  args: [],
  deprecated: NotDeprecated,
});

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
      Result.Ok(Hashtbl.find(keyValMap^, keyVals))
    } else {
      let fieldNames = keyVals |> map(fst);
      let fieldResultTypes = keyVals 
                             |> map(((key, value)) => aux(value, pascalCase(key)));
      if (List.for_all(Result.isOk, fieldResultTypes)) {
        let obj = fieldResultTypes 
                  |> map(Result.unwrap)
                  |> map2(mkField, fieldNames)
                  |> (fields => GraphQL.Object({
                      name: getName(name ++ "Object"),
                      description: None,
                      fields,
                      interfaces: [],
                    }));
        Hashtbl.add(keyValMap^, keyVals, obj);
        typeMap := GraphQL.TypeMap.add(name, obj, typeMap^);
        Result.ok(obj)
      } else {
        fieldResultTypes 
        |> List.find(n => !Result.isOk(n))
      }
    }
  }
  and buildPossibleTypes(name, types) = {
    let gqlTypes = types |> map((x) => aux(x, name)); 
    if (List.for_all(Result.isOk, gqlTypes)) {
      let types = gqlTypes |> map(Result.unwrap) |> uniqueBy(GraphQL.typeName);
      switch (types) {
        | [] => aux(Any, name)
        | [t] => Result.Ok(t)
        | [t, ...ts] => {
            let name = getName(name ++ "Union");
            let unionType = GraphQL.Union({
              name,
              description: None,
              possibleTypes: types,
            });
          typeMap := GraphQL.TypeMap.add(name, unionType, typeMap^);
          Result.Ok(unionType)
        }
      }
    } else {
      List.find(n => !Result.isOk(n), gqlTypes)
    }
  }
  and aux(jst, name) = {
    switch(jst) {
      | String                  => GraphQL.gqlString |> Result.ok
      | Float                   => GraphQL.gqlFloat |> Result.ok
      | Int                     => GraphQL.gqlInt |> Result.ok
      | Bool                    => GraphQL.gqlBoolean |> Result.ok
      | Object(keyVals)         => buildObject(name, keyVals)
      | PossibleTypes(types)    => buildPossibleTypes(name, types)
      | Array(t)                => aux(t, name) |> Result.map(n => GraphQL.ListType(n))
      | NonNull(t)              => aux(t, name) |> Result.map(n => GraphQL.NonNull(n))
      | Any                     => GraphQL.LazyType("Any") |> Result.ok
      | Impossible              => Result.Err("Could not resolve type")
    }
  };
  aux(jst, "Query") 
  |> Result.map(t => GraphQL.{
      query: t |> GraphQL.baseType |> t => GraphQL.NonNull(t), 
      mutation: None,
      types: typeMap^,
    })
};