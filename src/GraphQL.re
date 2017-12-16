type name = string;

type description = option(string);

type defaultValue = option(string);

type deprecated =
  | IsDeprecated(string)
  | NotDeprecated;

type enumValue =
  | EnumValue(name, description, deprecated);

type enumValues = list(enumValue);

type field = {
  name,
  description,
  args: list(inputValueType),
  output_type: t,
  deprecated
}
and fields = list(field)
and inputValueType = {name, description, defaultValue, graphqlType: t}
and possibleTypes = list(t)
and scalarType = {name, description}
and objectType = {name, description, fields, interfaces: list(t)}
and interfaceType = {name, description, fields, possibleTypes}
and unionType = {name, description, possibleTypes}
and enumType = {name, description, enumValues}
and inputObjectType = {name, description, inputValueTypes: list(inputValueType)}
and t =
  | Scalar(scalarType)
  | Object(objectType)
  | Interface(interfaceType)
  | Union(unionType)
  | Enum(enumType)
  | InputObject(inputObjectType)
  | ListType(t)
  | NonNull(t) /*TODO: Figure out way to avoid nested NonNulls. GADT? */
  | LazyType(string) /*TODO: Figure out way to avoid this */
;

let gqlString = Scalar({
  name: "String", 
  description: Some("A UTF‐8 character sequence."),
});
let gqlInt = Scalar({
  name: "Int", 
  description: Some("A signed 32‐bit integer.")
});
let gqlFloat = Scalar({
  name: "Float", 
  description: Some("A signed double-precision floating-point value."),
});
let gqlBoolean = Scalar({
  name: "Boolean", 
  description: Some("`true` or `false`")
});
module TypeMap = Map.Make(String);

type type_map = TypeMap.t(t);

type schema = {
  query: t,
  mutation: option(t),
  types: type_map
};

let rec typeName =
  fun
  | Scalar({name, _})
  | Object({name, _})
  | Interface({name, _})
  | Union({name, _})
  | Enum({name, _})
  | InputObject({name, _})
  | LazyType(name) => name
  | ListType(typ)
  | NonNull(typ) => typeName(typ);

let rec typeLabel = fun
  | Scalar({name, _})
  | Object({name, _})
  | Interface({name, _})
  | Union({name, _})
  | Enum({name, _})
  | InputObject({name, _})
  | LazyType(name) => name
  | ListType(typ) => "[" ++ typeLabel(typ) ++ "]"
  | NonNull(typ) => typeLabel(typ) ++ "!"
;