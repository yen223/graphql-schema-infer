/* This is the basic component. */

type state = {
  json: string,
  gql: string
};
type action = 
  | UpdateGql(string)
;

let component = ReasonReact.reducerComponent("Page");

let str = ReasonReact.stringToElement;
let valueFromEvent = (evt):string => (
  evt |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj
)##value
;

let inputStyle = ReactDOMRe.Style.make(
  ~display="flex",
  ~fontSize="15px",
  ~height="100%",
  ~lineHeight="1",
  ~borderRadius="2px",
  ~color="#424548",
  ~padding="12px 12px",
  ~width="100%",
  ~transition="border-color 0.15s ease",
  ~resize="none",
  ~boxSizing="border-box",
  ~border="1px solid #d4d7d9",
  ()
);

let jsonToGql(json) = {
  try ( 
    json 
    |> Js.Json.parseExn 
    |> Js.Json.classify
    |> JsonType.deriveType
    |> JsonType.mapToGraphQLSchema
    |> GqlPrinter.printSchema
  ){
    | _ => ""
  }
};

let initialJson = {|{
  "hero": {
    "name": "R2-D2",
    "friends": [
      {
        "name": "Luke Skywalker"
      },
      {
        "name": "Han Solo"
      },
      {
        "name": "Leia Organa"
      }
    ]
  }
}
|};
let make = (_children) => {
  ...component,
  initialState: () => {
    json: initialJson,
    gql: jsonToGql(initialJson),
  },
  reducer: (action, _) => switch action {
    | UpdateGql(json) => ReasonReact.Update({
        json,
        gql: jsonToGql(json)
      })
  },
  render: ({state: {json, gql}, reduce}) =>
  <div style=(
    ReactDOMRe.Style.make(~margin="10px auto", ~display="flex", ())
  )>
    <div style=(ReactDOMRe.Style.make(~padding="10px", ~flexBasis="100%", ())) >
      <p>(str("JSON:"))</p>
      <textarea 
        rows=40
        style=(inputStyle)
        value=(json)
        onChange=(reduce((_evt) => UpdateGql(valueFromEvent(_evt)))) 
        width="100%"
        height="100%"></textarea>
    </div>
    <div style=(ReactDOMRe.Style.make(~padding="10px", ~flexBasis="100%", ()))>
      <p>(str("GraphQL Schema:"))</p>
      <textarea 
      rows=40
      style=(inputStyle) 
      readOnly=(Js.Boolean.to_js_boolean(true))
      value=(gql)
      width="100%"
      height="100%"></textarea>
    </div>
  </div>
};