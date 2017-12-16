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
  ~fontSize="15px",
  ~height="500px",
  ~lineHeight="1",
  ~borderRadius="4px",
  ~color="#424548",
  ~padding="0 12px",
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
    |> Json.deriveType
    |> Json.mapToGraphQLSchema
    |> GqlPrinter.printSchema
  ){
    | _ => ""
  }
};

let initialJson = {|
  {
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
    ReactDOMRe.Style.make(~margin="0 auto", ~maxWidth="500px", ())
  )>
    <p>(str("JSON:"))</p>
    <textarea 
      rows=40
      style=(inputStyle) 
      value=(json)
      onChange=(reduce((_evt) => UpdateGql(valueFromEvent(_evt)))) 
      width="100%"
      height="100%"></textarea>
    <p>(str("Gql Output:"))</p>
    <textarea 
    rows=40
    style=(inputStyle) 
    readOnly=(Js.Boolean.to_js_boolean(true))
    value=(gql)
    width="100%"
    height="100%"></textarea>
  </div>
};


/* This is the basic component.
let component = ReasonReact.statelessComponent("Page");

/* Your familiar handleClick from ReactJS. This mandatorily takes the payload,
   then the `self` record, which contains state (none here), `handle`, `reduce`
   and other utilities */
let handleClick = (_event, _self) => Js.log("clicked!");

/* `make` is the function that mandatorily takes `children` (if you want to use
   `JSX). `message` is a named argument, which simulates ReactJS props. Usage:

   `<Page message="hello" />`

   Which desugars to

   `ReasonReact.element(Page.make(~message="hello", [||]))` */
let test = [%bs.raw {|JSON.parse("{\"woah\": 123}")|} ];
let data = {| {"type": "PlottableGraph", "eye_candy": {"tick_label_format": "fraction", "axes": {"y": {"major_unit": "5", "minor_unit": "1"}, "x": {"major_unit": "5", "minor_unit": "1"}}, "axis_intersection": "centre", "foreground_layer": [{"points": [{"label": "A", "y": "1/1", "substatus": "unknown", "x": "7/1"}], "type": "point"}]}, "interactivity": {"interactive_layer": [{"points": [{"label": "B", "y": "1/1", "substatus": "correct", "x": "3/1"}], "type": "point"}]}} |};
let line = data 
          |> Js.Json.parseExn 
          |> Js.Json.classify
          |> Json.deriveType
          |> Json.mapToGraphQLSchema
          |> GqlPrinter.printSchema
;
Js.log(line);
let make = (~message, _children) => {
  ...component,
  render: (self) =>
    <div onClick=(self.handle(handleClick))> (ReasonReact.stringToElement(message)) </div>
}; */
