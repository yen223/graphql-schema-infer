type t('t) = 
  | Ok('t)
  | Err(string)
;
let map(fn, res) = switch(res) {
  | Ok(x)   => Ok(fn(x))
  | Err(s)  => Err(s)
};
let ok(n) = Ok(n);
let err(s) = Err(s);
let isOk(r) = switch(r){
  | Ok(_) => true
  | Err(_) => false
}
;
let isErr(r) = !isOk(r);
let unwrap(Ok(r)) = r;

let mapOrFirstError(fn, results) = {
  if (List.for_all(isOk, results)) {
    results |> List.map(unwrap) |> fn
  } else {
    List.find(isErr, results)
  }
}