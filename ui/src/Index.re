[@platform js]
module Style = {
  [%%raw "import \"./style.css\""];
};

[@platform native]
module Style = {
  let _css = ();
};

let rootElement = ReactDOM.querySelector("#root");

let%browser_only _ =
  switch (rootElement) {
  | Some(domNode) => ReactDOM.Client.hydrateRoot(domNode, <App />)->ignore
  | None => Js.log("No root element found")
  };
