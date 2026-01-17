type ws_protocols = option(array(string));
type ws_events =
  | [@mel.string "open"] Open
  | [@mel.string "close"] Close
  | [@mel.string "message"] Message;
type ws_event = {. data: string};
[@mel.new]
external makeWithProtocols: (. string, ws_protocols) => 'a = "WebSocket";
[@mel.new] external make: string => 'a = "WebSocket";
[@mel.send] external close: unit => unit = "close";
[@mel.send] external send_string: ('a, string) => unit = "send";
[@mel.get] external readyState: 'a => int = "readyState";
[@mel.send]
external addEventListener: (. 'a, ws_events, ws_event => unit) => unit =
  "addEventListener";
