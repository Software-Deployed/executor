let render = (url: string): string => {
  /*let app_url = ReasonReactRouter.dangerouslyGetInitialUrl(~serverUrlString=url, ());*/
  let _requested_root = "/";
  /*switch (app_url.path->Belt.List.head) {
    | Some(path) => path
    | None => "/"
    };*/
  let _is_child_route = false;
  /*
   Common.Constants.premise_routes->Belt.List.has(requested_root, (a, b) =>
     a == b
   );
   */
  let route_root = "/"; /*
    if (is_child_route || app_url.path->Belt.List.length == 0) {
      "/";
    } else {
      "/" ++ app_url.path->Belt.List.head->Option.get;
    };
    */

  ReactDOM.renderToString(<App route_root />);
};
/*
 Route.getMatchingPremise(route_root)
 |> Js.Promise.then_((premise: option(PeriodList.Premise.t)) => {
      switch (premise) {
      | None =>
        Js.Promise.resolve({
          html: ReactDOMServer.renderToString(<NotFound />),
          executorConfig: {
            inventory: [||],
            premise: None,
          },
        })
      | Some(premise) =>
        premise.id
        |> Inventory.getInventoryList
        |> Js.Promise.then_(inventory => {
             Js.log(inventory);
             let config: Config.t = {
               inventory,
               premise: Some(premise),
             };
             Js.Promise.resolve(
               State.Store.makeServerStore(config, _ => {
                 {
                   html:
                     ReactDOMServer.renderToString(
                       <App route_root server_url="/" />,
                     ),
                   executorConfig: config,
                 }
               }),
             );
           })
      }
    });
   */
