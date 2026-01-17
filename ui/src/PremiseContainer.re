type input_premise = Js.Dict.t(string);

type input_config = {
  inventory: array(Config.InventoryItem.t),
  premise: option(input_premise),
};

let empty: Config.t = {
  inventory: [||],
  premise: None,
};

let domExecutorConfig =
  switch%platform (Runtime.platform) {
  | Client => Some(empty)
  | Server => None
  };

let initialExecutorConfig =
  switch (domExecutorConfig) {
  | None => empty
  | Some(config) => {
      inventory: config.inventory,
      premise: None,
      /*switch (config.premise) {
        | Some(premise_in) =>
          Some({
            id: premise_in.id, //->Js.Dict.unsafeGet("id"),
            name: premise_in.name, //->Js.Dict.unsafeGet("name"),
            description: premise_in.description, //->Js.Dict.unsafeGet("description"),
            updated_at: premise_in.updated_at // ->Js.Dict.unsafeGet("updated_at")
            //|> Js.Date.fromString,
          })
        | None => None
        },*/
    }
  };

let state =
  switch%platform (Runtime.platform) {
  | Client =>
    Tilia.Core.source(. initialExecutorConfig, (. _prev, set) => {
      switch (initialExecutorConfig.premise) {
      | Some(premise) =>
        let { id, updated_at, _ }: PeriodList.Premise.t = premise;
        set->Client.Socket.subscribe(id, updated_at->Js.Date.getTime);
        initialExecutorConfig;
      | None => initialExecutorConfig
      }
    })
  | Server => initialExecutorConfig
  };
