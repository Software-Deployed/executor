module Reservation = {
  type t = {
    // Date of the reservation
    date: Js.Date.t,
    // If the unit_type is hour and units is set to 4, the reservation will end 4 hours after the date.
    // There is probably a better way to model this.
    units: int,
    unit_type: PeriodList.Unit.t,
  };
};

module CartItem = {
  type t = {
    reservation: option(Reservation.t),
    // This is the ID of an InventoryItem.t. There is probably a better way to model this.
    inventory_id: int,
    quantity: int,
  };
};

module CartStore = {
  type t = {items: Js.Dict.t(CartItem.t)};
  // For now make is a stub that returns an empty cart. This will eventually restore the user's previous cart.
  // A feature I would really like to have is real time cart sharing. Use cases include: group ordering, third party cart payment, etc.

  let state =
    switch%platform (Runtime.platform) {
    | Server => { items: Js.Dict.fromArray([||]) }
    | Client => Tilia.Core.make({ items: Js.Dict.fromArray([||]) })
    };
  let add_to_cart = (item: Config.InventoryItem.t) => {
    let cart_item =
      switch (state.items->Js.Dict.get(item.id->Int.to_string)) {
      | Some(item) => {
          ...item,
          quantity: item.quantity + 1,
        }
      | None => {
          reservation: None,
          inventory_id: item.id,
          quantity: 1,
        }
      };

    state.items->Js.Dict.set(item.id->Int.to_string, cart_item);
  };
};

type t = {
  // Perhaps the ID should be typed as a UUID?
  premise_id: string,
  config: Config.t,
  period_list: array(Config.Pricing.period),
  unit: PeriodList.Unit.t,
};

let deriveConfig = (store: t) => store.config;

let derivePremiseId = (config: Config.t) => {
  let premise = config.premise->Belt.Option.getUnsafe;
  premise.id;
};

let derivePeriodList = (config: Config.t) => {
  let inventory = config.inventory->Belt.Array.copy;
  let seen = [||];
  let periods: array(Config.Pricing.period) = [||];
  inventory->Belt.Array.forEach(inv => {
    inv.period_list
    ->Belt.Array.forEach((pl: Config.Pricing.period) =>
        if (seen->Belt.Array.some(u => u == pl.unit)) {
          periods->Belt.Array.push(pl) |> ignore;
          seen->Belt.Array.push(pl.unit) |> ignore;
        }
      )
  });
  periods;
};

let%browser_only makeStore = initialExecutorConfig =>
  Tilia.Core.carve(({ derived }) => {
    {
      premise_id: derived(store => store->deriveConfig->derivePremiseId),
      config: initialExecutorConfig,
      period_list: derived(store => store->deriveConfig->derivePeriodList),
      unit: PeriodList.Unit.defaultState /*PeriodList.Unit.signal->lift,*/
    }
  });

let makeServerStore = () => PremiseContainer.empty;

// This store needs to isomorphic so that it's in the context of the user's session on the server
// There may be a better way to do this, but for now I make main_store an option.
// Then I use getStore in my components to get the store based on the execution context.
let getStore = () =>
  switch%platform (Runtime.platform) {
  | Client => makeStore(PremiseContainer.state)
  | Server => {
      premise_id: "",
      config: PremiseContainer.empty,
      period_list: [||],
      unit: PeriodList.Unit.defaultState,
    }
  };
