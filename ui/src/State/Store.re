open Common;
open Tilia;

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

  let state = Tilia.make({ items: Js.Dict.fromArray([||]) });
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
  let seen = Js.Set.make();
  let periods: array(Config.Pricing.period) = [||];
  inventory->Js.Array.forEach(~f=inv => {
    inv.period_list
    |> Js.Array.forEach(~f=(pl: Config.Pricing.period) =>
         if (seen->Js.Set.has(~value=pl.unit) === false) {
           periods->Js.Array.push(~value=pl) |> ignore;
           seen->Js.Set.add(~value=pl.unit) |> ignore;
         }
       )
  });
  periods;
};

let makeStore = initialExecutorConfig =>
  carve(({ derived }) => {
    {
      premise_id: derived(deriveConfig |> derivePremiseId),
      config: initialExecutorConfig,
      period_list: derived(deriveConfig |> derivePeriodList),
      unit: PeriodList.Unit.signal->lift,
    }
  });

let makeServerStore = () => PremiseContainer.empty;

// This store needs to be isomorphic so that it's in the context of the user's session on the server
// There may be a better way to do this, but for now I make main_store an option.
// Then I use getStore in my components to get the store based on the execution context.
let main_store: option(t) =
  switch%platform (Runtime.platform) {
  | Client => Some(makeStore(PremiseContainer.state))
  | Server => None
  };

let%browser_only getStore = () => main_store;
