module Pricing = {
  type period = {
    id: int,
    unit: string,
    label: string,
    price: int,
    max_value: int,
    min_value: int,
  };
  type period_list = array(period);
};

module InventoryItem = {
  type t = {
    description: string,
    id: int,
    name: string,
    quantity: int,
    premise_id: string,
    period_list: Pricing.period_list,
  };
};

type t = {
  inventory: array(InventoryItem.t),
  premise: option(PeriodList.Premise.t),
};

module SSR = {
  let empty: t = {
    premise: None,
    inventory: [||],
  };
};
