open Tilia.React;

let str = React.string;

[@react.component]
let make =
  leaf((~item: option(Config.InventoryItem.t)=?) => {
    let item =
      switch (item) {
      | Some(item) => item
      | None =>
        Js.Exn.raiseError("The item property is required on InventoryItem")
      };
    let image =
      "https://random.danielpetrica.com/api/random?" ++ item.id->Int.to_string;
    let%browser_only select_item = e => {
      e->React.Event.toSyntheticEvent->React.Event.Synthetic.preventDefault;
      ReasonReactRouter.replace("/item/" ++ Int.to_string(item.id));
    };
    <a
      id={"item-" ++ Int.to_string(item.id)}
      onClick=select_item
      href={"/item/" ++ Int.to_string(item.id)}
      className="flex flex-1 flex-col grow border-2">
      <button
        className="relative m-[1.5] flex flex-1 flex-col grow"
        onClick={_ => Store.CartStore.add_to_cart(item)}>
        <div className="rounded-sm shadow-sm m-0 p-0">
          <img className="p-[1.5] w-full aspect-square" src=image />
        </div>
        <div
          className="flex flex-row justify-between w-full bg-gray-300 text-white shadow-sm">
          <h2 className="tracking-wider text-xs px-2"> item.name->str </h2>
        </div>
        <div
          className="flex flex-col grow flex-1 w-full bg-white/40 rounded-sm m-[1.5] justify-between items-end">
          <p className="text-xs text-left m-2"> item.description->str </p>
          <Pricing period_list={item.period_list} />
        </div>
      </button>
    </a>;
  });
