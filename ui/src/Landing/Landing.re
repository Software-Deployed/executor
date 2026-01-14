open Tilia.React;
open Ui;

let str = React.string;

[@react.component]
let make =
  leaf(() => {
    let main_store = State.Store.getStore();
    Js.log(main_store);
    let _unit = main_store.unit;
    let today =
      Js.Date.make()
      |> Js.Date.setHours(
           ~hours=0.0,
           ~minutes=0.0,
           ~seconds=0.0,
           ~milliseconds=0.0,
         )
      |> Js.Date.fromFloat;
    let (openDate, setOpenDate) = React.useState(() => today);
    let (closeDate, setCloseDate) = React.useState(() => today);

    let updateOpenDate = (openDate: Js.Date.t) => {
      setOpenDate(_prev => openDate);
    };
    let updateCloseDate = (closeDate: Js.Date.t) => {
      setCloseDate(_prev => closeDate);
    };

    <Container>
      <Card className="bg-slate-200/40 border-slate-200/40 border">
        <h1 className="text-xl">
          <span>
            <Icon.MonitorCloud
              size=48
              className="text-slate-400 mr-2 my-auto inline content-start"
            />
            "Cloud Hardware Rental"->str
          </span>
        </h1>
      </Card>
      <Card
        className="grid grid-cols-[auto_1fr] bg-white/20 gap-4 place-items-start items-center">
        <span className="align-middle text-lg">
          <Icon.Clock
            className="text-slate-400 mr-2 my-auto inline content-start"
            size=48
          />
          "Select your reservation type: "->str
        </span>
        <ReservationTypeSelection />
        <div className="col-span-full grid grid-cols-subgrid relative">
          <Icon.Calendar
            size=48
            className="absolute left-0 top-0 bottom-0 my-auto text-slate-400"
          />
          <span className="align-middle text-lg pl-14">
            "Select your reservation start time: "->str
          </span>
          <ReactDayPicker
            mode="range"
            selected={
                       `Range({
                         ReactDayPicker.from: Js.Nullable.return(openDate),
                         ReactDayPicker.to_: Js.Nullable.return(closeDate),
                       })
                     }
            onSelect={
                       `Range(
                         (dates: ReactDayPicker.rangeDate) => {
                           let openDate =
                             switch (dates.from->Js.Nullable.toOption) {
                             | Some(date) => date
                             | None => today
                             };
                           let closeDate =
                             switch (dates.to_->Js.Nullable.toOption) {
                             | Some(date) => date
                             | None => openDate
                             };
                           updateOpenDate(openDate);
                           updateCloseDate(closeDate);
                         },
                       )
                     }
          />
          <span className="align-middle text-lg pl-14">
            "Select your reservation end time: "->str
          </span>
          <input
            className="block align-end outline-slate-400 outline-1 px-2"
          />
        </div>
      </Card>
      <InventoryList openDate closeDate />
      <Cart />
      <div className="w-full">
        <button
          className="mx-auto mt-4 bg-slate-500 hover:bg-slate-700 text-white py-2 px-4 rounded-sm">
          "Book Reservation"->str
        </button>
      </div>
    </Container>;
  });
