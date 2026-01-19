[@react.component]
let make = (~children=?) =>
  <html lang="en">
    <head>
      <link rel="stylesheet" href="/style.css" />
      <script type_="module" src="/app.js" />
      <meta charSet="utf-8" />
      <meta httpEquiv="X-UA-Compatible" content="IE=edge" />
      <meta name="viewport" content="width=device-width, initial-scale=1" />
      <title> "Create Reason React Tailwind"->React.string </title>
    </head>
    <body>
      <div id="root">
        {children->Belt.Option.getWithDefault(React.null)}
      </div>
    </body>
  </html>;
