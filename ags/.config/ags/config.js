const date = Variable("", {
  poll: [1000, 'date "+%H:%M:%S"'],
});

const Bar = (/** @type {number} */ monitor) =>
  Widget.Window({
    monitor,
    name: `bar${monitor}`,
    anchor: ["top", "left", "right"],
    exclusivity: "exclusive",
    child: Widget.CenterBox({
      center_widget: Widget.Label({ hpack: "center", label: date.bind() }),
    }),
  });

App.config({
  windows: [Bar(0)],
});
