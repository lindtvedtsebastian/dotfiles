import { SystemTray } from "./modules/tray.js";
import { Workspaces } from "./modules/hyprland.js";

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
      start_widget: Workspaces,
      center_widget: Widget.Label({ hpack: "center", label: date.bind() }),
      end_widget: Widget.Box({ hpack: "end", child: SystemTray }),
    }),
  });

App.config({
  windows: [Bar(0)],
});
