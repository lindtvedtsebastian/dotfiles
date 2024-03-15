import { SystemTray } from "./modules/tray.js";
import { Workspaces } from "./modules/hyprland.js";
import { System } from "./modules/system.js";

App.addIcons(`${App.configDir}/assets/icons`);

const date = Variable("", {
  poll: [1000, 'date "+%H:%M:%S"'],
});

const Bar = (/** @type {number} */ monitor) =>
  Widget.Window({
    monitor,
    name: `bar${monitor}`,
    anchor: ["top", "left", "right"],
    exclusivity: "exclusive",
    className: "bar",
    child: Widget.CenterBox({
      start_widget: Widget.Box({ child: Workspaces }),
      center_widget: Widget.Label({
        hpack: "center",
        label: date.bind(),
        className: "box",
      }),
      end_widget: Widget.Box({ hpack: "end", children: [System, SystemTray] }),
    }),
  });

App.config({
  windows: [Bar(0)],
  style: "./style.css",
});
