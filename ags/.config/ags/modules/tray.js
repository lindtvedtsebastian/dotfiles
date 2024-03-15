const systemTray = await Service.import("systemtray");

/** @param {import('types/service/systemtray').TrayItem} item */
const SystemTrayItem = (item) =>
  Widget.Button({
    child: Widget.Icon().bind("icon", item, "icon"),
    tooltipMarkup: item.bind("tooltip_markup"),
    onPrimaryClick: (_, event) => item.activate(event),
    onSecondaryClick: (_, event) => item.openMenu(event),
  });

const SystemTray = Widget.Box({
  children: systemTray.bind("items").as((i) => i.map(SystemTrayItem)),
  spacing: 4,
  margin: 4,
});

export { SystemTray };
