const divide = ([total, free]) => free / total;

const ram = Variable(0, {
  poll: [
    2000,
    "free",
    (out) =>
      divide(
        // @ts-ignore
        out
          .split("\n")
          .find((line) => line.includes("Mem:"))
          ?.split(/\s+/)
          .splice(1, 2) ?? ["0", "0"],
      ),
  ],
});

const Ram = Widget.Box({
  className: "box",
  spacing: 4,
  children: [
    Widget.Label({
      label: ram.bind().transform((r) => Math.round(r * 100).toString() + "%"),
    }),
    Widget.CircularProgress({
      className: "circle",
      startAt: 0.75,
      value: ram.bind(),
      child: Widget.Icon({
        icon: "ram-symbolic",
        css: "color:#eeeeee;",
        size: 22,
      }),
    }),
  ],
});

const cpu = Variable(0, {
  poll: [
    2000,
    "top -b -n 1",
    (out) =>
      divide([
        100,
        // @ts-ignore
        out
          .split("\n")
          .find((line) => line.includes("Cpu(s)"))
          .split(/\s+/)[1]
          .replace(",", "."),
      ]),
  ],
});

const Cpu = Widget.Box({
  className: "box",
  spacing: 4,
  children: [
    Widget.Label({
      label: cpu.bind().transform((c) => Math.round(c * 100).toString() + "%"),
    }),
    Widget.CircularProgress({
      className: "circle",
      startAt: 0.75,
      value: cpu.bind(),
      child: Widget.Icon({
        icon: "cpu-symbolic",
        css: "color:#eeeeee;",
        size: 22,
      }),
    }),
  ],
});

export const System = Widget.Box({
  spacing: 4,
  children: [Cpu, Ram],
});
