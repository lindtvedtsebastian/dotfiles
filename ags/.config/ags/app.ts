import { App } from "astal/gtk3";
import Bar from "./widget/Bar";
import style from "./style.css"

App.start({
  css: style,
  icons: `${SRC}/assets`,
  main() {
    App.get_monitors().map(Bar);
  },
});
