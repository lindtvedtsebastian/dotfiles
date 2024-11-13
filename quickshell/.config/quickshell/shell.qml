import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Widgets

ShellRoot {
    PanelWindow {

    id: root
        color: "#262730"
        anchors {
            top: true
            left: true
            right: true
        }

        height: 30

        Button {
            id: nix
            width: 30
            height: 30
            background: Rectangle {
                color: nix.down ? "#fafafa" : "#373841"
                bottomLeftRadius: 5
                topLeftRadius: 5
                bottomRightRadius: 5
                topRightRadius: 5
                width: 25
                height: 25
                anchors.centerIn: parent
                IconImage {
                    anchors.centerIn: parent
                    width: 20
                    height: 20
                    source: "root:assets/nix.svg"
                }
            }
        }

        Hyprland {
           bar: root
        }

        Text {
            id: clock
            anchors.centerIn: parent
            font.family: "inter"
            color: "#fafafa"

            Process {
                id: dateProc

                command: ["date", "+%a %b %d %H:%M:%S"]
                running: true

                stdout: SplitParser {
                    onRead: data => clock.text = data
                }
            }

            Timer {
                interval: 1000

                running: true

                repeat: true

                onTriggered: dateProc.running = true
            }
        }
        exclusiveZone: 5
    }
}
