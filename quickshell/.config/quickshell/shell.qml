import QtQuick
import Quickshell

ShellRoot {
    PanelWindow {
        anchors {
            top: true
            left: true
            right: true
        }

        height: 20
        Text {
            anchors.centerIn: parent

            text: "Hello Quickshell"
        }
        exclusiveZone: 5
        
   }
}
