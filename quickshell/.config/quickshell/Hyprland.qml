import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts

MouseArea {
    id: root
    required property var bar
    property int wsCount: 10
    readonly property HyprlandMonitor monitor: Hyprland.focusedMonitor
    property int currentIndex: 0
    property int existsCount: 0

    signal workspaceAdded(workspace: HyprlandWorkspace)

    width: row.implicitWidth + 10
    height: 50

    Row {
        id: row
        spacing: 4
        Repeater {
            model: wsCount
            delegate: MouseArea {
                id: wsItem
                required property int index
                property int wsIndex: 1 + index
                property HyprlandWorkspace workspace: null
                property bool exists: workspace != null
                property bool active: monitor.activeWorkspace == workspace && workspace.monitor == monitor

                onActiveChanged: {
                    if (active) {
                        root.currentIndex = wsIndex;
                    }
                    // Start the width animation each time active changes
                    widthAnimation.to = active ? 24 : 12;
                    widthAnimation.running = true;
                }

                onExistsChanged: {
                    root.existsCount += exists ? 1 : -1;
                }

                onPressed: Hyprland.dispatch(`workspace ${wsIndex}`)

                Connections {
                    target: root
                    function onWorkspaceAdded(workspace: HyprlandWorkspace) {
                        if (workspace.id == wsItem.wsIndex) {
                            wsItem.workspace = workspace;
                        }
                    }
                }

                hoverEnabled: true
                width: 12
                height: 12

                Rectangle {
                    width: parent.width
                    height: parent.height
                    radius: height / 2
                    color: exists ? (parent.containsMouse ? "#f7f7a3" : "#f79199") : (parent.containsMouse ? "#f7f7a3" : "#fafafa")
                }

                // Define NumberAnimation outside of property bindings to run manually
                NumberAnimation {
                    id: widthAnimation
                    target: wsItem
                    property: "width"
                    duration: 200
                }
            }
        }
    }

    Connections {
        target: Hyprland.workspaces
        function onObjectInsertedPost(workspace) {
            root.workspaceAdded(workspace);
        }
    }



    Component.onCompleted: {
        Hyprland.workspaces.values.forEach(workspace => {
            root.workspaceAdded(workspace);
        });
    }
}
