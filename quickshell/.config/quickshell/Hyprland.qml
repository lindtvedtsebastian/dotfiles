import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts

MouseArea {
    id: root
    property int wsCount: 10
    readonly property HyprlandMonitor monitor: Hyprland.focusedMonitor

    signal workspaceAdded(workspace: HyprlandWorkspace)

    width: row.implicitWidth + 10
    height: 50

    Row {
        id: row
        spacing: 4
        Repeater {
            model: wsCount
            MouseArea {
                id: wsItem
                hoverEnabled: true

                required property int index
                property int wsIndex: 1 + index
                property HyprlandWorkspace workspace: null
                property bool exists: workspace != null
                property bool active: (monitor?.activeWorkspace ?? false) && monitor.activeWorkspace == workspace

                onPressed: Hyprland.dispatch(`workspace ${wsIndex}`)

                Connections {
                    target: root
                    function onWorkspaceAdded(workspace: HyprlandWorkspace) {
                        if (workspace.id == wsItem.wsIndex) {
                            wsItem.workspace = workspace;
                        }
                    }
                }

                property real animActive: active ? 100 : 0
                Behavior on animActive {
                    NumberAnimation {
                        duration: 100
                    }
                }

                width: 12 + 0.12 * animActive
                height: 12

                Rectangle {
                    width: parent.width
                    height: parent.height
                    radius: height / 2
                    color: active ? "#bfdef2" : exists ? (parent.containsMouse ? "#f7f7a3" : "#f79199") : (parent.containsMouse ? "#f7f7a3" : "#fafafa")
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
