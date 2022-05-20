#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use tauri::{PhysicalPosition, PhysicalSize, Position, Size};
use tauri::{Menu, MenuItem, Submenu, WindowUrl};


fn main() {
    let port = 44999;

    tauri::Builder::default()
        .plugin(tauri_plugin_localhost::Localhost::new(port))
        .plugin(tauri_plugin_window_state::WindowState::default())
        .setup(move |app| {
            let w = tauri::window::WindowBuilder::new(
                    app,
                    "main",
                    WindowUrl::External(
                        format!("http://localhost:{}", port).parse().unwrap()
                    )
                )
                .title("Diffuse")
                .menu(menu())
                .maximized(true)
                .resizable(true)
                .build()
                .unwrap();

            // Scale window to a bit smaller than screen size
            // let monitor = w.current_monitor().unwrap().unwrap();
            // let screen_size = tauri::window::Monitor::size(&monitor);

            // w.set_size(Size::Physical(PhysicalSize {
            //     width: screen_size.width - 60,
            //     height: screen_size.height - 60,
            // }))
            // .unwrap();

            // Put the window in the middle of the screen
            // let window_offset = Position::Physical(PhysicalPosition { x: 30, y: 30 });

            // w.set_position(window_offset).unwrap();

            // Fin
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("Error while running tauri application");
}

/**
 * Menu
 */
fn menu() -> Menu {
    let app_menu = Menu::new()
        .add_native_item(MenuItem::Hide)
        .add_native_item(MenuItem::Services)
        .add_native_item(MenuItem::Separator)
        .add_native_item(MenuItem::Quit);

    let file_menu = Menu::new().add_native_item(MenuItem::CloseWindow);

    let edit_menu = Menu::new()
        .add_native_item(MenuItem::Undo)
        .add_native_item(MenuItem::Redo)
        .add_native_item(MenuItem::Separator)
        .add_native_item(MenuItem::Cut)
        .add_native_item(MenuItem::Copy)
        .add_native_item(MenuItem::Paste)
        .add_native_item(MenuItem::SelectAll);

    let window_menu = Menu::new()
        .add_native_item(MenuItem::Minimize)
        .add_native_item(MenuItem::Zoom)
        .add_native_item(MenuItem::Separator)
        .add_native_item(MenuItem::ShowAll);

    Menu::new()
        .add_submenu(Submenu::new("Diffuse", app_menu))
        .add_submenu(Submenu::new("File", file_menu))
        .add_submenu(Submenu::new("Edit", edit_menu))
        .add_submenu(Submenu::new("Window", window_menu))
}
