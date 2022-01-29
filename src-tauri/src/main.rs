#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use tauri::{App, PhysicalPosition, PhysicalSize, Position, Size};
use tauri::{Window, WindowBuilder, WindowUrl};

fn main() {
    let port = 44999;

    tauri::Builder::default()
        .plugin(tauri_plugin_localhost::Localhost::new(port))
        .setup(move |app| {
            let w = create_window(app, port).unwrap();

            let monitor = w.current_monitor().unwrap().unwrap();
            let screen_size = tauri::window::Monitor::size(&monitor);

            w.set_size(Size::Physical(PhysicalSize {
                width: screen_size.width - 60,
                height: screen_size.height - 60 - 50,
            }))
            .unwrap();

            w.set_position(Position::Physical(PhysicalPosition { x: 30, y: 30 }))
                .unwrap();

            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}

fn create_window(app: &mut App, port: u16) -> tauri::Result<Window> {
    return app.create_window(
        "Diffuse",
        WindowUrl::External(format!("http://localhost:{}", port).parse().unwrap()),
        |window_builder, webview_attributes| {
            let w = window_builder
                .title("Diffuse")
                .maximized(true)
                // .position(10.0, 10.0)
                // .inner_size(screen_size.width - 20, screen_size.height - 20)
                .resizable(true)
                .transparent(true);

            (w, webview_attributes)
        },
    );
}
