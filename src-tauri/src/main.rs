#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]


use tauri::{WindowBuilder, WindowUrl};


fn main() {
    let port = 44999;
    let context = tauri::generate_context!("tauri.conf.json");

    tauri::Builder::default()
        .plugin(tauri_plugin_localhost::Localhost::new(port))
        .plugin(tauri_plugin_window_state::Builder::default().build())
        .menu(tauri::Menu::os_default(&context.package_info().name))
        .setup(move |app| {
            WindowBuilder::new(
                app,
                "main",
                WindowUrl::External(
                    format!("http://localhost:{}", port).parse().unwrap()
                )
            )
                .title("Diffuse")
                .maximized(true)
                .resizable(true)
                .build()?;

            // Fin
            Ok(())
        })
        .run(context)
        .expect("Error while running tauri application");
}
