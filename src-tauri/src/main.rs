#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]


use tauri::{WindowBuilder, WindowUrl};


fn main() {
    let port = 44999;
    let context = tauri::generate_context!("tauri.conf.json");

    tauri::Builder::default()
        .plugin(tauri_plugin_localhost::Builder::new(port).build())
        .plugin(tauri_plugin_window_state::Builder::default().build())
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
                .theme(None)
                .build()?;

            // Fin
            Ok(())
        })
        // .on_window_event(|event| match event.event() {

        //     tauri::WindowEvent::ThemeChanged(theme) => {
        //         if !focused {
        //             event.window().hide().unwrap();
        //         }

        //     }

        //     _ => {}

        // })
        .run(context)
        .expect("Error while running tauri application");
}
