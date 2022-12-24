#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use std::path::PathBuf;
use tauri::TitleBarStyle;
use tauri::{utils::config::AppUrl, WindowBuilder, WindowUrl};

fn main() {
    let port = 44999;
    let mut context = tauri::generate_context!("tauri.conf.json");

    let url = format!("http://localhost:{}", port).parse().unwrap();
    let window_url = WindowUrl::External(url);

    context.config_mut().build.dist_dir = AppUrl::Url(window_url.clone());
    context.config_mut().build.dev_path = AppUrl::Url(window_url.clone());

    tauri::Builder::default()
        .plugin(tauri_plugin_localhost::Builder::new(port).build())
        .plugin(tauri_plugin_window_state::Builder::default().build())
        .setup(move |app| {
            WindowBuilder::new(
                app,
                "main",
                WindowUrl::App(PathBuf::from("index.html"))
            )
                .title("Diffuse")
                .title_bar_style(TitleBarStyle::Overlay)
                .hidden_title(true)
                .maximized(true)
                .resizable(true)
                .theme(None)
                .enable_clipboard_access()
                .user_agent("Chrome")
                .build()?;

            // Fin
            Ok(())
        })
        .run(context)
        .expect("Error while running tauri application");
}