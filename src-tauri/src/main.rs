#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use std::path::PathBuf;
use tauri::{TitleBarStyle, WindowBuilder, WindowUrl};
use tauri::utils::config::AppUrl;


// 🚀 PRODUCTION


#[cfg(not(dev))]
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
            build_window(app.handle());
            Ok(())
        })
        .run(context)
        .expect("Error while running tauri application");
}



// 💣 DEVELOPMENT


#[cfg(dev)]
fn main() {
    let mut context = tauri::generate_context!("tauri.conf.json");

    // Need to run Diffuse's dev server command on port 8000
    // instead of Tauri's dev server which doesn't to work very well.
    let url = format!("http://localhost:{}", 8000).parse().unwrap();
    let window_url = WindowUrl::External(url);

    context.config_mut().build.dist_dir = AppUrl::Url(window_url.clone());
    context.config_mut().build.dev_path = AppUrl::Url(window_url.clone());

    tauri::Builder::default()
        .plugin(tauri_plugin_window_state::Builder::default().build())
        .setup(move |app| {
            build_window(app.handle());
            Ok(())
        })
        .run(context)
        .expect("Error while running tauri application");
}



// WINDOWS


fn build_window(app: tauri::AppHandle) {
    WindowBuilder::new(
        &app,
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
        .build()
        .unwrap();
}