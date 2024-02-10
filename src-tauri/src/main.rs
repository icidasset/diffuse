// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use tauri::{webview::Url, AppHandle, WebviewBuilder, WebviewUrl, WindowBuilder};
use tauri::{Manager, Runtime};

// 🚀 PRODUCTION

#[cfg(not(dev))]
fn main() {
    let port = 44999;
    let url = format!("http://localhost:{}", port).parse().unwrap();

    let builder = default_builder().plugin(tauri_plugin_localhost::Builder::new(port).build());

    setup(url, builder)
}

// 💣 DEVELOPMENT

#[cfg(dev)]
fn main() {
    let port = 8000;
    let url = format!("http://localhost:{}", port).parse().unwrap();

    let builder = default_builder();

    setup(url, builder)
}

// BUILDER

fn default_builder() -> tauri::Builder<tauri::Wry> {
    tauri::Builder::default().plugin(tauri_plugin_shell::init())
}

// WINDOWS

fn build_window(app: &AppHandle, url: Url) {
    let mut window_builder = WindowBuilder::new(app, "main").title("Diffuse");

    window_builder = title_styles(window_builder);

    let window = window_builder.build().unwrap();

    let webview_builder = WebviewBuilder::new("main", WebviewUrl::External(url))
        .auto_resize()
        .enable_clipboard_access()
        .user_agent("Chrome");

    window
        .add_child(
            webview_builder,
            tauri::LogicalPosition::new(0, 0),
            window.inner_size().unwrap(),
        )
        .unwrap();

    window.maximize().unwrap();
    window.set_resizable(true).unwrap();
}

#[cfg(target_os = "macos")]
fn title_styles<R: Runtime, M: Manager<R>>(builder: WindowBuilder<R, M>) -> WindowBuilder<R, M> {
    return builder
        .title_bar_style(tauri::TitleBarStyle::Overlay)
        .hidden_title(true);
}

#[cfg(not(target_os = "macos"))]
fn title_styles(builder: WindowBuilder) -> WindowBuilder {
    return builder;
}

// SETUP

fn setup(url: Url, builder: tauri::Builder<tauri::Wry>) {
    builder
        .setup(move |app| {
            build_window(app.handle(), url);
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
