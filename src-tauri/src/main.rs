// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use tauri::{webview::Url, AppHandle, WebviewBuilder, WebviewUrl, WindowBuilder};
use tauri::{Manager, Runtime};
use tauri_plugin_positioner::{Position, WindowExt};
use tauri_plugin_window_state::{StateFlags, WindowExt as WindowStateExt};

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
    tauri::Builder::default()
        .plugin(tauri_plugin_fs::init())
        .plugin(tauri_plugin_shell::init())
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_window_state::Builder::default().build())
}

// WINDOWS

fn build_window(app: &AppHandle, url: Url) {
    let monitor = app.primary_monitor().unwrap();

    let height;
    let width;

    match monitor {
        Some(m) => {
            height = (m.size().height as f64 / m.scale_factor()) - 80.0;
            width = (m.size().width as f64 / m.scale_factor()) - 40.0;
        }

        None => {
            height = 675.0;
            width = 1080.0;
        }
    }

    let mut window_builder = WindowBuilder::new(app, "main")
        .title("Diffuse")
        .theme(None)
        .inner_size(width, height);

    window_builder = title_styles(window_builder);

    let window = window_builder.build().unwrap();
    window.move_window(Position::Center).unwrap();
    window.restore_state(StateFlags::all()).unwrap();

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

    window.set_resizable(true).unwrap();
}

#[cfg(target_os = "macos")]
fn title_styles<R: Runtime, M: Manager<R>>(builder: WindowBuilder<R, M>) -> WindowBuilder<R, M> {
    return builder
        .title_bar_style(tauri::TitleBarStyle::Overlay)
        .hidden_title(true);
}

#[cfg(not(target_os = "macos"))]
fn title_styles<R: Runtime, M: Manager<R>>(builder: WindowBuilder<R, M>) -> WindowBuilder<R, M> {
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
