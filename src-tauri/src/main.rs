#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]


use tauri::{utils::config::AppUrl, WindowBuilder, WindowUrl};


// #[tauri::command]
// fn my_command(args: u64) -> Result<String, ()> {
//   println!("executed command with args {:?}", args);
//   Ok("executed".into())
// }


fn main() {
    let port = 44999;
    // let http = tauri_invoke_http::Invoke::new(["http://localhost:44998"]);
    let mut context = tauri::generate_context!("tauri.conf.json");

    let url = format!("http://localhost:{}", port).parse().unwrap();
    let window_url = WindowUrl::External(url);

    context.config_mut().build.dist_dir = AppUrl::Url(window_url.clone());
    context.config_mut().build.dev_path = AppUrl::Url(window_url.clone());

    tauri::Builder::default()
        // .invoke_system(http.initialization_script(), http.responder())
        // .invoke_handler(tauri::generate_handler![my_command])
        .plugin(tauri_plugin_localhost::Builder::new(port).build())
        .plugin(tauri_plugin_window_state::Builder::default().build())
        .setup(move |app| {
            // http.start(app.handle());

            WindowBuilder::new(app, "main", window_url)
                .title("Diffuse")
                .maximized(true)
                .resizable(true)
                .theme(None)
                .build()?;

            // Fin
            Ok(())
        })
        .run(context)
        .expect("Error while running tauri application");
}
