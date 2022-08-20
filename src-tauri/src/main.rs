#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use tauri::{utils::config::AppUrl, Runtime, Window, WindowBuilder, WindowUrl};


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
            let win = WindowBuilder::new(app, "main", window_url)
                .title("Diffuse")
                .maximized(true)
                .resizable(true)
                .theme(None)
                .build()?;

            win.set_transparent_titlebar(ToolbarThickness::Thin);
            set_user_agent(win);

            // Fin
            Ok(())
        })
        .run(context)
        .expect("Error while running tauri application");
}



// TRANSPARENT WINDOW


#[allow(dead_code)]
pub enum ToolbarThickness {
    Thick,
    Medium,
    Thin,
}

pub trait WindowExt {
    fn set_transparent_titlebar(&self, thickness: ToolbarThickness);
}

impl<R: Runtime> WindowExt for Window<R> {
    #[cfg(target_os = "macos")]
    fn set_transparent_titlebar(&self, thickness: ToolbarThickness) {
        use cocoa::appkit::{NSWindow, NSWindowTitleVisibility};

        unsafe {
            let id = self.ns_window().unwrap() as cocoa::base::id;

            id.setTitlebarAppearsTransparent_(cocoa::base::YES);

            match thickness {
                ToolbarThickness::Thick => {
                    self.set_title("").expect("Title wasn't set to ''");
                    make_toolbar(id);
                }
                ToolbarThickness::Medium => {
                    id.setTitleVisibility_(NSWindowTitleVisibility::NSWindowTitleHidden);
                    make_toolbar(id);
                }
                ToolbarThickness::Thin => {
                    id.setTitleVisibility_(NSWindowTitleVisibility::NSWindowTitleHidden);
                }
            }
        }
    }

    #[cfg(not(target_os = "macos"))]
    fn set_transparent_titlebar(&self, _thickness: ToolbarThickness) {}
}

#[cfg(target_os = "macos")]
unsafe fn make_toolbar(id: cocoa::base::id) {
    use cocoa::appkit::{NSToolbar, NSWindow};

    let new_toolbar = NSToolbar::alloc(id);
    new_toolbar.init_();
    id.setToolbar_(new_toolbar);
}



// USER AGENT


fn set_user_agent(window: Window) {
    let user_agent = "Chrome";

    window.with_webview(|webview| {
        #[cfg(windows)]
        unsafe {
            use webview2_com::Microsoft::Web::WebView2::Win32::ICoreWebView2Settings2;
            use windows::core::Interface;

            let settings: ICoreWebView2Settings2 = webview
                .controller()
                .CoreWebView2()
                .unwrap()
                .Settings()
                .unwrap();

            settings
                .SetUserAgent(user_agent)
                .unwrap();
        }

        #[cfg(target_os = "linux")]
        {
            use webkit2gtk::{WebViewExt, SettingsExt};
            let webview = webview.inner();
            let settings = webview.settings().unwrap();
            settings.set_user_agent(Some(user_agent));
        }

        // untested
        #[cfg(target_os = "macos")]
        unsafe {
            use objc::{msg_send, sel, sel_impl};
            use objc_foundation::{NSString, INSString};
            let agent = NSString::from_str(user_agent);
            let () = msg_send![webview.inner(), setCustomUserAgent: agent];
        }
    }).unwrap();
}