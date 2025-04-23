use leptos::prelude::*;
use syn_verus as syn;
use verus_find_web::VerusFindComponent;
use chrono;

// This file is generated in build.rs and includes the full vstd files so we can search in them.
include!(concat!(env!("OUT_DIR"), "/vstd_files.rs"));

// Info about the build so we can include date and commit in the footer
include!(concat!(env!("OUT_DIR"), "/built.rs"));

pub fn main() {
    _ = console_log::init_with_level(log::Level::Debug);
    console_error_panic_hook::set_once();

    let mut files = FILES;
    files.sort();
    let files = files
        .iter()
        .map(|(path, file)| {
            (
                path.to_string(),
                syn::parse_file(file)
                    .map_err(|e| {
                        dbg!(&e.span().start(), &e.span().end());
                        format!("failed to parse file: {}", e)
                    })
                    .unwrap(),
            )
        })
        .collect::<Vec<_>>();

    let build_date = chrono::DateTime::parse_from_rfc2822(BUILT_TIME_UTC).unwrap().date_naive().to_string();

    mount_to_body(|| {
        view! {
            <h1>"Search in Verus' vstd"</h1>
            <a href="guide.html">"Guide"</a><br />
            <a href="https://github.com/matthias-brun/verus-find/issues">"Report an issue"</a><br /><br />
            <VerusFindComponent files=files />
            <hr />
            <a href="https://github.com/matthias-brun/verus-find">"verus-find"</a>
            " built on "{build_date}
            {format!(" (commit {})", GIT_COMMIT_HASH_SHORT.unwrap())}
        }
    })
}
