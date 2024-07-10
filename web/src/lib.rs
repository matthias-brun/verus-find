use leptos::*;
use leptos_dom::log;
use syn::spanned::Spanned;
use syn_verus as syn;
use verus_find_lib::matching;

pub fn token_to_view(tok: matching::fmt::FmtToken) -> impl IntoView {
    match tok {
        matching::fmt::FmtToken::Newline => Fragment::new(vec![view! { <br /> }.into_view()]),
        matching::fmt::FmtToken::Text(s, matching::fmt::HLToken(d)) => {
            let class = if d == 0 {
                "".to_string()
            } else {
                format!("hl_match{}", d % 3)
            };
            Fragment::new(vec![view! { <span class=class>{s}</span> }.into_view()])
        }
    }
}

#[component]
pub fn MatchesView(matches: ReadSignal<Vec<matching::Match>>) -> impl IntoView {
    view! {
        <div>
            <For
                each=move || matches.get().clone()
                key=|m| m.hash()
                children=move |m| {
                    view! {
                        <hr />
                        <div>
                            <div style="color:green; font-weight: bold;">
                            {m.format_location_line()}
                            <a href={format!("https://verus-lang.github.io/verus/verusdoc/src/{}.html#{}", m.file(), m.sig().span().start().line)} target="_blank">[source]</a>
                            </div>
                            <For
                                each=move || m.as_fmt_tokens()
                                key=|m| m.hash()
                                children=move |tok| token_to_view(tok)
                            ></For>
                        </div><br />
                    }
                }
            >
            </For>
        </div>
    }
}

#[component]
pub fn VerusFindComponent(files: Vec<(String, syn::File)>) -> impl IntoView {
    let (query, set_query) = create_signal(matching::Query::empty());
    let (matches, set_matches) = create_signal(vec![]);
    let (err_expr, set_err_expr) = create_signal("".to_string());
    let (err_sig, set_err_sig) = create_signal("".to_string());

    let form_submit = move |ev: leptos::ev::SubmitEvent| {
        //log!("Form submitted");
        ev.prevent_default();
        set_matches.set(get_matches(&files, &query.get()));
    };
    view! {
        <div>
            <form on:submit=form_submit>
                <span>Search expression:</span>
                <input type="text" class={move || if err_expr.get().is_empty() { "" } else { "err" }}
                    on:input=move |ev| {
                        let s = event_target_value(&ev);
                        if s.is_empty() {
                            set_query.set(query.get().set_reqens(None));
                            set_err_expr.set("".to_string());
                        } else {
                            match syn::parse_str(&event_target_value(&ev)) {
                                Ok(e) => {
                                    set_query.set(query.get().set_reqens(Some(e)));
                                    set_err_expr.set("".to_string());
                                },
                                Err(_) => set_err_expr.set("Invalid expr".to_string()),
                            }
                        }
                        //log!("{:?}", query.get());
                    }
                /><br />
                <span>Search signature:</span>
                <input type="text" class={move || if err_sig.get().is_empty() { "" } else { "err" }}
                    on:input=move |ev| {
                        let s = event_target_value(&ev);
                        if s.is_empty() {
                            set_query.set(query.get().set_sig(None));
                            set_err_sig.set("".to_string());
                        } else {
                            match syn::parse_str(&event_target_value(&ev)) {
                                Ok(e) => {
                                    set_query.set(query.get().set_sig(Some(e)));
                                    set_err_sig.set("".to_string());
                                },
                                Err(_) => set_err_sig.set("Invalid signature".to_string()),
                            }
                        }
                        //log!("{:?}", query.get());
                    }
                /><br />
                <button>"Search"</button>
            </form>
            <hr />
            <div>"Result: "{
                move || { if err_expr.get().is_empty() && err_sig.get().is_empty() {
                    view! { <span><b>{format!("{} matches found", matches.get().len())}</b></span> }
                } else {
                    view! { <span class="err_msg">{format!("{} {}", err_expr.get(), err_sig.get())}</span> }
                } }
                }<br />
                <MatchesView
                matches=matches
                />
            </div>
        </div>
    }
}

fn get_matches(files: &[(String, syn::File)], query: &matching::Query) -> Vec<matching::Match> {
    log!("Searching for {:?}", query);
    let matches = files
        .iter()
        .flat_map(|(path, file)| {
            matching::get_matches_file(file, query, path)
        })
    .collect();
    matches
}
