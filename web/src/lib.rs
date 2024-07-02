use leptos::*;
use leptos_dom::log;
use syn_verus as syn;
use verus_find_lib::matching;

#[component]
pub fn FmtTokenView(tok: matching::fmt::FmtToken) -> impl IntoView {
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
                        <div>
                            <div style="color:green; font-weight: bold;">
                            {m.format_location_line()}
                            </div>
                            <For
                                each=move || m.as_fmt_tokens()
                                key=|m| m.hash()
                                children=move |tok| view! {
                                    <FmtTokenView
                                        tok=tok
                                    />
                                }
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
    let (expr, set_expr) = create_signal("".to_string());
    let (sig, set_sig) = create_signal("".to_string());
    let (matches, set_matches) = create_signal(vec![]);

    view! {
        <div>
            <span>Search expression:</span>
            <input type="text"
                on:input=move |ev| {
                    log!("Expr: {}", &event_target_value(&ev));
                    set_expr.set(event_target_value(&ev));
                }
            />
            <span>Search signature:</span>
            <input type="text"
                on:input=move |ev| {
                    log!("Sig: {}", &event_target_value(&ev));
                    set_sig.set(event_target_value(&ev));
                }
            />
            <button on:click=move |_| set_matches.set(get_matches(&files, expr.get(), sig.get()))>"Search"</button>
            <div>"Results: "<br />
                <MatchesView
                matches=matches
                />
            </div>
        </div>
    }
}

fn get_matches(files: &[(String, syn::File)], expr: String, sig: String) -> Vec<matching::Match> {
    if expr.is_empty() && sig.is_empty() {
        vec![]
    } else {
        let parsed_expr: Option<syn::Expr> = syn::parse_str(&expr).ok();
        let parsed_sig: Option<syn::Signature> = syn::parse_str(&sig).ok();
        if parsed_expr.is_none() && parsed_sig.is_none() {
            log!("Expr and sig are none");
            vec![]
        } else {
            log!("Searching");
            let query = matching::Query::new(parsed_expr, None, None, None, parsed_sig);
            let matches = files
                .iter()
                .flat_map(|(path, file)| {
                    matching::other::get_matches_items(file.items.iter(), &query, path)
                })
                .collect();
            matches
        }
    }
}
