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
pub fn VerusFindComponent(files: &'static [(&'static str, &'static str)]) -> impl IntoView {
    let (expr, set_expr) = create_signal("".to_string());
    let (matches, set_matches) = create_signal(vec![]);

    let files = files.iter().map(|(path, file)|
        (path, syn::parse_file(file)
        .map_err(|e| {
            dbg!(&e.span().start(), &e.span().end());
            format!("failed to parse file: {}", e)
        }).unwrap())
    ).collect::<Vec<_>>();

    view! {
        <div>
            <span>Search expression:</span>
            <input type="text"
                on:input=move |ev| {
                    log!("{}", &event_target_value(&ev));
                    if event_target_value(&ev).is_empty() {
                        set_matches.set(vec![]);
                    } else {
                        let parsed_expr: syn::Expr = syn::parse_str(&event_target_value(&ev))
                                .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", expr.get()));
                        let query = matching::Query::new(Some(parsed_expr), None, None, None, None);
                        let matches = files.iter().flat_map(|(path, file)|
                            matching::other::get_matches_items(file.items.iter(), &query, path)
                        ).collect();
                        log!("{:?}", matches);
                        set_matches.set(matches);
                    }
                }
            />
            <div>"Results: "<br />
                <MatchesView
                matches=matches
                />
            </div>
        </div>
    }
}
